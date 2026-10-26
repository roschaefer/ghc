#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <unistd.h>

/* Some platforms may not have SOCK_CLOEXEC available as an option to socket(),
 * so we need to set it explicitly with fcntl()
 * */
static int set_cloexec(int fd) {
    int flags = fcntl(fd, F_GETFD, 0);
    if (flags == -1) return -1;
    return fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
}

int hs_listen_domain_socket(const char *path) {
    if (strlen(path) >= sizeof(((struct sockaddr_un *)0)->sun_path)) {
        errno = ENAMETOOLONG;
        return -1;
    }

    int listen_type = SOCK_STREAM;
#ifdef SOCK_NONBLOCK
    listen_type |= SOCK_NONBLOCK;
#endif
#ifdef SOCK_CLOEXEC
    listen_type |= SOCK_CLOEXEC;
#endif
    int fd = socket(AF_UNIX, listen_type, 0);
    if (fd == -1) {
        return -1;
    }
#ifndef SOCK_NONBLOCK
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        int saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }
#endif
#ifndef SOCK_CLOEXEC
    if (set_cloexec(fd) == -1) {
        int saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }
#endif

    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(struct sockaddr_un));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, path, sizeof(server_addr.sun_path) - 1);

    if (bind(fd, (struct sockaddr *) &server_addr, sizeof(struct sockaddr_un)) == -1) {
        int saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }

    if (listen(fd, SOMAXCONN) == -1) {
        int saved_errno = errno;
        close(fd);
        /* bind(2) succeeded so the socket file exists on disk; unlink it. */
        unlink(path);
        errno = saved_errno;
        return -1;
    }

    return fd;
}

int hs_connect_domain_socket(const char *path) {
    if (strlen(path) >= sizeof(((struct sockaddr_un *)0)->sun_path)) {
        errno = ENAMETOOLONG;
        return -1;
    }

#ifdef SOCK_CLOEXEC
    int fd = socket(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0);
#else
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
#endif
    if (fd == -1) {
        return -1;
    }
#ifndef SOCK_CLOEXEC
    if (set_cloexec(fd) == -1) {
        int saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }
#endif

    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(struct sockaddr_un));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, path, sizeof(server_addr.sun_path) - 1);

    if (connect(fd, (struct sockaddr *) &server_addr, sizeof(struct sockaddr_un)) == -1) {
        if (errno != EINTR) {
            int saved_errno = errno;
            close(fd);
            errno = saved_errno;
            return -1;
        }
        /* See http://www.madore.org/~david/computers/connect-intr.html */
        struct pollfd unix_really_sucks;
        int some_more_junk;
        socklen_t yet_more_useless_junk;
        unix_really_sucks.fd = fd;
        unix_really_sucks.events = POLLOUT;
        while (poll(&unix_really_sucks, 1, -1) == -1) {
            if (errno != EINTR) {
                int saved_errno = errno;
                close(fd);
                errno = saved_errno;
                return -1;
            }
        }
        yet_more_useless_junk = sizeof(some_more_junk);
        if (getsockopt(fd, SOL_SOCKET, SO_ERROR,
                       &some_more_junk, &yet_more_useless_junk) == -1) {
            int saved_errno = errno;
            close(fd);
            errno = saved_errno;
            return -1;
        }
        if (some_more_junk != 0) {
            close(fd);
            errno = some_more_junk;
            return -1;
        }
    }
    /* At this point, fd is connected. */

    return fd;
}

/*
 * Cooperative interruptible accept.
 *
 * Blocks in poll(2) until either a connection arrives on listen_fd or
 * cancel_fd becomes readable. Spins until cancel_fd is written to,
 * or reading from listen_fd fails for some reason. 
 *
 * This avoids interruptible FFI (which is subject to GHC #27110 and #27113).
 *
 * Returns:
 *   >= 0  accepted fd
 *   -1    error (with errno)
 *   -2    cancelled (cancel_fd was signalled or closed)
 *
 */
int hs_poll_accept(int listen_fd, int cancel_fd) {
    struct pollfd fds[2];
    fds[0].fd = listen_fd;
    fds[0].events = POLLIN;
    fds[1].fd = cancel_fd;
    fds[1].events = POLLIN;

    for (;;) {
        int r = poll(fds, 2, -1);
        if (r == -1) {
            if (errno == EINTR) continue;
            return -1;
        }
        /* Check cancel fd first */
        if (fds[1].revents & (POLLIN | POLLHUP | POLLERR))
            return -2;
        /* Listen fd error (e.g. closed/invalid): report as error, don't spin */
        if (fds[0].revents & (POLLERR | POLLHUP | POLLNVAL)) {
            errno = EBADF;
            return -1;
        }
        if (fds[0].revents & POLLIN) {
            int afd = accept(listen_fd, NULL, NULL);
            if (afd >= 0) {
                int fl = fcntl(afd, F_GETFL, 0);
                if (fl != -1)
                    fcntl(afd, F_SETFL, fl & ~O_NONBLOCK);
                set_cloexec(afd);
                return afd;
            }
            /* Listen fd is O_NONBLOCK. an EAGAIN/EINTR here means the queued
             * connection was droppped poll and accept. */
            if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK)
                continue;
            return -1;
        }
        /* spurious wakeup — retry */
    }
}
