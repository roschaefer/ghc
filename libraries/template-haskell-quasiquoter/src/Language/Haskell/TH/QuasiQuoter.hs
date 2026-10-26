{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module Language.Haskell.TH.QuasiQuoter
  ( QuasiQuoter (QuasiQuoter, quoteExp, quotePat, quoteType, quoteDec)
  , namedDefaultQuasiQuoter
  , defaultQuasiQuoter
  , Q
  , Exp
  , Pat
  , Type
  , Dec
  ) where

#if __GLASGOW_HASKELL__ >= 915
import GHC.Internal.TH.Monad (QuasiQuoter(..), Q)
import GHC.Internal.TH.Syntax (Exp, Pat, Type, Dec)
#elif __GLASGOW_HASKELL__ >= 912
import GHC.Internal.TH.Quote (QuasiQuoter(..))
import GHC.Internal.TH.Syntax
#else
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp, Q, Pat, Type, Dec)
#endif

-- | A t'QuasiQuoter' that fails with a helpful error message in every
-- context. It is intended to be modified to create a t'QuasiQuoter' that
-- fails in all inappropriate contexts.
--
-- For example, you could write
--
-- @
-- myPatQQ = (namedDefaultQuasiQuoter "myPatQQ")
--   { quotePat = ... }
-- @
--
-- If @myPatQQ@ is used in an expression context, the compiler will report
-- that, naming @myPatQQ@.
--
-- See also 'defaultQuasiQuoter', which does not name the t'QuasiQuoter' in
-- the error message, and might therefore be more appropriate when
-- the users of a particular t'QuasiQuoter' tend to define local \"synonyms\"
-- for it.
namedDefaultQuasiQuoter :: String -> QuasiQuoter
namedDefaultQuasiQuoter name = QuasiQuoter
  { quoteExp = f "use in expression contexts."
  , quotePat = f "use in pattern contexts."
  , quoteType = f "use in types."
  , quoteDec = f "creating declarations."
  }
  where
    f m _ = fail $ "The " ++ name ++ " quasiquoter is not for " ++ m

-- | A t'QuasiQuoter' that fails with a helpful error message in every
-- context. It is intended to be modified to create a t'QuasiQuoter' that
-- fails in all inappropriate contexts.
--
-- For example, you could write
--
-- @
-- myExpressionQQ = defaultQuasiQuoter
--   { quoteExp = ... }
-- @
--
-- See also 'namedDefaultQuasiQuoter', which names the t'QuasiQuoter' in the
-- error messages.
defaultQuasiQuoter :: QuasiQuoter
defaultQuasiQuoter = QuasiQuoter
  { quoteExp = f "use in expression contexts."
  , quotePat = f "use in pattern contexts."
  , quoteType = f "use in types."
  , quoteDec = f "creating declarations."
  }
  where
    f m _ = fail $ "This quasiquoter is not for " ++ m
