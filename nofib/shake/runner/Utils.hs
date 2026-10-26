{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Used for instance (MatchPatCount a) => MultiFileRule a where
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
module Utils where

import Control.Monad
import System.FilePath as IO
import Development.Shake

import Data.Maybe

import Prelude hiding (lines)

import RunnerTypes
import GHC.Stack (HasCallStack)
import GHC.Natural
import GHC.TypeLits as TypeLits
import Data.Tuple
-------------------------------------------------------------------------------
-- Finding source files for a given .o file
-------------------------------------------------------------------------------

runWithVerbosity :: Monad m => Nofib -> Int -> m () -> m ()
runWithVerbosity Build { verbosity = conf_verbosity} v log_action = do
    when (conf_verbosity > v) log_action

runVerbose :: Monad m => Nofib -> m () -> m ()
runVerbose n act = runWithVerbosity n 2 act


-- | dropPathPrefix "foo" "foo/bar" == "bar"
dropPathPrefix :: [Char] -> [Char] -> FilePath
dropPathPrefix prefix path =
    let path_segments        = splitPath path
        prefix_segments = splitPath prefix
        filter_segments (pre:pres) (seg:segs)
            | equalFilePath pre seg
            = filter_segments pres segs
        filter_segments _ segs = segs
    in  joinPath $ filter_segments prefix_segments path_segments

-- | Try to find the source in the .depends file
--
-- E.g. gc/linear/Matrix.o => Just gc/linear/Matrix.lhs
-- We already filtered out any lines *not* specifying a dep
-- for the current file. So if there are deps the first line will
-- give the source file.
getHaskellSrcFromDeps :: String -> [String] -> Maybe String
getHaskellSrcFromDeps _    (x:_)
    | (_ : ":" : rhs : _) <- words x
    = Just rhs
getHaskellSrcFromDeps _    _ = Nothing

-- -- There is always one Main.o file for which the source file is recorded
-- -- in the .config file
-- getMainSrc :: String -> String -> (String -> String) -> Maybe String
-- getMainSrc "Main.o" dir config = Just $! (dir </> (config "MAIN"))
-- getMainSrc _ _      _        = Nothing

-- | Generic case, for foo.o look for foo.c
--
-- Haskell source files should be recorded in the .depends file
-- so only C files should remain.
-- No maybe as a source file is required. We just
-- fail if we can't find it.
getCSrc :: String -> String -> String
getCSrc o_file dir = dir </> replaceExtension o_file ".c"

-------------------------------------------------------------------------------
-- Deriving the name of the .o file a given source file
-------------------------------------------------------------------------------

-- | Turn Foo.[hs|lhs|c] into Foo.o
oFromSrc :: FilePath -> Action FilePath
oFromSrc src = do
    let (file,ext) = splitExtension src
    unless (ext `elem` [".hs", ".lhs", ".c"]) $
        putWarn $ "Unknown source type " ++ ext
    return $ replaceExtension file "o"

--------------------------------------------------------
-- Helper to deal with incomplete matched caused by &%>
--------------------------------------------------------

type family TupleSize a :: Natural where
    TupleSize (Solo _) = 1
    TupleSize (_,_) = 2
    TupleSize (_,_,_) = 3
    TupleSize (_,_,_,_) = 4
    TupleSize _ = TypeError (TypeLits.Text "Add more equations to HomoTupleN to support this.")

class MatchPatCount a where
    matchPatCount :: (a -> c) -> [FilePath] -> c

-- Which introduce this wrapper? It allows us to give multifile rules without incomplete patterns
class (MatchPatCount a) => MultiFileRule a where
    multiFileRule :: (KnownNat (n :: Natural), (n :: Natural) ~ (TupleSize a), HasCallStack) => [FilePattern] -> (a -> Action()) -> Rules ()
    multiFileRule pat f =
        pat &%> matchPatCount f

instance (MatchPatCount a) => MultiFileRule a where

instance MatchPatCount (FilePath,FilePath) where
    matchPatCount f [x1,x2] = f (x1,x2)
    matchPatCount _ _  = error "Missmatched"

instance MatchPatCount (FilePath,FilePath,FilePath) where
    matchPatCount f [x1,x2,x3]= f (x1,x2,x3)
    matchPatCount _ _  = error "Missmatched"

instance MatchPatCount (FilePath,FilePath,FilePath,FilePath) where
    matchPatCount f [x1,x2,x3,x4]= f (x1,x2,x3,x4)
    matchPatCount _ _  = error "Missmatched"

----------------------------------------------------
-- Misc stuff

-- | Split a list into sublists delimited by the given element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)