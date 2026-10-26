module Markup.Tests (markupTests) where

import System.Process
import Control.Monad (void, forM_)
import qualified System.FilePath as FP
import Test.Tasty (TestTree, testGroup)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Test.Tasty.Golden (goldenVsString, goldenVsFile)
import Utils (runCommands)
import Data.List (intercalate)

inputBaseDir :: FilePath
inputBaseDir = FP.joinPath ["test", "Markup", "input"]

goldBaseDir :: FilePath
goldBaseDir = FP.joinPath ["test", "Markup", "gold"]

-- | Tests of the "hpc markup" subcommand
markupTests :: TestTree
markupTests = testGroup "markup" [helpTextTest, recipTest, recipTestMarkup]

helpTextTest :: TestTree
helpTextTest = goldenVsString "Help" (goldBaseDir FP.</> "Help.stdout") go
  where
    go :: IO BS.ByteString
    go = runCommands "." ["hpc help markup"]

-- | Remove all generated *.html files in the Recip subdirectory
rmHtml :: IO ()
rmHtml = void $ readCreateProcess ((shell "rm -r *.html") { cwd = Just(inputBaseDir FP.</> "Recip") }) ""


rm :: [FilePath] -> IO ()
rm files = void $ readCreateProcess ((shell (intercalate " && " . map ("rm -r " <>) $ files)) { cwd = Just (inputBaseDir FP.</> "Recip")}) ""

recipTest :: TestTree
recipTest = goldenVsString "RecipStdout" (goldBaseDir FP.</> "Recip.stdout") go
  where
    go :: IO BS.ByteString
    go = runCommands (inputBaseDir FP.</> "Recip") ["hpc markup Recip.tix"] <* rmHtml

recipTestMarkup :: TestTree
recipTestMarkup =
  testGroup "Recip" $ (flip map) filenames $ \fname ->
    goldenVsFile fname (recipTestGoldDir FP.</> fname)
                       (inputBaseDir FP.</> "Recip" FP.</> fname) go
  where
    filenames = ["hpc_index_alt.html", "hpc_index_fun.html", "hpc_index_exp.html", "Main.hs.html"]

    recipTestGoldDir :: FilePath
    recipTestGoldDir = goldBaseDir FP.</> "Recip"

    go :: IO ()
    go = do
        _ <- runCommands (inputBaseDir FP.</> "Recip") ["hpc markup Recip.tix"]
        pure ()
