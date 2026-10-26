-- |
-- Module             : Trace.Hpc.Markup
-- Description        : The subcommand @hpc markup@
-- Copyright          : Andy Gill and Colin Runciman, 2006
-- License            : BSD-3-Clause
module Trace.Hpc.Markup (markupPlugin) where

import qualified Lucid as L
import qualified Data.Text.Lazy as T
import Control.Monad
import Data.Array
import Data.List (find, sortBy)
import Data.Maybe
import qualified Data.Set as Set
import System.FilePath
import Trace.Hpc.Flags
import Trace.Hpc.Markup.Summary
import Trace.Hpc.Mix
import Trace.Hpc.Plugin
import Trace.Hpc.Tix
import Trace.Hpc.Util
import Trace.Hpc.Utils

------------------------------------------------------------------------------

markupOptions :: FlagOptSeq
markupOptions =
  excludeOpt
    . includeOpt
    . srcDirOpt
    . hpcDirOpt
    . resetHpcDirsOpt
    . funTotalsOpt
    . altHighlightOpt
    . destDirOpt
    . verbosityOpt

markupPlugin :: Plugin
markupPlugin =
  Plugin
    { name = "markup",
      usage = "[OPTION] .. <TIX_FILE> [<MODULE> [<MODULE> ..]]",
      options = markupOptions,
      summary = "Markup Haskell source with program coverage",
      implementation = markupMain
    }

------------------------------------------------------------------------------

markupMain :: Flags -> [String] -> IO ()
markupMain flags (prog : modNames) = do
  let hpcflags1 = flags {includeMods = Set.fromList modNames `Set.union` includeMods flags}
  let Flags {funTotals = theFunTotals, altHighlight = invertOutput, destDir = dest_dir} = hpcflags1

  mtix <- readTix (getTixFileName prog)
  Tix tixs <- case mtix of
    Nothing -> hpcError markupPlugin $ "unable to find tix file for: " ++ prog
    Just a -> return a

  mods <-
    sequence
      [ genHtmlFromMod dest_dir hpcflags1 tix theFunTotals invertOutput
        | tix <- tixs,
          allowModule hpcflags1 (tixModuleName tix)
      ]

  -- Write "hpc_index.html"
  let (fp_name_index, html_name_index) = name_summary mods
  unless (verbosity flags < Normal) (putStrLn ("Writing: " <> fp_name_index))
  writeFileUtf8 (dest_dir </> fp_name_index) (T.unpack . L.renderText $ html_name_index)

  -- Write "hpc_index_fun.html"
  let (fp_fun_index, html_fun_index) = fun_summary mods
  unless (verbosity flags < Normal) (putStrLn ("Writing: " <> fp_fun_index))
  writeFileUtf8 (dest_dir </> fp_fun_index) (T.unpack . L.renderText $ html_fun_index)

  -- Write "hpc_index_alt.html"
  let (fp_alt_index, html_alt_index) = alt_summary mods
  unless (verbosity flags < Normal) (putStrLn ("Writing: " <> fp_alt_index))
  writeFileUtf8 (dest_dir </> fp_alt_index) (T.unpack . L.renderText $ html_alt_index)

  -- Write "hpc_index_exp.html"
  let (fp_exp_index, html_exp_index) = exp_summary mods
  unless (verbosity flags < Normal) (putStrLn ("Writing: " <> fp_exp_index))
  writeFileUtf8 (dest_dir </> fp_exp_index) (T.unpack . L.renderText $ html_exp_index)
markupMain _ [] =
  hpcError markupPlugin "no .tix file or executable name specified"

-- Add characters to the left of a string until it is at least as
-- large as requested.
padLeft :: Int -> Char -> String -> String
padLeft n c str = go n str
  where
    -- If the string is already long enough, stop traversing it.
    go 0 _ = str
    go k [] = replicate k c ++ str
    go k (_ : xs) = go (k - 1) xs

genHtmlFromMod :: String -> Flags -> TixModule -> Bool -> Bool -> IO (String, [Char], ModuleSummary)
genHtmlFromMod dest_dir flags tix theFunTotals invertOutput = do
  let theHsPath = srcDirs flags
  let modName0 = tixModuleName tix

  (Mix origFile _ _ tabStop mix') <- readMixWithFlags flags (Right tix)

  let arr_tix :: Array Int Integer
      arr_tix =
        listArray (0, length (tixModuleTixs tix) - 1) $
          tixModuleTixs tix

  let tickedWith :: Int -> Integer
      tickedWith n = arr_tix ! n

  let isTicked n = tickedWith n /= 0

  let info =
        [ (pos, theMarkup)
          | (gid, (pos, boxLabel)) <- zip [0 ..] mix',
            let binBox = case (isTicked gid, isTicked (gid + 1)) of
                  (False, False) -> []
                  (True, False) -> [TickedOnlyTrue]
                  (False, True) -> [TickedOnlyFalse]
                  (True, True) -> [],
            let tickBox =
                  if isTicked gid
                    then [IsTicked]
                    else [NotTicked],
            theMarkup <- case boxLabel of
              ExpBox {} -> tickBox
              TopLevelBox {} ->
                TopLevelDecl theFunTotals (tickedWith gid) : tickBox
              LocalBox {} -> tickBox
              BinBox _ True -> binBox
              _ -> []
        ]

  let modSummary =
        foldr
          ($)
          mempty
          ( [ \st ->
                case boxLabel of
                  ExpBox False ->
                    st
                      { expTicked = ticked (expTicked st),
                        expTotal = succ (expTotal st)
                      }
                  ExpBox True ->
                    st
                      { expTicked = ticked (expTicked st),
                        expTotal = succ (expTotal st),
                        altTicked = ticked (altTicked st),
                        altTotal = succ (altTotal st)
                      }
                  TopLevelBox _ ->
                    st
                      { topFunTicked = ticked (topFunTicked st),
                        topFunTotal = succ (topFunTotal st)
                      }
                  _ -> st
              | (gid, (_pos, boxLabel)) <- zip [0 ..] mix',
                let ticked =
                      if isTicked gid
                        then succ
                        else id
            ]
          )

  -- add prefix to modName argument
  content <- readFileFromPath (hpcError markupPlugin) origFile theHsPath

  let content' = markup tabStop info content
  let addLine n xs = "<span class=\"lineno\">" ++ padLeft 5 ' ' (show n) ++ " </span>" ++ xs
  let addLines = unlines . zipWith addLine [1 :: Int ..] . lines
  let fileName = modName0 <.> "hs" <.> "html"

  unless (verbosity flags < Normal) $ do
    putStrLn $ "Writing: " ++ fileName
  writeFileUtf8 (dest_dir </> fileName) $
    unlines
      [ "<html>",
        "<head>",
        "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
        "<style type=\"text/css\">",
        "span.lineno { color: white; background: #aaaaaa; border-right: solid white 12px }",
        if invertOutput
          then "span.nottickedoff { color: #404040; background: white; font-style: oblique }"
          else "span.nottickedoff { background: " ++ yellow ++ "}",
        if invertOutput
          then "span.istickedoff { color: black; background: #d0c0ff; font-style: normal; }"
          else "span.istickedoff { background: white }",
        "span.tickonlyfalse { margin: -1px; border: 1px solid " ++ red ++ "; background: " ++ red ++ " }",
        "span.tickonlytrue  { margin: -1px; border: 1px solid " ++ green ++ "; background: " ++ green ++ " }",
        "span.funcount { font-size: small; color: orange; z-index: 2; position: absolute; right: 20 }",
        if invertOutput
          then "span.decl { font-weight: bold; background: #d0c0ff }"
          else "span.decl { font-weight: bold }",
        "span.spaces    { background: white }",
        "</style>",
        "</head>",
        "<body>",
        "<pre>",
        concat
          [ "<span class=\"decl\">",
            "<span class=\"nottickedoff\">never executed</span> ",
            "<span class=\"tickonlytrue\">always true</span> ",
            "<span class=\"tickonlyfalse\">always false</span></span>"
          ],
        "</pre>",
        "<pre>"
      ]
      ++ addLines content'
      ++ "\n</pre>\n</body>\n</html>\n"

  modSummary `seq` return (modName0, fileName, modSummary)

data Loc = Loc !Int !Int
  deriving (Eq, Ord, Show)

data Markup
  = NotTicked
  | TickedOnlyTrue
  | TickedOnlyFalse
  | IsTicked
  | TopLevelDecl
      Bool -- display entry totals
      Integer
  deriving (Eq, Show)

markup ::
  -- | tabStop
  Int ->
  [(HpcPos, Markup)] -> -- random list of tick location pairs
  String -> -- text to mark up
  String
markup tabStop mix str = addMarkup tabStop str (Loc 1 1) [] sortedTickLocs
  where
    tickLocs =
      [ (Loc ln1 c1, Loc ln2 c2, mark)
        | (pos, mark) <- mix,
          let (ln1, c1, ln2, c2) = fromHpcPos pos
      ]
    sortedTickLocs =
      sortBy
        (\(locA1, locZ1, _) (locA2, locZ2, _) -> (locA1, locZ2) `compare` (locA2, locZ1))
        tickLocs

addMarkup ::
  Int -> -- tabStop
  String -> -- text to mark up
  Loc -> -- current location
  [(Loc, Markup)] -> -- stack of open ticks, with closing location
  [(Loc, Loc, Markup)] -> -- sorted list of tick location pairs
  String
-- check the pre-condition.
-- addMarkup tabStop cs loc os ticks
--   | not (isSorted (map fst os)) = error $ "addMarkup: bad closing ordering: " ++ show os
-- addMarkup tabStop cs loc os@(_:_) ticks
--   | trace (show (loc,os,take 10 ticks)) False = undefined
-- close all open ticks, if we have reached the end
addMarkup _ [] _loc os [] =
  concatMap (const closeTick) os
addMarkup tabStop cs loc ((o, _) : os) ticks
  | loc > o =
      closeTick ++ addMarkup tabStop cs loc os ticks
-- addMarkup tabStop cs loc os ((t1,t2,tik@(TopLevelDecl {})):ticks) | loc == t1 =
--   openTick tik ++ closeTick ++ addMarkup tabStop cs loc os ticks
addMarkup tabStop cs loc os ((t1, t2, tik0) : ticks) | loc == t1 =
  case os of
    ((_, tik') : _)
      | not (allowNesting tik0 tik') ->
          addMarkup tabStop cs loc os ticks -- already marked or bool within marked bool
    _ -> openTick tik0 ++ addMarkup tabStop cs loc (addTo (t2, tik0) os) ticks
  where
    addTo (t, tik) [] = [(t, tik)]
    addTo (t, tik) ((t', tik') : xs)
      | t <= t' = (t, tik) : (t', tik') : xs
      | otherwise = (t', tik) : (t', tik') : xs
addMarkup tabStop0 cs loc os ((t1, _t2, _tik) : ticks)
  | loc > t1 =
      -- throw away this tick, because it is from a previous place ??
      addMarkup tabStop0 cs loc os ticks
addMarkup tabStop0 ('\n' : cs) loc@(Loc ln col) os@((Loc ln2 col2, _) : _) ticks
  | ln == ln2 && col < col2 =
      addMarkup tabStop0 (' ' : '\n' : cs) loc os ticks
addMarkup tabStop0 (c0 : cs) loc@(Loc _ p) os ticks
  | c0 == '\n' && os /= [] =
      concatMap (const closeTick) (downToTopLevel os)
        ++ c0
        : "<span class=\"spaces\">"
        ++ expand 1 w
        ++ "</span>"
        ++ concatMap (openTick . snd) (reverse (downToTopLevel os))
        ++ addMarkup tabStop0 cs' loc' os ticks
  | c0 == '\t' =
      expand p "\t" ++ addMarkup tabStop0 cs (incBy c0 loc) os ticks
  | otherwise =
      escape c0 ++ addMarkup tabStop0 cs (incBy c0 loc) os ticks
  where
    (w, cs') = span (`elem` " \t") cs
    loc' = foldl (flip incBy) loc (c0 : w)
    escape '>' = "&gt;"
    escape '<' = "&lt;"
    escape '"' = "&quot;"
    escape '&' = "&amp;"
    escape c = [c]
    expand :: Int -> String -> String
    expand _ "" = ""
    expand c ('\t' : s) =
      replicate (c' - c) ' ' ++ expand c' s
      where
        c' = tabStopAfter 8 c
    expand c (' ' : s) = ' ' : expand (c + 1) s
    expand _ _ = error "bad character in string for expansion"
    incBy :: Char -> Loc -> Loc
    incBy '\n' (Loc ln _c) = Loc (succ ln) 1
    incBy '\t' (Loc ln c) = Loc ln (tabStopAfter tabStop0 c)
    incBy _ (Loc ln c) = Loc ln (succ c)
    tabStopAfter :: Int -> Int -> Int
    tabStopAfter tabStop c =
      fromJust (find (> c) [1, (tabStop + 1) ..])
addMarkup tabStop cs loc os ticks =
  "ERROR: " ++ show (take 10 cs, tabStop, loc, take 10 os, take 10 ticks)

openTick :: Markup -> String
openTick NotTicked = "<span class=\"nottickedoff\">"
openTick IsTicked = "<span class=\"istickedoff\">"
openTick TickedOnlyTrue = "<span class=\"tickonlytrue\">"
openTick TickedOnlyFalse = "<span class=\"tickonlyfalse\">"
openTick (TopLevelDecl False _) = openTopDecl
openTick (TopLevelDecl True 0) =
  "<span class=\"funcount\">-- never entered</span>"
    ++ openTopDecl
openTick (TopLevelDecl True 1) =
  "<span class=\"funcount\">-- entered once</span>"
    ++ openTopDecl
openTick (TopLevelDecl True n0) =
  "<span class=\"funcount\">-- entered " ++ showBigNum n0 ++ " times</span>" ++ openTopDecl
  where
    showBigNum n
      | n <= 9999 = show n
      | otherwise = case n `quotRem` 1000 of
          (q, r) -> showBigNum' q ++ "," ++ showWith r
    showBigNum' n
      | n <= 999 = show n
      | otherwise = case n `quotRem` 1000 of
          (q, r) -> showBigNum' q ++ "," ++ showWith r
    showWith n = padLeft 3 '0' $ show n

closeTick :: String
closeTick = "</span>"

openTopDecl :: String
openTopDecl = "<span class=\"decl\">"

downToTopLevel :: [(Loc, Markup)] -> [(Loc, Markup)]
downToTopLevel ((_, TopLevelDecl {}) : _) = []
downToTopLevel (o : os) = o : downToTopLevel os
downToTopLevel [] = []

-- build in logic for nesting bin boxes

allowNesting ::
  Markup -> -- innermost
  Markup -> -- outermost
  Bool
allowNesting n m | n == m = False -- no need to double nest
allowNesting IsTicked TickedOnlyFalse = False
allowNesting IsTicked TickedOnlyTrue = False
allowNesting _ _ = True

------------------------------------------------------------------------------
-- global color palette

red, green, yellow :: String
red = "#f20913"
green = "#60de51"
yellow = "yellow"
