{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

-- |
-- Module             : Trace.Hpc.Markup.Summary
-- Description        : Generating the summary html files.
module Trace.Hpc.Markup.Summary
  ( ModuleSummary (..),
    name_summary,
    fun_summary,
    exp_summary,
    alt_summary,
  )
where

import Data.List (sortBy)
import Data.Semigroup as Semi
import qualified Data.Text as T
import qualified Lucid as L
import Prelude hiding (exp, last)

index_name :: String
index_name = "hpc_index.html"

index_fun :: String
index_fun = "hpc_index_fun.html"

index_alt :: String
index_alt = "hpc_index_alt.html"

index_exp :: String
index_exp = "hpc_index_exp.html"

-- | Legacy non-HTML5 attributes that are missing from Lucid
cellpadding_, cellspacing_, align_, border_ :: T.Text -> L.Attributes
cellpadding_ = L.makeAttributes "cellpadding"
cellspacing_ = L.makeAttributes "cellspacing"
align_ = L.makeAttributes "align"
border_ = L.makeAttributes "border"

-- | A string literal embedded as-is, without escaping
rawStr_ :: T.Text -> L.Html ()
rawStr_ = L.toHtmlRaw

data ModuleSummary = ModuleSummary
  { expTicked :: !Int,
    expTotal :: !Int,
    topFunTicked :: !Int,
    topFunTotal :: !Int,
    altTicked :: !Int,
    altTotal :: !Int
  }
  deriving (Show)

showModuleSummary :: (String, String, ModuleSummary) -> L.Html ()
showModuleSummary (modName, fileName, modSummary) = L.tr_ (link <> top <> alt <> exp)
  where
    link = L.td_ (rawStr_ "&nbsp;&nbsp;" <> L.code_ ("module" <> rawStr_ "&nbsp;" <> L.a_ [L.href_ (T.pack fileName)] (L.toHtml modName)))
    top = showSummary (topFunTicked modSummary) (topFunTotal modSummary)
    alt = showSummary (altTicked modSummary) (altTotal modSummary)
    exp = showSummary (expTicked modSummary) (expTotal modSummary)

showTotalSummary :: ModuleSummary -> L.Html ()
showTotalSummary modSummary = L.tr_ [L.style_ "background: #e0e0e0"] content
  where
    content = header <> top <> alt <> exp
    header = L.th_ [align_ "left"] (rawStr_ "&nbsp;&nbsp;" <> "Program Coverage Total")
    top = showSummary (topFunTicked modSummary) (topFunTotal modSummary)
    alt = showSummary (altTicked modSummary) (altTotal modSummary)
    exp = showSummary (expTicked modSummary) (expTotal modSummary)

showSummary :: Int -> Int -> L.Html ()
showSummary ticked total = percentHtml <> tickedTotal <> last
  where
    percentHtml :: L.Html ()
    percentHtml = L.td_ [align_ "right"] (showP (percent ticked total))

    showP :: Maybe Int -> L.Html ()
    showP Nothing = "-&nbsp;"
    showP (Just x) = L.toHtml (show x) <> "%"

    tickedTotal :: L.Html ()
    tickedTotal = L.td_ [] (L.toHtml (show ticked <> "/" <> show total))

    last :: L.Html ()
    last = L.td_ [L.width_ "100"] (case percent ticked total of Nothing -> "&nbsp;"; Just w -> bar w "bar")

    bar :: Int -> String -> L.Html ()
    bar 0 _ = bar 100 "invbar"
    bar w inner = L.table_ [cellpadding_ "0", cellspacing_ "0", L.width_ "100%", L.class_ "bar"] (L.tr_ (L.td_ innerTable))
      where
        innerTable :: L.Html ()
        innerTable = L.table_ [cellpadding_ "0", cellspacing_ "0", L.width_ (T.pack (show w <> "%"))] tableContent

        tableContent :: L.Html ()
        tableContent = L.tr_ (L.td_ [L.height_ "12", L.class_ (T.pack inner)] ("" :: L.Html ()))

percent :: (Integral a) => a -> a -> Maybe a
percent ticked total = if total == 0 then Nothing else Just (ticked * 100 `div` total)

instance Semi.Semigroup ModuleSummary where
  (ModuleSummary eTik1 eTot1 tTik1 tTot1 aTik1 aTot1) <> (ModuleSummary eTik2 eTot2 tTik2 tTot2 aTik2 aTot2) =
    ModuleSummary (eTik1 + eTik2) (eTot1 + eTot2) (tTik1 + tTik2) (tTot1 + tTot2) (aTik1 + aTik2) (aTot1 + aTot2)

instance Monoid ModuleSummary where
  mempty =
    ModuleSummary
      { expTicked = 0,
        expTotal = 0,
        topFunTicked = 0,
        topFunTotal = 0,
        altTicked = 0,
        altTotal = 0
      }
  mappend = (<>)

summaryHtml :: [(String, String, ModuleSummary)] -> L.Html ()
summaryHtml mods =
  L.html_ $ header <> body
  where
    header :: L.Html ()
    header =
      L.head_ $ mconcat
        [ L.meta_ [L.httpEquiv_ "Content-Type", L.content_ "text/html; charset=UTF-8"]
        , stylesheet
        ]

    stylesheet :: L.Html ()
    stylesheet = L.style_ [L.type_ "text/css"] (L.toHtml (T.unpack stylecontent))

    stylecontent :: T.Text
    stylecontent =
      "table.bar { background-color: #f25913; }\n"
        <> "td.bar { background-color: #60de51;  }\n"
        <> "td.invbar { background-color: #f25913;  }\n"
        <> "table.dashboard { border-collapse: collapse  ; border: solid 1px black }\n"
        <> ".dashboard td { border: solid 1px black }\n"
        <> ".dashboard th { border: solid 1px black }\n"

    body :: L.Html ()
    body =
      L.body_ $
        L.table_ [L.class_ "dashboard", L.width_ "100%", border_ "1"] $ mconcat
          [ L.tr_ $ mconcat
            [ L.th_ [L.rowspan_ "2"] $ L.a_ [L.href_ (T.pack index_name)] "module"
            , L.th_ [L.colspan_ "3"] $ L.a_ [L.href_ (T.pack index_fun)] "Top Level Definitions"
            , L.th_ [L.colspan_ "3"] $ L.a_ [L.href_ (T.pack index_alt)] "Alternatives"
            , L.th_ [L.colspan_ "3"] $ L.a_ [L.href_ (T.pack index_exp)] "Expressions"
            ]
          , L.tr_ $ mconcat
            [ L.th_ "%" <> L.th_ [L.colspan_ "2"] ("covered / total" :: L.Html ())
            , L.th_ "%" <> L.th_ [L.colspan_ "2"] ("covered / total" :: L.Html ())
            , L.th_ "%" <> L.th_ [L.colspan_ "2"] ("covered / total" :: L.Html ())
            ]
          , mconcat
            [ showModuleSummary (modName, fileName, modSummary)
              | (modName, fileName, modSummary) <- mods
            ]
          , L.tr_ ""
          , showTotalSummary
            ( mconcat
                [ modSummary
                  | (_, _, modSummary) <- mods
                ]
            )
          ]

-- | Compute "hpc_index.html"
name_summary :: [(String, String, ModuleSummary)] -> (FilePath, L.Html ())
name_summary mods = (index_name, summaryHtml (sortBy cmp mods))
  where
    cmp (n1, _, _) (n2, _, _) = compare n1 n2

-- | Compute "hpc_index_fun.html"
fun_summary :: [(String, String, ModuleSummary)] -> (FilePath, L.Html ())
fun_summary mods = (index_fun, summaryHtml (sortBy cmp mods))
  where
    cmp (_, _, s1) (_, _, s2) =
      compare
        (percent (topFunTicked s2) (topFunTotal s2))
        (percent (topFunTicked s1) (topFunTotal s1))

-- | Compute "hpc_index_alt.html"
alt_summary :: [(String, String, ModuleSummary)] -> (FilePath, L.Html ())
alt_summary mods = (index_alt, summaryHtml (sortBy cmp mods))
  where
    cmp (_, _, s1) (_, _, s2) =
      compare
        (percent (altTicked s2) (altTotal s2))
        (percent (altTicked s1) (altTotal s1))

-- | Compute "hpc_index_exp.html"
exp_summary :: [(String, String, ModuleSummary)] -> (FilePath, L.Html ())
exp_summary mods = (index_exp, summaryHtml (sortBy cmp mods))
  where
    cmp (_, _, s1) (_, _, s2) =
      compare
        (percent (expTicked s2) (expTotal s2))
        (percent (expTicked s1) (expTotal s1))
