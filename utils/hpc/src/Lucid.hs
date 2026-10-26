{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | At the moment `lucid2` is not a GHC boot package,
-- so this module provides a poor man substitute
-- tailored just enough to suffice for HPC purposes.

module Lucid
  ( Html
  , toHtmlRaw
  , toHtml
  , renderText
  , Attributes
  , makeAttributes
  , a_
  , body_
  , class_
  , code_
  , colspan_
  , content_
  , head_
  , height_
  , href_
  , html_
  , httpEquiv_
  , meta_
  , rowspan_
  , style_
  , table_
  , td_
  , th_
  , tr_
  , type_
  , width_
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.List (intersperse)
import Data.Char (isSpace, isControl)

type Html a = B.Builder

toHtmlRaw :: T.Text -> B.Builder
toHtmlRaw = B.fromText

toHtml :: String -> B.Builder
toHtml = foldMap $ \case
  '>' -> "&gt;"
  '<' -> "&lt;"
  '&' -> "&amp;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  '\DEL' -> ""
  c
    | isControl c && not (isSpace c) -> ""
    | otherwise -> B.singleton c

renderText :: B.Builder -> TL.Text
renderText = B.toLazyText

newtype Attributes = Attributes { unAttributes :: B.Builder }

class Term arg result | result -> arg where
  term :: T.Text -> arg -> result

instance Term B.Builder B.Builder where
  term = makeTag

instance Term [Attributes] (B.Builder -> B.Builder) where
  term = makeTagWithAttr

instance Term T.Text Attributes where
  term = makeAttributes

makeAttributes :: T.Text -> T.Text -> Attributes
makeAttributes attr cnt = Attributes $ B.fromText attr <> "=\"" <> toHtml (T.unpack cnt) <> "\""

makeTag :: T.Text -> B.Builder -> B.Builder
makeTag tag cnt = "<" <> B.fromText tag <> ">" <> cnt <> "</" <> B.fromText tag <> ">"

makeTagWithAttr :: T.Text -> [Attributes] -> B.Builder -> B.Builder
makeTagWithAttr tag attrs cnt =
  "<" <> mconcat (intersperse " " (B.fromText tag : map unAttributes attrs)) <> ">" <> cnt <> "</" <> B.fromText tag <> ">"

makeTagWithoutContent :: T.Text -> [Attributes] -> B.Builder
makeTagWithoutContent tag attrs =
  "<" <> mconcat (intersperse " " (B.fromText tag : map unAttributes attrs)) <> ">"

colspan_, rowspan_, width_, height_, class_, type_, href_, content_, httpEquiv_ :: T.Text -> Attributes
colspan_ = makeAttributes "colspan"
rowspan_ = makeAttributes "rowspan"
width_ = makeAttributes "width"
height_ = makeAttributes "height"
class_ = makeAttributes "class"
type_ = makeAttributes "type"
href_ = makeAttributes "href"
content_ = makeAttributes "content"
httpEquiv_ = makeAttributes "http-equiv"

html_, head_, body_, code_ :: B.Builder -> B.Builder
html_ = makeTag "html"
head_ = makeTag "head"
body_ = makeTag "body"
code_ = makeTag "code"

a_, table_ :: [Attributes] -> B.Builder -> B.Builder
a_ = makeTagWithAttr "a"
table_ = makeTagWithAttr "table"

meta_ :: [Attributes] -> B.Builder
meta_ = makeTagWithoutContent "meta"

th_, tr_, td_, style_ :: Term arg result => arg -> result
th_ = term "th"
tr_ = term "tr"
td_ = term "td"
style_ = term "style"
