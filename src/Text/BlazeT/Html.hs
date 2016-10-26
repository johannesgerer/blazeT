{-# LANGUAGE RankNTypes #-}
module Text.BlazeT.Html
    ( module Text.BlazeT
    , Html
    , toHtml
    , preEscapedToHtml
    -- * BlazeT new stuff
    ,HtmlM
    ,HtmlT
    ) where

import Text.BlazeT

type HtmlT = MarkupT
type HtmlM = MarkupM
type Html = Markup

toHtml ::(ToMarkup a) => a -> Html
toHtml = toMarkup

preEscapedToHtml ::(ToMarkup a) => a -> Html
preEscapedToHtml = preEscapedToMarkup
