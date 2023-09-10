{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
module Text.BlazeT.Html
    (
    module Text.BlazeT
    -- * Entities exported only by the @blazeT@ version of this module
    ,HtmlM
    ,HtmlT
    -- * Entities exported also by "Text.Blaze.Html"
    -- $descr1
    , Html
    , toHtml
    , preEscapedToHtml
    ) where

import Text.BlazeT

type HtmlT = MarkupT
type HtmlM a = MarkupM a
type Html = Markup

toHtml ::(ToMarkup a) => a -> Html
toHtml = toMarkup

preEscapedToHtml ::(ToMarkup a) => a -> Html
preEscapedToHtml = preEscapedToMarkup

-- $descr1 The following is an adaptation of all "Text.Blaze.Html"
-- exports to @blazeT@ types. For their documentation consult the
-- "Text.Blaze.Html" documentation.
