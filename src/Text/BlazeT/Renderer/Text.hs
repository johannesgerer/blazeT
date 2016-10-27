{-# LANGUAGE RankNTypes #-}
module Text.BlazeT.Renderer.Text
    ( renderMarkupBuilder
    , renderMarkupBuilderWith
    , renderMarkup
    , renderMarkupWith
    , renderHtmlBuilder
    , renderHtmlBuilderWith
    , renderHtml
    , renderHtmlWith
  ) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Text.Blaze.Renderer.Text as BU
import           Text.BlazeT

renderMarkupBuilder :: MarkupI a -> B.Builder
renderMarkupBuilder = BU.renderMarkupBuilder . execMarkup

renderHtmlBuilder :: MarkupI a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderMarkup :: MarkupI a -> L.Text
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupI a -> L.Text
renderHtml = renderMarkup

renderMarkupWith :: (ByteString -> Text) -> MarkupI a -> L.Text
renderMarkupWith g = (BH.renderHtmlWith g) . execMarkup

renderHtmlWith :: (ByteString -> Text) -> MarkupI a -> L.Text
renderHtmlWith = renderMarkupWith

renderMarkupBuilderWith :: (ByteString -> Text) -> MarkupI a -> B.Builder
renderMarkupBuilderWith g = (BU.renderMarkupBuilderWith g) . execMarkup

renderHtmlBuilderWith :: (ByteString -> Text) -> MarkupI a -> B.Builder
renderHtmlBuilderWith = renderHtmlBuilderWith

