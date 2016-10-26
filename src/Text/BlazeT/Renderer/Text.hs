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

renderMarkupBuilder :: MarkupM a -> B.Builder
renderMarkupBuilder = BU.renderMarkupBuilder . execMarkup

renderHtmlBuilder :: MarkupM a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderMarkup :: MarkupM a -> L.Text
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupM a -> L.Text
renderHtml = renderMarkup

renderMarkupWith :: (ByteString -> Text) -> MarkupM a -> L.Text
renderMarkupWith g = (BH.renderHtmlWith g) . execMarkup

renderHtmlWith :: (ByteString -> Text) -> MarkupM a -> L.Text
renderHtmlWith = renderMarkupWith

renderMarkupBuilderWith :: (ByteString -> Text) -> MarkupM a -> B.Builder
renderMarkupBuilderWith g = (BU.renderMarkupBuilderWith g) . execMarkup

renderHtmlBuilderWith :: (ByteString -> Text) -> MarkupM a -> B.Builder
renderHtmlBuilderWith = renderHtmlBuilderWith

