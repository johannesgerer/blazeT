{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=230 #-}

-- the above option was not needed with
  --   ,blaze-html >= 0.6.0.0 && < 0.7.0.0
  --   ,blaze-builder >= 0.2
  --   ,text  < 1.2

module Text.BlazeT.Renderer.Utf8
    (
    renderMarkupBuilder
    , renderMarkup
    , renderMarkupToByteStringIO
    , renderHtmlBuilder
    , renderHtml
    , renderHtmlToByteStringIO
  ) where

import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Text.Blaze.Renderer.Utf8 as BU
import           Text.BlazeT

renderMarkupBuilder :: MarkupM a -> B.Builder
renderMarkupBuilder x = BU.renderMarkupBuilder $ execMarkup x

renderHtmlBuilder :: MarkupM a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderMarkup :: MarkupM a -> BL.ByteString
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupM a -> BL.ByteString
renderHtml = renderMarkup

renderMarkupToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupM a -> IO ()
renderMarkupToByteStringIO g = BU.renderMarkupToByteStringIO g . execMarkup

renderHtmlToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupM a -> IO ()
renderHtmlToByteStringIO = renderMarkupToByteStringIO
