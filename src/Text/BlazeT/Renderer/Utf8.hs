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

renderMarkupBuilder :: MarkupI a -> B.Builder
renderMarkupBuilder = BU.renderMarkupBuilder . execMarkup

renderHtmlBuilder :: MarkupI a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderMarkup :: MarkupI a -> BL.ByteString
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupI a -> BL.ByteString
renderHtml = renderMarkup

renderMarkupToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupI a -> IO ()
renderMarkupToByteStringIO g = BU.renderMarkupToByteStringIO g . execMarkup

renderHtmlToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupI a -> IO ()
renderHtmlToByteStringIO = renderMarkupToByteStringIO
