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

    -- * new BlazeT stuff
    , renderMarkupBuilderT
    , renderMarkupT
    , renderMarkupToByteStringIOT
    , renderHtmlToByteStringIOT
    , renderHtmlBuilderT
    , renderHtmlT
  ) where

import qualified Blaze.ByteString.Builder as B
import           Control.Monad
import           Control.Monad.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Text.Blaze.Renderer.Utf8 as BU
import           Text.BlazeT

renderMarkupBuilder :: MarkupM a -> B.Builder
renderMarkupBuilder = runIdentity . renderMarkupBuilderT

renderMarkupBuilderT :: Monad m => MarkupT m a -> m B.Builder
renderMarkupBuilderT = liftM BU.renderMarkupBuilder . execMarkupT

renderHtmlBuilder :: MarkupM a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderHtmlBuilderT :: Monad m => MarkupT m a -> m B.Builder
renderHtmlBuilderT = renderMarkupBuilderT

renderMarkup :: MarkupM a -> BL.ByteString
renderMarkup = runIdentity . renderMarkupT
renderMarkupT :: Monad m => MarkupT m a -> m BL.ByteString
renderMarkupT = liftM BU.renderMarkup . execMarkupT

renderHtml :: MarkupM a -> BL.ByteString
renderHtml = renderMarkup
renderHtmlT :: Monad m => MarkupT m a -> m BL.ByteString
renderHtmlT = renderMarkupT

renderMarkupToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupM a -> IO ()
renderMarkupToByteStringIO g = runIdentity . renderMarkupToByteStringIOT g 
renderMarkupToByteStringIOT :: Monad m => (BS.ByteString -> IO ()) ->
                               MarkupT m a -> m (IO ())
renderMarkupToByteStringIOT g = liftM (BU.renderMarkupToByteStringIO g) . execMarkupT

renderHtmlToByteStringIO :: (BS.ByteString -> IO ()) -> MarkupM a -> IO ()
renderHtmlToByteStringIO g = runIdentity . renderMarkupToByteStringIOT g 
renderHtmlToByteStringIOT :: Monad m => (BS.ByteString -> IO ()) ->
                             MarkupT m a -> m (IO ())
renderHtmlToByteStringIOT g = liftM (BU.renderMarkupToByteStringIO g) . execMarkupT
