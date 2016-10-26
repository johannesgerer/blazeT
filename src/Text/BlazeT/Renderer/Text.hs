module Text.BlazeT.Renderer.Text
    ( renderMarkupBuilderT
    , renderMarkupBuilder
    , renderMarkupBuilderWithT
    , renderMarkupT
    , renderMarkupWithT
    , renderHtmlBuilderT
    , renderHtmlBuilderWithT
    , renderHtmlT
    , renderHtmlWithT
    , renderMarkupBuilderWith
    , renderMarkup
    , renderMarkupWith
    , renderHtmlBuilder
    , renderHtmlBuilderWith
    , renderHtml
    , renderHtmlWith
  ) where

import           Control.Monad
import           Data.ByteString (ByteString)
import           Control.Monad.Identity
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import qualified Text.Blaze.Html.Renderer.Text as BH
import qualified Text.Blaze.Renderer.Text as BU
import           Text.BlazeT

renderMarkupBuilder :: MarkupM a -> B.Builder
renderMarkupBuilder = runIdentity . renderMarkupBuilderT

renderMarkupBuilderT :: Monad m => MarkupT m a -> m B.Builder
renderMarkupBuilderT = liftM BU.renderMarkupBuilder . execMarkupT

renderHtmlBuilder :: MarkupM a -> B.Builder
renderHtmlBuilder = renderMarkupBuilder

renderHtmlBuilderT :: Monad m => MarkupT m a -> m B.Builder
renderHtmlBuilderT = renderMarkupBuilderT

renderMarkup :: MarkupM a -> L.Text
renderMarkup = runIdentity . renderMarkupT
renderMarkupT :: Monad m => MarkupT m a -> m L.Text
renderMarkupT = liftM BU.renderMarkup . execMarkupT

renderHtml :: MarkupM a -> L.Text
renderHtml = renderMarkup
renderHtmlT :: Monad m => MarkupT m a -> m L.Text
renderHtmlT = renderMarkupT

renderMarkupWithT :: Monad m => (ByteString -> Text) -> MarkupT m a -> m L.Text
renderMarkupWithT g = liftM (BU.renderMarkupWith g) . execMarkupT

renderMarkupWith :: (ByteString -> Text) -> MarkupM a -> L.Text
renderMarkupWith g = runIdentity . renderMarkupWithT g

renderHtmlWithT :: Monad m => (ByteString -> Text) -> MarkupT m a -> m L.Text
renderHtmlWithT g = liftM (BH.renderHtmlWith g) . execMarkupT

renderHtmlWith :: (ByteString -> Text) -> MarkupM a -> L.Text
renderHtmlWith g = runIdentity . renderHtmlWithT g

renderHtmlBuilderWithT :: Monad m => (ByteString -> Text) -> MarkupT m a -> m B.Builder       
renderHtmlBuilderWithT g = liftM (BH.renderHtmlBuilderWith g) . execMarkupT

renderHtmlBuilderWith :: (ByteString -> Text) -> MarkupM a -> B.Builder       
renderHtmlBuilderWith g = runIdentity . renderHtmlBuilderWithT g


renderMarkupBuilderWithT :: Monad m => (ByteString -> Text) -> MarkupT m a -> m B.Builder      
renderMarkupBuilderWithT g = liftM (BU.renderMarkupBuilderWith g) . execMarkupT

renderMarkupBuilderWith :: (ByteString -> Text) -> MarkupM a -> B.Builder       
renderMarkupBuilderWith g = runIdentity . renderMarkupBuilderWithT g
