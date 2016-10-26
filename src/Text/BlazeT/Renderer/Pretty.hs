module Text.BlazeT.Renderer.Pretty
    ( renderMarkup
    , renderHtml
    , renderMarkupT
    , renderHtmlT
  ) where

import           Control.Monad
import           Control.Monad.Identity
import qualified Text.Blaze.Renderer.Pretty as BU
import           Text.BlazeT

renderMarkup :: MarkupM a -> String
renderMarkup = runIdentity . renderMarkupT
renderMarkupT :: Monad m => MarkupT m a -> m String
renderMarkupT = liftM BU.renderMarkup . execMarkupT

renderHtml :: MarkupM a -> String
renderHtml = renderMarkup
renderHtmlT :: Monad m => MarkupT m a -> m String
renderHtmlT = renderMarkupT

