module Text.BlazeT.Renderer.String
    ( fromChoiceString
    , renderMarkup
    , renderHtml
    , renderMarkupT
    , renderHtmlT
  ) where

import           Control.Monad
import           Control.Monad.Identity
import           Text.Blaze.Internal (ChoiceString)
import qualified Text.Blaze.Renderer.String as BU
import           Text.BlazeT

fromChoiceString :: ChoiceString -> String -> String       
fromChoiceString = BU.fromChoiceString

renderMarkup :: MarkupM a -> String
renderMarkup = runIdentity . renderMarkupT
renderMarkupT :: Monad m => MarkupT m a -> m String
renderMarkupT = liftM BU.renderMarkup . execMarkupT

renderHtml :: MarkupM a -> String
renderHtml = renderMarkup
renderHtmlT :: Monad m => MarkupT m a -> m String
renderHtmlT = renderMarkupT

