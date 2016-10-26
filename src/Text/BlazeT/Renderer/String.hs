module Text.BlazeT.Renderer.String
    ( fromChoiceString
    , renderMarkup
    , renderHtml
  ) where

import           Control.Monad
import           Control.Monad.Identity
import           Text.Blaze.Internal (ChoiceString)
import qualified Text.Blaze.Renderer.String as BU
import           Text.BlazeT

fromChoiceString :: ChoiceString -> String -> String       
fromChoiceString = BU.fromChoiceString

renderMarkup :: MarkupM a -> String
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupM a -> String
renderHtml = renderMarkup

