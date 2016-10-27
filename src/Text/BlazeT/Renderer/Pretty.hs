{-# LANGUAGE RankNTypes #-}
module Text.BlazeT.Renderer.Pretty
    ( renderMarkup
    , renderHtml
  ) where

import qualified Text.Blaze.Renderer.Pretty as BU
import           Text.BlazeT

renderMarkup :: MarkupM a -> String
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupM a -> String
renderHtml = renderMarkup

