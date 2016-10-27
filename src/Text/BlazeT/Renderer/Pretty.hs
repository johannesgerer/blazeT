{-# LANGUAGE RankNTypes #-}
module Text.BlazeT.Renderer.Pretty
    ( renderMarkup
    , renderHtml
  ) where

import qualified Text.Blaze.Renderer.Pretty as BU
import           Text.BlazeT

renderMarkup :: MarkupI a -> String
renderMarkup = BU.renderMarkup . execMarkup

renderHtml :: MarkupI a -> String
renderHtml = renderMarkup

