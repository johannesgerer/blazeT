{-# LANGUAGE OverloadedStrings #-}

import Data.Time (getCurrentTime)
import Text.BlazeT.Html5 hiding (main)
import Text.BlazeT.Renderer.String
import Control.Monad.Trans.Class (lift)

-- Backwords compatible Blaze HTML
old :: Markup
old = do
  p $ "created with blaze-html"

-- BlazeT HTML with lifted IO actions
new :: MarkupT IO ()
new = do
  time <- lift getCurrentTime
  p $ string $ "created with blazeT at " ++ show time

main :: IO ()
main = do
  putStrLn $            renderMarkup old
  putStrLn =<< execWith renderMarkup new
  
