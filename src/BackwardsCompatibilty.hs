{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

import Text.Blaze.Html5.Attributes
#if 1
import Text.BlazeT
import Text.BlazeT.Html5 hiding (main)
import Text.BlazeT.Renderer.Utf8
#else
import Text.Blaze
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Renderer.Utf8
#endif

main :: IO ()
main = print $ renderMarkup $ do
  docType
  -- "some text" -- does not work
  html $ do text "some text" -- does not work
            br
  ("wow" :: Markup) -- overloaded strings
  text "asd" ! href "asd"
  string "string" ! href "asd"
  toMarkup ("more text" :: String)
  html "wow"
  html ! src "asd" $ br

