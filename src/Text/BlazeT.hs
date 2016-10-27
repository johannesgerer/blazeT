{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Text.BlazeT
  (
   -- * DO NOT READ THIS. READ "Text.BlazeT.Internal" INSTEAD 
    -- $descr
    
   -- * DO NOT READ THIS
-- -- * Important types.
      Markup
    , Tag
    , Attribute
    , AttributeValue

-- -- * Creating attributes.
    , dataAttribute
    , customAttribute

-- -- * Converting values to Markup.
    , ToMarkup (..)
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

-- -- * Comments
    , textComment
    , lazyTextComment
    , stringComment
    , unsafeByteStringComment
    , unsafeLazyByteStringComment

-- -- * Creating tags.
    , textTag
    , stringTag

-- -- * Converting values to attribute values.
    , Text.Blaze.ToValue (..)
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue
    , unsafeLazyByteStringValue

-- -- * Setting attributes
    , (!)
    , (!?)

-- -- * Modifiying Markup trees
    , contents

    ,MarkupT(..)
    ,MarkupI
    ,mapMarkupT
    ,MarkupM
    ,Markup2
    ,runMarkupT
    ,runMarkup
    ,runWith
    ,execMarkupT
    ,execMarkup
    ,execWith
    ) where

import qualified Text.Blaze
import           Text.BlazeT.Internal as Text.BlazeT.Internal 

class ToMarkup a where
  toMarkup :: a -> Markup
  preEscapedToMarkup :: a -> Markup

-- test :: (ToMarkup a, Monad m) => a -> MarkupT m ()
-- test = toMarkup

instance Text.Blaze.ToMarkup a => ToMarkup a where
  toMarkup = wrapMarkup . Text.Blaze.toMarkup
  {-# INLINE toMarkup #-}
  preEscapedToMarkup = wrapMarkup . Text.Blaze.preEscapedToMarkup
  {-# INLINE preEscapedToMarkup #-}


-- $descr
-- 
-- Due due a Haddock bug, this documentation is misleading. Please
-- read "Text.BlazeT.Internal" instead.
--
-- (The bug shows both @Text.Blaze.Markup@ and @Text.BlazeT.Markup@ as
-- "Markup".)
--
-- Use this documentation only to see which entities are exported by
-- this module.
