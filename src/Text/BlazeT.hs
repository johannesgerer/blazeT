{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Text.BlazeT
  (
      -- * Important types.
      Markup
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating attributes.
    , dataAttribute
    , customAttribute

      -- * Converting values to Markup.
    , ToMarkup (..)
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

      -- * Comments
    , textComment
    , lazyTextComment
    , stringComment
    , unsafeByteStringComment
    , unsafeLazyByteStringComment

      -- * Creating tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , B.ToValue (..)
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue
    , unsafeLazyByteStringValue

      -- * Setting attributes
    , (!)
    , (!?)

      -- * Modifiying Markup trees
    , contents

    -- * BlazeT new stuff
    ,MarkupM
    ,Markup2
    ,mapMarkupT
    ,MarkupT
    ,runMarkup
    ,runMarkupT
    ,execMarkup
    ,execMarkupT
    ) where

import qualified Text.Blaze as B
import           Text.BlazeT.Internal

class ToMarkup a where
  toMarkup :: a -> Markup
  preEscapedToMarkup :: a -> Markup

-- test :: (ToMarkup a, Monad m) => a -> MarkupT m ()
-- test = toMarkup

instance B.ToMarkup a => ToMarkup a where
  toMarkup = wrapMarkup . B.toMarkup
  {-# INLINE toMarkup #-}
  preEscapedToMarkup = wrapMarkup . B.preEscapedToMarkup
  {-# INLINE preEscapedToMarkup #-}
