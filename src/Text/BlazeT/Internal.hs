{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#if MIN_VERSION_blaze_markup(0,7,1)
#define PRE_BUILDER
#endif
module Text.BlazeT.Internal
    (
      -- * Important types.
      B.ChoiceString (..)
    , B.StaticString (..)
    , MarkupM
    , Markup
    , B.Tag
    , B.Attribute
    , B.AttributeValue

      -- * Creating custom tags and attributes.
    , customParent
    , customLeaf
    , B.attribute
    , B.dataAttribute
    , B.customAttribute

      -- * Converting values to Markup.
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , textBuilder
    , preEscapedTextBuilder
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

      -- * Comments
    , B.textComment
    , B.lazyTextComment
    , B.stringComment
    , B.unsafeByteStringComment
    , B.unsafeLazyByteStringComment

      -- * Converting values to tags.
    , B.textTag
    , B.stringTag

      -- * Converting values to attribute values.
    , B.textValue
    , B.preEscapedTextValue
    , B.lazyTextValue
    , B.preEscapedLazyTextValue
    , B.textBuilderValue
    , B.preEscapedTextBuilderValue
    , B.stringValue
    , B.preEscapedStringValue
    , B.unsafeByteStringValue
    , B.unsafeLazyByteStringValue

      -- * Setting attributes
    , B.Attributable
    , (B.!)
    , (B.!?)

      -- * Modifying Markup elements
    , contents
    , external

      -- * Querying Markup elements
    , null

    -- * BlazeT new stuff
    ,Markup2
    ,mapMarkupT
    ,MarkupT
    ,runMarkup
    ,runMarkupT
    ,execMarkup
    ,execMarkupT
    ,wrapMarkup
    ,wrapMarkupT
    ,wrapMarkup2
    ,wrapMarkupT2
  ) where

import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Text.Blaze as B
import qualified Text.Blaze.Internal as B

newtype MarkupT m a= MarkupT { fromMarkupT :: WriterT B.Markup m a }
                     deriving (Functor
#if MIN_VERSION_base(4,8,0)
                              ,Applicative
#endif
                              ,Monad
                              ,MonadWriter B.Markup
                              ,MonadTrans
                              )

-- | Map both the return value and markup of a computation using the
-- given function
mapMarkupT :: (m (a,B.Markup) -> n (b,B.Markup)) -> MarkupT m a -> MarkupT n b
mapMarkupT f = MarkupT . mapWriterT f . fromMarkupT
{-# INLINE mapMarkupT #-}

type MarkupM = MarkupT Identity
type Markup = forall m . Monad m => MarkupT m ()
type Markup2 = forall m . Monad m => MarkupT m () -> MarkupT m ()

runMarkupT :: MarkupT m a -> m (a,B.Markup)
runMarkupT = runWriterT . fromMarkupT
{-# INLINE runMarkupT #-}

execMarkupT :: Monad m => MarkupT m a -> m B.Markup
execMarkupT = liftM snd . runMarkupT
{-# INLINE execMarkupT #-}

runMarkup :: MarkupM a -> (a,B.Markup)
runMarkup = runIdentity . runMarkupT
{-# INLINE runMarkup #-}

execMarkup :: MarkupM a -> B.Markup
execMarkup = snd . runMarkup
{-# INLINE execMarkup #-}

-- instance MonadTrans MarkupT where


instance (Monad m,Monoid a) => Monoid (MarkupT m a) where
  mempty = return mempty
  {-# INLINE mempty #-}
  a `mappend` b = do {a' <- a; b >>= return . (mappend a')}
  {-# INLINE mappend #-}


instance Monad m => B.Attributable (MarkupT m a) where
  h ! a = wrapMarkupT2 (B.! a) h
  {-# INLINE (!) #-}

instance Monad m => B.Attributable (a -> MarkupT m b) where
  h ! a = \x -> wrapMarkupT2 (B.! a) $ h x
  {-# INLINE (!) #-}

instance Monad m => IsString (MarkupT m ()) where
  fromString = wrapMarkup . fromString
  {-# INLINE fromString #-}

wrapMarkupT :: Monad m => B.Markup -> MarkupT m ()
wrapMarkupT = tell
{-# INLINE wrapMarkupT #-}

wrapMarkup :: B.Markup -> Markup
wrapMarkup = wrapMarkupT
{-# INLINE wrapMarkup #-}

wrapMarkupT2 ::  Monad m => (B.Markup -> B.Markup)
                 -> MarkupT m a -> MarkupT m a
wrapMarkupT2 = censor
{-# INLINE wrapMarkupT2 #-}

wrapMarkup2 :: (B.Markup -> B.Markup) -> Markup2
wrapMarkup2 = wrapMarkupT2
{-# INLINE wrapMarkup2 #-}

unsafeByteString :: BS.ByteString -> Markup
unsafeByteString = wrapMarkup . B.unsafeByteString
{-# INLINE unsafeByteString #-}

-- | Insert a lazy 'BL.ByteString'. See 'unsafeByteString' for reasons why this
-- is an unsafe operation.
--
unsafeLazyByteString :: BL.ByteString  -- ^ Value to insert
                     -> Markup         -- ^ Resulting HTML fragment
unsafeLazyByteString = wrapMarkup . B.unsafeLazyByteString
{-# INLINE unsafeLazyByteString #-}

external :: Monad m => MarkupT m a -> MarkupT m a
external = wrapMarkupT2  B.external
{-# INLINE external #-}

contents :: Monad m => MarkupT m a -> MarkupT m a
contents = wrapMarkupT2  B.contents
{-# INLINE contents #-}

customParent ::B.Tag -> Markup2
customParent = wrapMarkup2 . B.customParent
{-# INLINE customParent #-}

customLeaf :: B.Tag -> Bool -> Markup
customLeaf = fmap wrapMarkup . B.customLeaf
{-# INLINE customLeaf #-}

preEscapedText :: T.Text -> Markup
preEscapedText = wrapMarkup . B.preEscapedText
{-# INLINE preEscapedText #-}

preEscapedLazyText :: LT.Text -> Markup
preEscapedLazyText = wrapMarkup . B.preEscapedLazyText
{-# INLINE preEscapedLazyText #-}

preEscapedTextBuilder :: LTB.Builder -> Markup
textBuilder :: LTB.Builder -> Markup

#ifdef PRE_BUILDER
preEscapedTextBuilder = wrapMarkup . B.preEscapedTextBuilder
textBuilder = wrapMarkup . B.textBuilder
{-# INLINE preEscapedTextBuilder #-}
{-# INLINE textBuilder #-}
#else
preEscapedTextBuilder = error "This function needs blaze-markup 0.7.1.0"
textBuilder = error "This function needs blaze-markup 0.7.1.0"
#endif

preEscapedString :: String -> Markup
preEscapedString = wrapMarkup . B.preEscapedString
{-# INLINE preEscapedString #-}

string :: String -> Markup
string = wrapMarkup . B.string
{-# INLINE string #-}

text :: T.Text -> Markup
text = wrapMarkup . B.text
{-# INLINE text #-}

lazyText :: LT.Text -> Markup
lazyText = wrapMarkup . B.lazyText
{-# INLINE lazyText #-}
