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
    -- * Entities exported only by the @blazeT@ version of this module
    MarkupT(..)
    ,MarkupI
    ,mapMarkupT
    -- ** Specializations for @blaze-markup@ backwards compatibility
    ,MarkupM
    ,Markup
    ,Markup2
    -- ** Running
    ,runMarkupT
    ,runMarkup
    ,runWith
    -- ** Executing
    ,execMarkupT
    ,execMarkup
    ,execWith
    -- ** Wrappers
    ,wrapMarkupT
    ,wrapMarkupT2
    ,wrapMarkup
    ,wrapMarkup2
    ,
    
    -- * Entities exported also by "Text.Blaze.Internal"
    -- $descr1
    
      -- ** Important types.
      Text.Blaze.ChoiceString (..)
    , Text.Blaze.StaticString (..)
    -- , MarkupM
    -- , Markup
    , Text.Blaze.Tag
    , Text.Blaze.Attribute
    , Text.Blaze.AttributeValue

      -- ** Creating custom tags and attributes.
    , customParent
    , customLeaf
    , Text.Blaze.attribute
    , Text.Blaze.dataAttribute
    , Text.Blaze.customAttribute

      -- ** Converting values to Markup.
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

      -- ** Comments
    , textComment
    , lazyTextComment
    , stringComment
    , unsafeByteStringComment
    , unsafeLazyByteStringComment

      -- ** Converting values to tags.
    , Text.Blaze.textTag
    , Text.Blaze.stringTag

      -- ** Converting values to attribute values.
    , Text.Blaze.textValue
    , Text.Blaze.preEscapedTextValue
    , Text.Blaze.lazyTextValue
    , Text.Blaze.preEscapedLazyTextValue
    , Text.Blaze.textBuilderValue
    , Text.Blaze.preEscapedTextBuilderValue
    , Text.Blaze.stringValue
    , Text.Blaze.preEscapedStringValue
    , Text.Blaze.unsafeByteStringValue
    , Text.Blaze.unsafeLazyByteStringValue

      -- ** Setting attributes
    , Text.Blaze.Attributable
    , (Text.Blaze.!)
    , (Text.Blaze.!?)

      -- ** Modifying Markup elements
    , contents
    , external

      -- ** Querying Markup elements
    , null

  ) where

import           Control.Arrow
import           Control.Monad.Identity
import           Control.Monad.Trans.Class
import           Control.Monad.Writer.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Text.Blaze
import qualified Text.Blaze.Internal as Text.Blaze

{- | Everything is build around the simple @newtype@ definition of the
'MarkupT' transformer, which makes use the 'Monoid' instance of Blaze
'Text.Blaze.Markup' and is simply a 'WriterT' writing Blaze
'Text.Blaze.Markup':
-}
newtype MarkupT m a= MarkupT { fromMarkupT :: WriterT Text.Blaze.Markup m a }
                     deriving (Functor
#if MIN_VERSION_base(4,8,0)
                              ,Applicative
#endif
                              ,Monad
                              ,MonadWriter Text.Blaze.Markup
                              ,MonadTrans
                              )

type MarkupI a = MarkupT Identity a

-- | Map both the return value and markup of a computation using the
-- given function
mapMarkupT :: (m (a,Text.Blaze.Markup) -> n (b,Text.Blaze.Markup)) -> MarkupT m a -> MarkupT n b
mapMarkupT f = MarkupT . mapWriterT f . fromMarkupT
{-# INLINE mapMarkupT #-}

type MarkupM a = forall m . Monad m => MarkupT m a
type Markup = MarkupM ()
type Markup2 = forall m . Monad m => MarkupT m () -> MarkupT m ()

runMarkupT :: MarkupT m a -> m (a,Text.Blaze.Markup)
runMarkupT = runWriterT . fromMarkupT
{-# INLINE runMarkupT #-}

-- | run the MarkupT and return a pair consisting of the result of the
-- computation and the blaze markup rendered with a blaze renderer
-- like 'Text.BlazeT.Renderer.Text.renderHtml'
runWith :: Monad m => (MarkupI () -> c) -> MarkupT m a -> m (a, c)
runWith renderer =  liftM (second $ renderer . wrapMarkup) . runMarkupT  
{-# INLINE runWith #-}
  
execMarkupT :: Monad m => MarkupT m a -> m Text.Blaze.Markup
execMarkupT = liftM snd . runMarkupT
{-# INLINE execMarkupT #-}

execWith :: Monad m => (MarkupI () -> c) -> MarkupT m a -> m c
execWith renderer = liftM snd . runWith renderer
{-# INLINE execWith #-}

runMarkup :: MarkupI a -> (a, Text.Blaze.Markup)
runMarkup = runIdentity . runMarkupT
{-# INLINE runMarkup #-}

execMarkup :: MarkupI a -> Text.Blaze.Markup
execMarkup = snd . runMarkup
{-# INLINE execMarkup #-}

-- | Wrapper for 'Text.Blaze.Markup' is simply
-- 'tell'
wrapMarkupT :: Monad m => Text.Blaze.Markup -> MarkupT m ()
wrapMarkupT = tell
{-# INLINE wrapMarkupT #-}

wrapMarkup :: Text.Blaze.Markup -> Markup
wrapMarkup = wrapMarkupT
{-# INLINE wrapMarkup #-}


-- | Wrapper for functions that modify 'Text.Blaze.Markup' is simply
-- 'censor'
wrapMarkupT2 ::  Monad m => (Text.Blaze.Markup -> Text.Blaze.Markup)
                 -> MarkupT m a -> MarkupT m a
wrapMarkupT2 = censor
{-# INLINE wrapMarkupT2 #-}

wrapMarkup2 :: (Text.Blaze.Markup -> Text.Blaze.Markup) -> Markup2
wrapMarkup2 = wrapMarkupT2
{-# INLINE wrapMarkup2 #-}


mappendMarkupT
#if defined(MIN_VERSION_base)
#  if MIN_VERSION_base(4, 11, 0)
  :: (Monad m, Semigroup a)
#  else
  :: (Monad m, Monoid a)
#  endif
#else
  -- Almost certainly GHC < 8, which corresponds to `base` w/o Semigroup
  :: (Monad m, Monoid a)
#endif
  => MarkupT m a
  -> MarkupT m a
  -> MarkupT m a
a `mappendMarkupT` b = do {a' <- a; b >>= return . (a' <>)}

#if defined(MIN_VERSION_base)
#  if MIN_VERSION_base(4, 11, 0)
instance (Monad m,Semigroup a) => Semigroup (MarkupT m a) where
  (<>) = mappendMarkupT
  {-# INLINE (<>) #-}
#  else
instance (Monad m,Monoid a) => Monoid (MarkupT m a) where
  mempty = return mempty
  {-# INLINE mempty #-}
  mappend = mappendMarkupT
  {-# INLINE mappend #-}
#  endif
#else
  -- Almost certainly GHC < 8, which corresponds to `base` w/o Semigroup
instance (Monad m,Monoid a) => Monoid (MarkupT m a) where
  mempty = return mempty
  {-# INLINE mempty #-}
  mappend = mappendMarkupT
  {-# INLINE mappend #-}
#endif


instance Monad m => Text.Blaze.Attributable (MarkupT m a) where
  h ! a = wrapMarkupT2 (Text.Blaze.! a) h
  {-# INLINE (!) #-}

instance Monad m => Text.Blaze.Attributable (a -> MarkupT m b) where
  h ! a = \x -> wrapMarkupT2 (Text.Blaze.! a) $ h x
  {-# INLINE (!) #-}

instance Monad m => IsString (MarkupT m ()) where
  fromString = wrapMarkup . fromString
  {-# INLINE fromString #-}

unsafeByteString :: BS.ByteString -> Markup
unsafeByteString = wrapMarkup . Text.Blaze.unsafeByteString
{-# INLINE unsafeByteString #-}

-- | Insert a lazy 'BL.ByteString'. See 'unsafeByteString' for reasons why this
-- is an unsafe operation.
--
unsafeLazyByteString :: BL.ByteString  -- ^ Value to insert
                     -> Markup         -- ^ Resulting HTML fragment
unsafeLazyByteString = wrapMarkup . Text.Blaze.unsafeLazyByteString
{-# INLINE unsafeLazyByteString #-}

external :: Monad m => MarkupT m a -> MarkupT m a
external = wrapMarkupT2  Text.Blaze.external
{-# INLINE external #-}

contents :: Monad m => MarkupT m a -> MarkupT m a
contents = wrapMarkupT2  Text.Blaze.contents
{-# INLINE contents #-}

customParent ::Text.Blaze.Tag -> Markup2
customParent = wrapMarkup2 . Text.Blaze.customParent
{-# INLINE customParent #-}

customLeaf :: Text.Blaze.Tag -> Bool -> Markup
customLeaf = fmap wrapMarkup . Text.Blaze.customLeaf
{-# INLINE customLeaf #-}

preEscapedText :: T.Text -> Markup
preEscapedText = wrapMarkup . Text.Blaze.preEscapedText
{-# INLINE preEscapedText #-}

preEscapedLazyText :: LT.Text -> Markup
preEscapedLazyText = wrapMarkup . Text.Blaze.preEscapedLazyText
{-# INLINE preEscapedLazyText #-}

preEscapedTextBuilder :: LTB.Builder -> Markup
textBuilder :: LTB.Builder -> Markup

#ifdef PRE_BUILDER
preEscapedTextBuilder = wrapMarkup . Text.Blaze.preEscapedTextBuilder
textBuilder = wrapMarkup . Text.Blaze.textBuilder
{-# INLINE preEscapedTextBuilder #-}
{-# INLINE textBuilder #-}
#else
preEscapedTextBuilder = error "This function needs blaze-markup 0.7.1.0"
textBuilder = error "This function needs blaze-markup 0.7.1.0"
#endif

preEscapedString :: String -> Markup
preEscapedString = wrapMarkup . Text.Blaze.preEscapedString
{-# INLINE preEscapedString #-}

string :: String -> Markup
string = wrapMarkup . Text.Blaze.string
{-# INLINE string #-}

text :: T.Text -> Markup
text = wrapMarkup . Text.Blaze.text
{-# INLINE text #-}

lazyText :: LT.Text -> Markup
lazyText = wrapMarkup . Text.Blaze.lazyText
{-# INLINE lazyText #-}


textComment :: T.Text -> Markup
textComment = wrapMarkup . Text.Blaze.textComment

lazyTextComment :: LT.Text -> Markup
lazyTextComment = wrapMarkup . Text.Blaze.lazyTextComment

stringComment :: String -> Markup
stringComment = wrapMarkup . Text.Blaze.stringComment

unsafeByteStringComment :: BS.ByteString -> Markup
unsafeByteStringComment = wrapMarkup . Text.Blaze.unsafeByteStringComment

unsafeLazyByteStringComment :: BL.ByteString -> Markup
unsafeLazyByteStringComment = wrapMarkup . Text.Blaze.unsafeLazyByteStringComment

-- $descr1
-- The following is an adaptation of all "Text.Blaze.Internal" exports to
-- @blazeT@ types.
-- 
-- Entities that are reexported from "Text.Blaze.Internal" have the original
-- documentation attached to them.
--
-- Entities that had to be adapted are tagged with \"(Adapted)\". For
-- their documentation consult the "Text.Blaze.Internal" documentation.
