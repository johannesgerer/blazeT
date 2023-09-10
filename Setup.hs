{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Haddock
main = do
  defaultMainWithHooks simpleUserHooks{
    haddockHook = \p l h flags -> haddockHook simpleUserHooks p l h flags{
        haddockHoogle       = Flag True,
        haddockHtml         = Flag True,
        haddockProgramArgs  = [("-q",["aliased"])], -- does not seam to do anything
        haddockExecutables  = Flag True,
#   if defined(MIN_VERSION_Cabal)
#     if MIN_VERSION_Cabal(2, 2, 0)
        haddockLinkedSource = Flag True
#     else
        haddockHscolour     = Flag True
#     endif
#   else
        -- Almost certainly Cabal 1.22 or older
        haddockHscolour     = Flag True
#   endif
        }
    }

