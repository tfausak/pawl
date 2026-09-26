module Pawl.Codec.LibraryDepthSpec where

import qualified Pawl.Codec.LibraryDepth as LibraryDepth
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LibraryDepth as LibraryDepth

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LibraryDepth" $ do
  -- Calim, Djinn Emperor's "seventh from the top".
  Spec.it s "FromTop" $
    Common.assertCodec
      s
      LibraryDepth.codec
      (LibraryDepth.FromTop 7)
      " {\"type\":\"FromTop\",\"value\":7} "
  -- Mine Security's "into the top eight cards of your library at random".
  Spec.it s "AtRandomInTop" $
    Common.assertCodec
      s
      LibraryDepth.codec
      (LibraryDepth.AtRandomInTop 8)
      " {\"type\":\"AtRandomInTop\",\"value\":8} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LibraryDepth.codec
