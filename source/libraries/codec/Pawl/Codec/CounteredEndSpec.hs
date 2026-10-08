module Pawl.Codec.CounteredEndSpec where

import qualified Pawl.Codec.CounteredEnd as CounteredEnd
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounteredEnd as CounteredEnd
import qualified Pawl.Types.LibraryPosition as LibraryPosition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CounteredEnd" $ do
  -- Memory Lapse's "on top".
  Spec.it s "Stated" $
    Common.assertCodec
      s
      CounteredEnd.codec
      (CounteredEnd.Stated LibraryPosition.Top)
      " {\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}} "
  -- Hinder's "your choice of the top or bottom".
  Spec.it s "CounteringPlayerChooses" $
    Common.assertCodec
      s
      CounteredEnd.codec
      CounteredEnd.CounteringPlayerChooses
      " {\"type\":\"CounteringPlayerChooses\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CounteredEnd.codec
