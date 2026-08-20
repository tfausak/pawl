module Pawl.Codec.LibraryPlacementSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.LibraryPlacement as LibraryPlacement
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LibraryPlacement" $ do
  -- A stated end writes its own tag around the position's, rather than the
  -- position's alone.
  Spec.it s "Stated Top" $
    Common.assertCodec
      s
      LibraryPlacement.codec
      (LibraryPlacement.Stated LibraryPosition.Top)
      " {\"type\":\"Stated\",\"value\":{\"type\":\"Top\"}} "
  Spec.it s "Stated Bottom" $
    Common.assertCodec
      s
      LibraryPlacement.codec
      (LibraryPlacement.Stated LibraryPosition.Bottom)
      " {\"type\":\"Stated\",\"value\":{\"type\":\"Bottom\"}} "
  Spec.it s "OwnerChooses" $
    Common.assertCodec
      s
      LibraryPlacement.codec
      LibraryPlacement.OwnerChooses
      " {\"type\":\"OwnerChooses\"} "
  -- A random order states its end the same way Stated does: the arm says who
  -- orders the batch, and the position it carries says where the batch lands.
  Spec.it s "RandomOrder Bottom" $
    Common.assertCodec
      s
      LibraryPlacement.codec
      (LibraryPlacement.RandomOrder LibraryPosition.Bottom)
      " {\"type\":\"RandomOrder\",\"value\":{\"type\":\"Bottom\"}} "
  -- A bare position is no longer a placement, which is what keeps the schema's
  -- oneOf honest. It no longer has to tell a placement from a zone --- moveTail
  -- is gone (#1305) and the placement is a named key.
  Spec.it s "rejects a bare position" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"Top\"} ") >>= Codec.decode LibraryPlacement.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s LibraryPlacement.codec
