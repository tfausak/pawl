module Pawl.Codec.RoomIndexSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.RoomIndex as RoomIndex

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RoomIndex" $ do
  Spec.it s "MkRoomIndex" $
    Common.assertCodec
      s
      RoomIndex.codec
      (RoomIndex.MkRoomIndex 3)
      " 3 "

  Spec.it s "has a schema" $
    Common.assertHasSchema s RoomIndex.codec

  -- The wrapped type is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " -1 ") >>= Codec.decode RoomIndex.codec))
      "expected a decode failure"
