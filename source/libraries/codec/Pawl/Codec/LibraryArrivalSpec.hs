module Pawl.Codec.LibraryArrivalSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.LibraryArrival as LibraryArrival
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LibraryArrival as LibraryArrival
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LibraryArrival" $ do
  Spec.it s "a melded pair at the bottom" $
    Common.assertCodec
      s
      LibraryArrival.codec
      LibraryArrival.MkLibraryArrival
        { LibraryArrival.owner = PlayerId.MkPlayerId 1,
          LibraryArrival.position = LibraryPosition.Bottom,
          LibraryArrival.cards = Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 4]
        }
      " {\"cards\":[7,4],\"owner\":1,\"position\":{\"type\":\"Bottom\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s LibraryArrival.codec
