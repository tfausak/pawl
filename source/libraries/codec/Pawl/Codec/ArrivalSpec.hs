module Pawl.Codec.ArrivalSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Codec.Arrival as Arrival
import qualified Pawl.Codec.ArrivalEnd as ArrivalEnd
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Arrival as Arrival
import qualified Pawl.Types.ArrivalEnd as ArrivalEnd
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Arrival" $ do
  Spec.it s "a melded pair at the bottom of a library" $
    Common.assertCodec
      s
      Arrival.codec
      Arrival.MkArrival
        { Arrival.owner = PlayerId.MkPlayerId 1,
          Arrival.end = ArrivalEnd.IntoLibrary LibraryPosition.Bottom,
          Arrival.cards = Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 4]
        }
      " {\"cards\":[7,4],\"end\":{\"type\":\"IntoLibrary\",\"value\":{\"type\":\"Bottom\"}},\"owner\":1} "
  Spec.it s "one card onto a graveyard" $
    Common.assertCodec
      s
      Arrival.codec
      Arrival.MkArrival
        { Arrival.owner = PlayerId.MkPlayerId 2,
          Arrival.end = ArrivalEnd.OntoGraveyard,
          Arrival.cards = Seq.singleton (ObjectId.MkObjectId 9)
        }
      " {\"cards\":[9],\"end\":{\"type\":\"OntoGraveyard\"},\"owner\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Arrival.codec
  Spec.it s "ArrivalEnd has a schema" $
    Common.assertHasSchema s ArrivalEnd.codec
