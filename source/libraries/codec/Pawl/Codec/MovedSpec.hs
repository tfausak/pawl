module Pawl.Codec.MovedSpec where

import qualified Pawl.Codec.Moved as Moved
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Moved" $ do
  -- CR 400.7's move, with a NEW id on arrival -- the shape that makes the two
  -- ObjectId keys inside the ZoneChange distinguishable.
  Spec.it s "MkMoved, both keys" $
    Common.assertCodec
      s
      Moved.codec
      ( Moved.MkMoved
          { Moved.change = ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) (ObjectId.MkObjectId 2) Zone.Battlefield Zone.Graveyard,
            Moved.characteristics = ProjectedCharacteristicsSpec.testCharacteristics
          }
      )
      ( "{\"change\":{\"departed\":1,\"object\":2,\"from\":{\"type\":\"Battlefield\"},\"to\":{\"type\":\"Graveyard\"}},\"characteristics\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s Moved.codec
