module Pawl.Codec.ObjectSnapshotSpec where

import qualified Pawl.Codec.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectSnapshot" $ do
  -- Controller and owner differ, so an encoder writing one for the other fails.
  Spec.it s "a controlled object" $
    Common.assertCodec
      s
      ObjectSnapshot.codec
      ObjectSnapshot.MkObjectSnapshot
        { ObjectSnapshot.object = ObjectId.MkObjectId 9,
          ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
          ObjectSnapshot.controller = Just (PlayerId.MkPlayerId 1),
          ObjectSnapshot.owner = PlayerId.MkPlayerId 2
        }
      (" {\"object\":9,\"characteristics\":" <> ProjectedCharacteristicsSpec.testCharacteristicsJson <> ",\"controller\":1,\"owner\":2} ")
  -- CR 108.4: a card in a hand has no controller.
  Spec.it s "an object with no controller" $
    Common.assertCodec
      s
      ObjectSnapshot.codec
      ObjectSnapshot.MkObjectSnapshot
        { ObjectSnapshot.object = ObjectId.MkObjectId 3,
          ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
          ObjectSnapshot.controller = Nothing,
          ObjectSnapshot.owner = PlayerId.MkPlayerId 4
        }
      (" {\"object\":3,\"characteristics\":" <> ProjectedCharacteristicsSpec.testCharacteristicsJson <> ",\"controller\":null,\"owner\":4} ")
  Spec.it s "has a schema" $
    Common.assertHasSchema s ObjectSnapshot.codec
