module Pawl.Codec.PastActivationSpec where

import qualified Pawl.Codec.PastActivation as PastActivation
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Types.PastActivation as PastActivation
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PastActivation" $ do
  let snapshotOf n =
        ObjectSnapshot.MkObjectSnapshot
          { ObjectSnapshot.object = ObjectId.MkObjectId n,
            ObjectSnapshot.characteristics = ProjectedCharacteristicsSpec.testCharacteristics,
            ObjectSnapshot.controller = Just (PlayerId.MkPlayerId 1),
            ObjectSnapshot.owner = PlayerId.MkPlayerId 1
          }
      snapshotJson n = "{\"object\":" <> show (n :: Integer) <> ",\"characteristics\":" <> ProjectedCharacteristicsSpec.testCharacteristicsJson <> ",\"controller\":1,\"owner\":1}"
  Spec.it s "a keyword ability announced at one target" $
    Common.assertCodec
      s
      PastActivation.codec
      PastActivation.MkPastActivation
        { PastActivation.activator = PlayerId.MkPlayerId 2,
          PastActivation.source = snapshotOf 5,
          PastActivation.keyword = Just Keyword.Flying,
          PastActivation.kind = AbilityKind.NonManaAbility,
          PastActivation.targets = [snapshotOf 6]
        }
      (" {\"activator\":2,\"source\":" <> snapshotJson 5 <> ",\"keyword\":{\"type\":\"Flying\"},\"kind\":{\"type\":\"NonManaAbility\"},\"targets\":[" <> snapshotJson 6 <> "]} ")
  -- CR 605.1a: a mana ability names no target.
  Spec.it s "a mana ability" $
    Common.assertCodec
      s
      PastActivation.codec
      PastActivation.MkPastActivation
        { PastActivation.activator = PlayerId.MkPlayerId 1,
          PastActivation.source = snapshotOf 7,
          PastActivation.keyword = Nothing,
          PastActivation.kind = AbilityKind.ManaAbility,
          PastActivation.targets = []
        }
      (" {\"activator\":1,\"source\":" <> snapshotJson 7 <> ",\"keyword\":null,\"kind\":{\"type\":\"ManaAbility\"},\"targets\":[]} ")
  Spec.it s "has a schema" $
    Common.assertHasSchema s PastActivation.codec
