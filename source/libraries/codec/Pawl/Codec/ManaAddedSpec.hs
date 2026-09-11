module Pawl.Codec.ManaAddedSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.ManaAdded as ManaAdded
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaAdded as ManaAdded
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaAdded" $ do
  -- Distinct numbers in the player and source keys, so a codec swapping the two
  -- would not round-trip.
  Spec.it s "MkManaAdded, every key" $
    Common.assertCodec
      s
      ManaAdded.codec
      (ManaAdded.MkManaAdded {ManaAdded.player = PlayerId.MkPlayerId 2, ManaAdded.source = ObjectId.MkObjectId 7, ManaAdded.mana = Set.fromList [ManaType.Colored Color.Red, ManaType.Colorless]})
      " {\"player\":2,\"source\":7,\"mana\":[{\"type\":\"Colored\",\"value\":{\"type\":\"Red\"}},{\"type\":\"Colorless\"}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaAdded.codec
