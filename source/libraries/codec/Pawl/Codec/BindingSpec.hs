module Pawl.Codec.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

-- | R7's one case for MkBinding's single constructor, at its all-Nothing unit
-- (Binding.empty).
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Binding" $ do
  Spec.it s "MkBinding, the empty binding" $
    Common.assertJsonCodec
      s
      Binding.toJson
      Binding.fromJson
      Binding.empty
      "{\"target\":null,\"amount\":null,\"modes\":null,\"copy\":null}"
  -- The codec is meant to be total over every Binding field -- amount, modes
  -- and copy too -- so round-trip a Binding with all four populated at once. No
  -- real slot ever carries all four together (copy lives only under the
  -- dedicated copySource slot in practice); this is a codec totality check, not
  -- a claim about a reachable game state.
  -- 'ProjectedCharacteristicsSpec.testCharacteristics' is the stand-in for a
  -- real snapshot -- this sublibrary cannot reach the registry or the
  -- engine's own Projection.project.
  Spec.it s "MkBinding, every field populated" $
    Common.assertJsonCodec
      s
      Binding.toJson
      Binding.fromJson
      ( Binding.MkBinding
          { Binding.target = Just (Recipient.ToPlayer (PlayerId.MkPlayerId 0)),
            Binding.amount = Just 3,
            Binding.modes = Just (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]),
            Binding.copy = Just ProjectedCharacteristicsSpec.testCharacteristics
          }
      )
      ( "{\"target\":{\"type\":\"ToPlayer\",\"value\":0},"
          <> "\"amount\":3,\"modes\":[0,2],\"copy\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "}"
      )
  -- Pawl.Codec.Binding.toJsonMap's own comment: a name-keyed map is a sorted
  -- array of entries (Map.toAscList) so the render is deterministic and the
  -- file byte-stable, the same shape Pawl.Codec.TriggeredAbility.toJsonDelayed
  -- takes for CR 603.7's delayed abilities. Two entries, inserted here in
  -- DESCENDING slot-name order, so a trip that emitted the map's incidental
  -- traversal order rather than Map.toAscList would fail this case even
  -- though both entries proved correct in isolation.
  Spec.it s "toJsonMap/fromJsonMap sorts by slot name" $
    Common.assertJsonCodec
      s
      Binding.toJsonMap
      Binding.fromJsonMap
      ( Map.fromList
          [ (SlotName.MkSlotName (Text.pack "z-slot"), Binding.empty {Binding.amount = Just 1}),
            (SlotName.MkSlotName (Text.pack "a-slot"), Binding.empty {Binding.amount = Just 2})
          ]
      )
      ( "[{\"slot\":\"a-slot\",\"binding\":{\"target\":null,\"amount\":2,\"modes\":null,\"copy\":null}},"
          <> "{\"slot\":\"z-slot\",\"binding\":{\"target\":null,\"amount\":1,\"modes\":null,\"copy\":null}}]"
      )
