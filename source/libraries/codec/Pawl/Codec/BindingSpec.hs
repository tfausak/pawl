{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BindingSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SlotName as SlotName

-- | MkBinding at its all-Nothing unit (Binding.empty).
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Binding" $ do
  Spec.it s "MkBinding, the empty binding" $
    Common.assertJsonCodec
      s
      Binding.toJson
      Binding.fromJson
      Binding.empty
      """ {} """
  -- A codec totality check, not a claim about a reachable game state: no real
  -- slot carries all five fields at once. The stand-in snapshot is needed
  -- because this sublibrary cannot reach the registry or Projection.project.
  Spec.it s "MkBinding, every field populated" $
    Common.assertJsonCodec
      s
      Binding.toJson
      Binding.fromJson
      ( Binding.MkBinding
          { Binding.targets = Just (Set.singleton (Recipient.ToPlayer (PlayerId.MkPlayerId 0))),
            Binding.amount = Just 3,
            Binding.modes = Just (Seq.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2, ModeIndex.MkModeIndex 2]),
            Binding.copy = Just ProjectedCharacteristicsSpec.testCharacteristics,
            Binding.objects = Just (Seq.fromList [ObjectId.MkObjectId 7, ObjectId.MkObjectId 4])
          }
      )
      ( "{\"targets\":[{\"type\":\"ToPlayer\",\"value\":0}],"
          <> "\"amount\":3,\"modes\":[0,2,2],\"copy\":"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> ",\"objects\":[7,4]}"
      )
  -- A name-keyed map is a sorted array of entries so the render is
  -- deterministic. The two entries are inserted in DESCENDING slot-name order,
  -- so a trip that emitted the map's incidental traversal order rather than
  -- Map.toAscList fails this case.
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
      ( "[{\"slot\":\"a-slot\",\"binding\":{\"amount\":2}},"
          <> "{\"slot\":\"z-slot\",\"binding\":{\"amount\":1}}]"
      )
