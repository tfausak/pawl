module Pawl.Codec.TargetSlotSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSlot as TargetSlot

-- Filter's own coverage lives in Pawl.Codec.FilterSpec, so these cases exercise
-- it only in its embedded position: a bare pool (Nothing filter, omitted key),
-- a filtered pool, and the Not IsSource conjunct carrying CR 601.2c's "another"
-- (#163). The count key is omitted for every one-target slot, which is what
-- keeps the corpus unrewritten.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetSlot" $ do
  Spec.it s "a required spec: bare pool" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.required Pool.Creatures Nothing)
      " {\"pool\":{\"type\":\"Creatures\"}} "
  Spec.it s "a required spec: filtered pool" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.required Pool.Permanents (Just (Filter.HasCardType CardType.Artifact)))
      " {\"pool\":{\"type\":\"Permanents\"},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Artifact\"}}} "
  Spec.it s "a required spec: \"another\" (Not IsSource)" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.required Pool.Permanents (Just (Filter.And [Filter.Not (Filter.HasCardType CardType.Land), Filter.Not Filter.IsSource])))
      " {\"pool\":{\"type\":\"Permanents\"},\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}} "
  -- CR 115.2 clause (a): a target slot over a graveyard, tagged with WHOSE.
  Spec.it s "a required spec: over a graveyard" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.required (Pool.CardsInGraveyard (GraveyardScope.Scoped PlayerScope.You)) (Just (Filter.HasCardType CardType.Creature)))
      " {\"pool\":{\"type\":\"CardsInGraveyard\",\"value\":{\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- CR 115.2 clause (a)'s other zone: no PlayerScope (CR 400.1's shared zone)
  -- and no Filter.
  Spec.it s "a required spec: over exile, scopeless and unfiltered" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.required Pool.CardsInExile Nothing)
      " {\"pool\":{\"type\":\"CardsInExile\"}} "
  -- CR 115.6's "up to one target", and CR 601.2c's larger count: the two cases
  -- that spend the count key.
  Spec.it s "an up-to-one spec" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.upTo 1 Pool.Creatures Nothing)
      " {\"pool\":{\"type\":\"Creatures\"},\"count\":{\"least\":0,\"most\":1}} "
  Spec.it s "an up-to-two spec" $
    Common.assertCodec
      s
      TargetSlot.codec
      (TargetSlot.upTo 2 Pool.Creatures Nothing)
      " {\"pool\":{\"type\":\"Creatures\"},\"count\":{\"least\":0,\"most\":2}} "
  -- The slot-keyed map is a JSON OBJECT keyed by the slot name (#1303). The two
  -- entries are inserted in DESCENDING slot-name order, so a trip that emitted
  -- the map's incidental traversal order rather than Map.toAscList fails this
  -- case -- Pawl.Json.Object is a list of pairs, so the order written is the
  -- order rendered.
  Spec.it s "codecMap keys by slot name, in ascending order" $
    Common.assertCodec
      s
      TargetSlot.codecMap
      ( Map.fromList
          [ (SlotName.MkSlotName (Text.pack "z-slot"), TargetSlot.required Pool.Players Nothing),
            (SlotName.MkSlotName (Text.pack "a-slot"), TargetSlot.required Pool.Creatures Nothing)
          ]
      )
      " {\"a-slot\":{\"pool\":{\"type\":\"Creatures\"}},\"z-slot\":{\"pool\":{\"type\":\"Players\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TargetSlot.codec
  Spec.it s "the map has a schema" $ Common.assertHasSchema s TargetSlot.codecMap
