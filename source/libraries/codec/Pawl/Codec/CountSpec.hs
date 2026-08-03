{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CountSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Count as Count
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- Instantiated at Integer via Common.integer/Common.asInteger, the simplest
-- element codec available -- MkCount is parametric in the per-object quantity
-- its Aggregation reads (see Pawl.Types.Count's haddock), and none of the three
-- cases below exercises Aggregation.Greatest, the one arm that would need it.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Count" $ do
  Spec.it s "MkCount, in a zone" $
    Common.assertJsonCodec
      s
      (Count.toJson Common.integer)
      (Count.fromJson Common.asInteger)
      ( Count.MkCount
          (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
          (Filter.And [Filter.HasSubtype Subtype.Swamp, Filter.ControlledBy PlayerRelation.You])
          Aggregation.Objects
      )
      """ {"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]},"filter":{"type":"And","value":[{"type":"HasSubtype","value":{"type":"Swamp"}},{"type":"ControlledBy","value":{"type":"You"}}]},"aggregation":{"type":"Objects"}} """
  -- CR 608.2i's look-back-in-time domain.
  Spec.it s "MkCount, scoped to the event history" $
    Common.assertJsonCodec
      s
      (Count.toJson Common.integer)
      (Count.fromJson Common.asInteger)
      ( Count.MkCount
          (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
          (Filter.HasCardType CardType.Creature)
          Aggregation.DistinctCardTypes
      )
      """ {"scope":{"type":"InHistory","value":{"type":"MovedBetween","value":[{"type":"Battlefield"},{"type":"Graveyard"}]}},"filter":{"type":"HasCardType","value":{"type":"Creature"}},"aggregation":{"type":"DistinctCardTypes"}} """
  Spec.it s "MkCount, scoped to a slot" $
    Common.assertJsonCodec
      s
      (Count.toJson Common.integer)
      (Count.fromJson Common.asInteger)
      ( Count.MkCount
          (Scope.InZone Zone.Hand (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
          (Filter.And [])
          Aggregation.Objects
      )
      """ {"scope":{"type":"InZone","value":[{"type":"Hand"},{"type":"InSlot","value":"target"}]},"filter":{"type":"And","value":[]},"aggregation":{"type":"Objects"}} """
