{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CombatRestrictionSpec where

import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CombatRestriction" $ do
  -- CR 508.1c: Pacifism's first half, "Enchanted creature can't attack" -- the
  -- FIRST clause of the parenthetical, so the "unless" key is absent rather than
  -- null.
  Spec.it s "CantAttack carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttack Affected.Attached Nothing)
      """ {"type":"CantAttack","value":{"affected":{"type":"Attached"}}} """
  -- CR 509.1b: Pacifism's second half, "... or block".
  Spec.it s "CantBlock carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantBlock Affected.Attached Nothing)
      """ {"type":"CantBlock","value":{"affected":{"type":"Attached"}}} """
  -- CR 508.1c's SECOND clause, "or that it can't attack unless some condition is
  -- met" -- Blind-Spot Giant's "unless you control another Giant", in miniature.
  Spec.it s "CantAttack carries its condition" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttack Affected.Attached (Just anotherGiant))
      ( "{\"type\":\"CantAttack\",\"value\":{\"affected\":{\"type\":\"Attached\"},\"unless\":"
          <> "{\"measured\":{\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InZone\",\"value\":[{\"type\":\"Battlefield\"},{\"type\":\"EachPlayer\"}]},"
          <> "\"filter\":{\"type\":\"And\",\"value\":[{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Giant\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}]},"
          <> "\"aggregation\":{\"type\":\"Objects\"}}},"
          <> "\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":1}}}}"
      )
  -- CR 509.1b's second clause is the same sentence with "block" in place of
  -- "attack", so the gate rides the other arm unchanged.
  Spec.it s "CantBlock carries its condition" $
    Spec.assertEqWith
      s
      "preserved"
      (CombatRestriction.fromJson (CombatRestriction.toJson (CombatRestriction.CantBlock Affected.Attached (Just anotherGiant))))
      (Right (CombatRestriction.CantBlock Affected.Attached (Just anotherGiant)))

-- "you control a Giant" -- Blind-Spot Giant's gate minus the `Not IsSource`
-- conjunct that makes it "ANOTHER Giant", which is dropped only to keep the
-- expected string readable: the codec writes one Filter like any other, and
-- Pawl.Codec.FilterSpec is where that conjunct is covered.
anotherGiant :: Condition.Condition
anotherGiant =
  Condition.MkCondition
    ( Quantity.Count
        ( Count.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.And [Filter.HasSubtype Subtype.Giant, Filter.ControlledBy PlayerRelation.You])
            Aggregation.Objects
        )
    )
    Comparison.AtLeast
    (Quantity.Literal 1)
