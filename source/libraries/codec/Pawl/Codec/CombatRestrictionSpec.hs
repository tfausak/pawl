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
  -- CR 508.1c's first clause: unconditional, so the "unless" key is absent
  -- rather than null.
  Spec.it s "CantAttack carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttack Affected.Attached Nothing)
      """ {"type":"CantAttack","value":{"affected":{"type":"Attached"}}} """
  -- CR 509.1b, the blocking counterpart.
  Spec.it s "CantBlock carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantBlock Affected.Attached Nothing)
      """ {"type":"CantBlock","value":{"affected":{"type":"Attached"}}} """
  -- CR 508.1c together with CR 506.5, the SET-SHAPED arm: the same payload as
  -- the two above, so the tag is the only thing that tells a reader this one is
  -- answered against a whole declaration.
  Spec.it s "CantAttackAlone carries its Affected" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttackAlone Affected.Attached Nothing)
      """ {"type":"CantAttackAlone","value":{"affected":{"type":"Attached"}}} """
  -- The SIZE-BOUNDING arms, whose payload spells "limit" where the three above
  -- spell "affected". They are here as much for the DECODER as for the shape:
  -- Pawl.Codec.CombatRestriction.fromJson dispatches on a string, so an arm
  -- missing there compiles and fails only when a card file is loaded, and these
  -- two cases are what turn that into a test failure.
  Spec.it s "CantAttackMoreThan carries its limit" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantAttackMoreThan 1 Nothing)
      """ {"type":"CantAttackMoreThan","value":{"limit":1}} """
  -- CR 509.1b, the blocking counterpart. A DIFFERENT limit from the one above,
  -- so a codec that crossed the two arms' payloads cannot pass both.
  Spec.it s "CantBlockMoreThan carries its limit" $
    Common.assertJsonCodec
      s
      CombatRestriction.toJson
      CombatRestriction.fromJson
      (CombatRestriction.CantBlockMoreThan 2 Nothing)
      """ {"type":"CantBlockMoreThan","value":{"limit":2}} """
  -- CR 508.1c's second clause: the gated form.
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

-- "you control a Giant", without the `Not IsSource` conjunct that would make it
-- "another Giant" -- dropped only to keep the expected string readable, since
-- Pawl.Codec.FilterSpec covers that conjunct.
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
