module Pawl.Codec.ConditionSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
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
spec s = Spec.describe s "Pawl.Codec.Condition" $ do
  -- Condition has exactly one constructor (see Pawl.Types.Condition's comment)
  -- and both sides are a full Quantity, so it is on the wire as a bare object
  -- keyed by the record's field names rather than as a tagged one. The measured
  -- side is 3 and the threshold 5 deliberately: the two are the same type, so
  -- only an asymmetric fixture would catch a codec that read them in the wrong
  -- order -- which is exactly what the positional array this replaced could get
  -- wrong, and what naming the fields makes unsayable.
  Spec.it s "MkCondition" $
    Common.assertJsonCodec
      s
      Condition.toJson
      Condition.fromJson
      (Condition.MkCondition (Quantity.Literal 3) Comparison.AtLeast (Quantity.Literal 5))
      "{\"measured\":{\"type\":\"Literal\",\"value\":3},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":5}}"
  -- Moved from Pawl.CodecSpec's "count + condition (M5.5 T2)" group: every
  -- Comparison, including both sides non-Count -- which the Count-on-the-left
  -- shape this type replaced could not say at all (Deathknell Berserker's "if
  -- its power was 3 or greater", CR 603.4). The Barbarian Outcast /
  -- Sarcomancy fixtures stay in Pawl.CodecIntegrationSpec: they are shared, by
  -- design, with Pawl.CardSpec and other test-suite specs Pawl.Codec cannot
  -- import.
  Spec.it s "round-trips at every comparison" $
    mapM_
      (\v -> Spec.assertEqWith s "preserved" (Condition.fromJson (Condition.toJson v)) (Right v))
      [ Condition.MkCondition (Quantity.Count zeroSwamps) Comparison.Exactly (Quantity.Literal 0),
        Condition.MkCondition (Quantity.Count zeroSwamps) Comparison.AtLeast (Quantity.Literal 3),
        Condition.MkCondition (Quantity.Count zeroSwamps) Comparison.AtMost (Quantity.Literal 1),
        Condition.MkCondition Quantity.Power Comparison.AtLeast (Quantity.Literal 3)
      ]

-- A count with every axis non-default, so a codec that drops one is caught.
-- Mirrors Pawl.CodecIntegrationSpec's fixture of the same name.
zeroSwamps :: Count.Count Quantity.Quantity
zeroSwamps =
  Count.MkCount
    (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
    (Filter.HasSubtype Subtype.Swamp)
    Aggregation.Objects
