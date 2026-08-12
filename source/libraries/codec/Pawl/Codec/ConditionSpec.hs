{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ConditionSpec where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Common as Common
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
  -- On the wire as a bare object keyed by the record's field names rather than
  -- a tagged one. The measured side is 3 and the threshold 5 deliberately: the
  -- two are the same type, so only an asymmetric fixture catches a codec that
  -- reads them in the wrong order.
  Spec.it s "Compares" $
    Common.assertJsonCodec
      s
      Condition.toJson
      Condition.fromJson
      (Condition.Compares (Quantity.Literal 3) Comparison.AtLeast (Quantity.Literal 5))
      """ {"measured":{"type":"Literal","value":3},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":5}} """
  -- CR 702.100a's "and/or", told from a comparison by its lone "any" key rather
  -- than by a tag. Two disjuncts, and asymmetric ones, so a codec that dropped
  -- or reordered the list is caught.
  Spec.it s "Any" $
    Common.assertJsonCodec
      s
      Condition.toJson
      Condition.fromJson
      (Condition.Any [Condition.Compares Quantity.Power Comparison.AtLeast (Quantity.Literal 1), Condition.Compares Quantity.Toughness Comparison.AtMost (Quantity.Literal 2)])
      """ {"any":[{"measured":{"type":"Power"},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}},{"measured":{"type":"Toughness"},"comparison":{"type":"AtMost"},"threshold":{"type":"Literal","value":2}}]} """
  -- Nesting, which is what makes Any a flat sibling arm rather than a wrapper.
  Spec.it s "Any nests" $
    Spec.assertEqWith
      s
      "preserved"
      (Condition.fromJson (Condition.toJson (Condition.Any [Condition.Any []])))
      (Right (Condition.Any [Condition.Any []]))
  -- Every Comparison, including both sides non-Count -- the shape an intervening
  -- "if" needs (CR 603.4). The
  -- registry-backed fixtures stay in Pawl.CodecIntegrationSpec, shared with
  -- test-suite specs Pawl.Codec cannot import.
  Spec.it s "round-trips at every comparison" $
    mapM_
      (\v -> Spec.assertEqWith s "preserved" (Condition.fromJson (Condition.toJson v)) (Right v))
      [ Condition.Compares (Quantity.Count zeroSwamps) Comparison.Exactly (Quantity.Literal 0),
        Condition.Compares (Quantity.Count zeroSwamps) Comparison.AtLeast (Quantity.Literal 3),
        Condition.Compares (Quantity.Count zeroSwamps) Comparison.AtMost (Quantity.Literal 1),
        Condition.Compares Quantity.Power Comparison.AtLeast (Quantity.Literal 3)
      ]

-- A count with every axis non-default, so a codec that drops one is caught.
zeroSwamps :: Count.Count Quantity.Quantity
zeroSwamps =
  Count.MkCount
    (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
    (Filter.HasSubtype Subtype.Swamp)
    Aggregation.Objects
