{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ConditionSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Condition" $ do
  -- Tagged, with the record's field names kept as the PAYLOAD's keys. The
  -- measured side is 3 and the threshold 5 deliberately: the two are the same
  -- type, so only an asymmetric fixture catches a codec that reads them in the
  -- wrong order.
  Spec.it s "Compares" $
    Common.assertCodec
      s
      Condition.codec
      (Condition.Compares (Compares.MkCompares (Quantity.Literal 3) Comparison.AtLeast (Quantity.Literal 5)))
      """ {"type":"Compares","value":{"measured":{"type":"Literal","value":3},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":5}}} """
  -- CR 702.100a's "and/or". Two disjuncts, and asymmetric ones, so a codec that
  -- dropped or reordered the list is caught.
  Spec.it s "Any" $
    Common.assertCodec
      s
      Condition.codec
      (Condition.Any [Condition.Compares (Compares.MkCompares Quantity.Power Comparison.AtLeast (Quantity.Literal 1)), Condition.Compares (Compares.MkCompares Quantity.Toughness Comparison.AtMost (Quantity.Literal 2))])
      """ {"type":"Any","value":[{"type":"Compares","value":{"measured":{"type":"Power"},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}}},{"type":"Compares","value":{"measured":{"type":"Toughness"},"comparison":{"type":"AtMost"},"threshold":{"type":"Literal","value":2}}}]} """
  -- The old untagged shapes are not conditions any more. A comparison's keys
  -- alone decoding would mean a card file written before #1304 kept working
  -- while writing something the schema does not describe.
  Spec.it s "rejects the untagged comparison shape" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"measured":{"type":"Power"},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}} """) >>= Codec.decode Condition.codec))
      "expected a decode failure"
  Spec.it s "rejects the untagged disjunction shape" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"any":[]} """) >>= Codec.decode Condition.codec))
      "expected a decode failure"
  -- Nesting, which is what makes Any a flat sibling arm rather than a wrapper.
  Spec.it s "Any nests" $
    Spec.assertEqWith
      s
      "preserved"
      (Codec.decode Condition.codec (Codec.encode Condition.codec (Condition.Any [Condition.Any []])))
      (Right (Condition.Any [Condition.Any []]))
  -- Every Comparison, including both sides non-Count -- the shape an intervening
  -- "if" needs (CR 603.4). The
  -- registry-backed fixtures stay in Pawl.CodecIntegrationSpec, shared with
  -- test-suite specs Pawl.Codec cannot import.
  Spec.it s "round-trips at every comparison" $
    mapM_
      (\v -> Spec.assertEqWith s "preserved" (Codec.decode Condition.codec (Codec.encode Condition.codec v)) (Right v))
      [ Condition.Compares (Compares.MkCompares (Quantity.Count zeroSwamps) Comparison.Exactly (Quantity.Literal 0)),
        Condition.Compares (Compares.MkCompares (Quantity.Count zeroSwamps) Comparison.AtLeast (Quantity.Literal 3)),
        Condition.Compares (Compares.MkCompares (Quantity.Count zeroSwamps) Comparison.AtMost (Quantity.Literal 1)),
        Condition.Compares (Compares.MkCompares Quantity.Power Comparison.AtLeast (Quantity.Literal 3))
      ]

-- A count with every axis non-default, so a codec that drops one is caught.
zeroSwamps :: Count.Count Quantity.Quantity
zeroSwamps =
  Count.MkCount
    (Scope.InZone (InZone.MkInZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent)))
    (Filter.HasSubtype Subtype.Swamp)
    Aggregation.Members
