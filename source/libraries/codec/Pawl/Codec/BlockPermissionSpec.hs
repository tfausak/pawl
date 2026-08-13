{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BlockPermissionSpec where

import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.Compares as Compares
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
spec s = Spec.describe s "Pawl.Codec.BlockPermission" $ do
  -- Echo Circlet's shape (CR 303.4m): the equipped creature is the one that may
  -- block one creature more than CR 509.1a allows.
  Spec.it s "MkBlockPermission" $
    Common.assertCodec
      s
      BlockPermission.codec
      (BlockPermission.MkBlockPermission Affected.Attached (Just (Quantity.Literal 1)) Nothing)
      """ {"affected":{"type":"Attached"},"additional":{"type":"Literal","value":1}} """
  -- Palace Guard's: "any number of creatures", an explicit null rather than an
  -- absent key.
  Spec.it s "an unbounded permission" $
    Common.assertCodec
      s
      BlockPermission.codec
      (BlockPermission.MkBlockPermission (Affected.Matching Filter.IsSource) Nothing Nothing)
      """ {"affected":{"type":"Matching","value":{"type":"IsSource"}},"additional":null} """
  -- Entourage of Trest's: CR 604.2's "as long as you're the monarch" (CR 725.1).
  Spec.it s "a gated permission" $
    Common.assertCodec
      s
      BlockPermission.codec
      ( BlockPermission.MkBlockPermission
          (Affected.Matching Filter.IsSource)
          (Just (Quantity.Literal 1))
          (Just (Condition.Compares (Compares.MkCompares (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You)) Comparison.AtLeast (Quantity.Literal 1))))
      )
      """ {"affected":{"type":"Matching","value":{"type":"IsSource"}},"additional":{"type":"Literal","value":1},"while":{"type":"Compares","value":{"measured":{"type":"IsMonarch","value":{"type":"Relative","value":{"type":"You"}}},"comparison":{"type":"AtLeast"},"threshold":{"type":"Literal","value":1}}}} """
  -- Kemba's Legion's: the arity itself is counted (CR 301.5a), so "additional"
  -- holds a whole Quantity rather than a number.
  Spec.it s "a counted permission" $
    Common.assertCodec
      s
      BlockPermission.codec
      ( BlockPermission.MkBlockPermission
          (Affected.Matching Filter.IsSource)
          ( Just
              ( Quantity.Count
                  Count.MkCount
                    { Count.scope = Scope.InZone Zone.Battlefield PlayerRef.EachPlayer,
                      Count.filter = Filter.And [Filter.HasSubtype Subtype.Equipment, Filter.IsAttachedToSource],
                      Count.aggregation = Aggregation.Members
                    }
              )
          )
          Nothing
      )
      """ {"affected":{"type":"Matching","value":{"type":"IsSource"}},"additional":{"type":"Count","value":{"aggregation":{"type":"Members"},"filter":{"type":"And","value":[{"type":"HasSubtype","value":{"type":"Equipment"}},{"type":"IsAttachedToSource"}]},"scope":{"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]}}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockPermission.codec
