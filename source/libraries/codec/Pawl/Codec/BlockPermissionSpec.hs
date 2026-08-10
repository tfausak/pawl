{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BlockPermissionSpec where

import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockPermission" $ do
  -- Echo Circlet's shape (CR 303.4m): the equipped creature is the one that may
  -- block one creature more than CR 509.1a allows.
  Spec.it s "MkBlockPermission" $
    Common.assertJsonCodec
      s
      BlockPermission.toJson
      BlockPermission.fromJson
      (BlockPermission.MkBlockPermission Affected.Attached (Just 1) Nothing)
      """ {"affected":{"type":"Attached"},"additional":1} """
  -- Palace Guard's: "any number of creatures", an explicit null rather than an
  -- absent key.
  Spec.it s "an unbounded permission" $
    Common.assertJsonCodec
      s
      BlockPermission.toJson
      BlockPermission.fromJson
      (BlockPermission.MkBlockPermission (Affected.Matching Filter.IsSource) Nothing Nothing)
      """ {"affected":{"type":"Matching","value":{"type":"IsSource"}},"additional":null} """
  -- Entourage of Trest's: CR 604.2's "as long as you're the monarch" (CR 725.1).
  Spec.it s "a gated permission" $
    Common.assertJsonCodec
      s
      BlockPermission.toJson
      BlockPermission.fromJson
      ( BlockPermission.MkBlockPermission
          (Affected.Matching Filter.IsSource)
          (Just 1)
          (Just (Condition.MkCondition (Quantity.IsMonarch (PlayerRef.Relative PlayerRelation.You)) Comparison.AtLeast (Quantity.Literal 1)))
      )
      """ {"affected":{"type":"Matching","value":{"type":"IsSource"}},"additional":1,"while":{"comparison":{"type":"AtLeast"},"measured":{"type":"IsMonarch","value":{"type":"Relative","value":{"type":"You"}}},"threshold":{"type":"Literal","value":1}}} """
