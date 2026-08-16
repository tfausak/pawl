{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BlockRequirementSpec where

import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BlockRequirement as BlockRequirement

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BlockRequirement" $ do
  -- Lure's shape (CR 303.4m): the enchanted creature IS the attacker every
  -- creature able to block must block, so the subject axis is absent.
  Spec.it s "MkBlockRequirement" $
    Common.assertCodec
      s
      BlockRequirement.codec
      (BlockRequirement.MkBlockRequirement Nothing (Just Affected.Attached))
      """ {"attacker":{"type":"Attached"}} """
  -- Razorgrass Screen's shape: the requirement names its own source and no
  -- attacker, so the object axis is the absent one.
  Spec.it s "MkBlockRequirement with no attacker" $
    Common.assertCodec
      s
      BlockRequirement.codec
      (BlockRequirement.MkBlockRequirement (Just Affected.Attached) Nothing)
      """ {"subject":{"type":"Attached"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockRequirement.codec
