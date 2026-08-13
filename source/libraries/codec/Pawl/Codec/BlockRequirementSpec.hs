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
  -- creature able to block must block.
  Spec.it s "MkBlockRequirement" $
    Common.assertCodec
      s
      BlockRequirement.codec
      (BlockRequirement.MkBlockRequirement Affected.Attached)
      """ {"attacker":{"type":"Attached"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s BlockRequirement.codec
