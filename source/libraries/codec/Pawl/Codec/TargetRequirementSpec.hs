{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TargetRequirementSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TargetRequirement as TargetRequirement
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TargetRequirement as TargetRequirement

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TargetRequirement" $ do
  Spec.it s "Required" $
    Common.assertJsonCodec
      s
      TargetRequirement.toJson
      TargetRequirement.fromJson
      TargetRequirement.Required
      """ {"type":"Required"} """
  Spec.it s "UpToOne" $
    Common.assertJsonCodec
      s
      TargetRequirement.toJson
      TargetRequirement.fromJson
      TargetRequirement.UpToOne
      """ {"type":"UpToOne"} """
