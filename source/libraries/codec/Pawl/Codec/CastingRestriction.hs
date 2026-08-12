module Pawl.Codec.CastingRestriction where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CastingRestriction as CastingRestriction

codec :: Codec.Codec CastingRestriction.CastingRestriction
codec =
  Arm.tagged
    encode
    [ Arm.payload "DuringPhase" Phase.codec CastingRestriction.DuringPhase,
      Arm.nullary "AttackedThisStep" CastingRestriction.AttackedThisStep
    ]
  where
    encode r = case r of
      CastingRestriction.DuringPhase p -> Common.tagged "DuringPhase" . Just $ Codec.encode Phase.codec p
      CastingRestriction.AttackedThisStep -> Common.nullary "AttackedThisStep"
