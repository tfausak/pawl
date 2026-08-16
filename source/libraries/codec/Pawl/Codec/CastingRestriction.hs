module Pawl.Codec.CastingRestriction where

import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CastingRestriction as CastingRestriction

codec :: Codec.Codec CastingRestriction.CastingRestriction
codec =
  Arm.tagged
    [ Arm.payload "DuringPhase" Phase.codec CastingRestriction.DuringPhase (\x -> case x of CastingRestriction.DuringPhase y -> Just y; _ -> Nothing),
      Arm.nullary "AttackedThisStep" CastingRestriction.AttackedThisStep,
      Arm.nullary "AfterBlockersDeclared" CastingRestriction.AfterBlockersDeclared
    ]
