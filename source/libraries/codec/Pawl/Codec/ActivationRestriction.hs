module Pawl.Codec.ActivationRestriction where

import qualified Pawl.Codec.DuringPhase as DuringPhase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a
-- window; SorcerySpeed and AttackedThisStep still render as bare tags.
--
-- There is no tag for "no rider": CR 602.2's default is the EMPTY LIST on the
-- ability, which Pawl.Codec.ActivatedAbility writes by omitting the key.
codec :: Codec.Codec ActivationRestriction.ActivationRestriction
codec =
  Arm.tagged
    [ Arm.nullary "SorcerySpeed" ActivationRestriction.SorcerySpeed,
      Arm.payload "DuringPhase" DuringPhase.codec ActivationRestriction.DuringPhase (\x -> case x of ActivationRestriction.DuringPhase y -> Just y; _ -> Nothing),
      Arm.nullary "AttackedThisStep" ActivationRestriction.AttackedThisStep
    ]
