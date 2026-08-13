module Pawl.Codec.ActivationRestriction where

import qualified Pawl.Codec.DuringPhase as DuringPhase
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction

-- | Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a
-- window; SorcerySpeed and AttackedThisStep still render as bare tags.
--
-- There is no tag for "no rider": CR 602.2's default is the EMPTY LIST on the
-- ability, which Pawl.Codec.ActivatedAbility writes by omitting the key.
codec :: Codec.Codec ActivationRestriction.ActivationRestriction
codec =
  Arm.tagged
    encode
    [ Arm.nullary "SorcerySpeed" ActivationRestriction.SorcerySpeed,
      Arm.payload "DuringPhase" DuringPhase.codec ActivationRestriction.DuringPhase,
      Arm.nullary "AttackedThisStep" ActivationRestriction.AttackedThisStep
    ]
  where
    encode t = case t of
      ActivationRestriction.SorcerySpeed -> Common.nullary "SorcerySpeed"
      ActivationRestriction.DuringPhase x -> Common.tagged "DuringPhase" . Just $ Codec.encode DuringPhase.codec x
      ActivationRestriction.AttackedThisStep -> Common.nullary "AttackedThisStep"
