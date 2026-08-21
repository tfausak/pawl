{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RequireAttack where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RequireAttack as RequireAttack

-- | A bare object keyed by the record's field names, Pawl.Codec.RequireBlock's
-- shape on the other side of the combat phase. The tag that picks it is written
-- by Pawl.Codec.Effect's RequireAttack arm.
codec :: Codec.Codec RequireAttack.RequireAttack
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec RequireAttack.duration
  attacker <- Fields.required "attacker" ObjectRef.codec RequireAttack.attacker
  defender <- Fields.required "defender" PlayerRef.codec RequireAttack.defender
  pure
    RequireAttack.MkRequireAttack
      { RequireAttack.duration = duration,
        RequireAttack.attacker = attacker,
        RequireAttack.defender = defender
      }
