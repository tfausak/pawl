{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackCost where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackCost as AttackCost

-- | "subject" is Pawl.Codec.AttackRequirement's key and names the same axis:
-- the creatures the effect is ABOUT, never Pawl.Codec.BlockRequirement's
-- "attacker", which is the object CR 509.1c's requirements carry instead.
-- "perAttacker" is one attacker's share, not the card's whole cost -- a "for
-- each" repeats it per taxed attacker before CR 508.1h totals the declaration.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec AttackCost.AttackCost
codec = Fields.object $ do
  subject <- Fields.required "subject" Affected.codec AttackCost.subject
  perAttacker <- Fields.required "perAttacker" ManaCost.codec AttackCost.perAttacker
  pure
    AttackCost.MkAttackCost
      { AttackCost.subject = subject,
        AttackCost.perAttacker = perAttacker
      }
