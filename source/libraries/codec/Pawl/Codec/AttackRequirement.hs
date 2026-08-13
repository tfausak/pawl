{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackRequirement where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackRequirement as AttackRequirement

-- | The key is "subject" and not "attacker": CR 508.1d's requirement names the
-- creatures REQUIRED to attack, where CR 509.1c's names the attacker to be
-- blocked. Same shape, opposite axis (Pawl.Types.AttackRequirement).
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec AttackRequirement.AttackRequirement
codec = Fields.object $ do
  subject <- Fields.required "subject" Affected.codec AttackRequirement.subject
  pure AttackRequirement.MkAttackRequirement {AttackRequirement.subject = subject}
