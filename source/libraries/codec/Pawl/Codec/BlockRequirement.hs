{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BlockRequirement where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.RequirementArity as RequirementArity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.RequirementArity as RequirementArity

-- | Both of CR 509.1c's axes default to absent, which is "unrestricted on that
-- axis" -- so Lure's `attacker` alone and Razorgrass Screen's `subject` alone are
-- each a whole requirement, and neither card writes the other key. "while" is a
-- third optional field, spelled exactly as Pawl.Codec.AttackRequirement and
-- Pawl.Codec.BlockPermission spell the same CR 604.2 clause and as
-- Pawl.Codec.CombatRestriction spells its opposite, "unless". "arity" is a
-- fourth, defaulting to CR 509.1c's own counting -- one requirement per creature
-- -- so only a card meaning the group reading writes it.
codec :: Codec.Codec BlockRequirement.BlockRequirement
codec = Fields.object $ do
  subject <- Fields.defaulted "subject" Nothing (Common.maybe Affected.codec) BlockRequirement.subject
  attacker <- Fields.defaulted "attacker" Nothing (Common.maybe Affected.codec) BlockRequirement.attacker
  while <- Fields.defaulted "while" Nothing (Common.maybe Condition.codec) BlockRequirement.while
  arity <- Fields.defaulted "arity" RequirementArity.EachSubject RequirementArity.codec BlockRequirement.arity
  pure
    BlockRequirement.MkBlockRequirement
      { BlockRequirement.subject = subject,
        BlockRequirement.attacker = attacker,
        BlockRequirement.while = while,
        BlockRequirement.arity = arity
      }
