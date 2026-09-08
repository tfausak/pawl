{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RuleAbilities where

import qualified Pawl.Codec.ActivationProhibition as ActivationProhibition
import qualified Pawl.Codec.AttachRestriction as AttachRestriction
import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.Codec.BlockCost as BlockCost
import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.CounterRestriction as CounterRestriction
import qualified Pawl.Codec.CrewRestriction as CrewRestriction
import qualified Pawl.Codec.EntryRestriction as EntryRestriction
import qualified Pawl.Codec.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Codec.UntapRestriction as UntapRestriction
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RuleAbilities as RuleAbilities

-- | The twelve field names Pawl.Codec.Face already writes, so a copy snapshot's
-- bundle round-trips through the same spellings a card's own face does.
codec :: Codec.Codec RuleAbilities.RuleAbilities
codec = Fields.object $ do
  activationProhibitions <- Fields.defaulted "activationProhibitions" [] (Common.list ActivationProhibition.codec) RuleAbilities.activationProhibitions
  attachRestrictions <- Fields.defaulted "attachRestrictions" [] (Common.list AttachRestriction.codec) RuleAbilities.attachRestrictions
  attackCosts <- Fields.defaulted "attackCosts" [] (Common.list AttackCost.codec) RuleAbilities.attackCosts
  attackRequirements <- Fields.defaulted "attackRequirements" [] (Common.list AttackRequirement.codec) RuleAbilities.attackRequirements
  blockCosts <- Fields.defaulted "blockCosts" [] (Common.list BlockCost.codec) RuleAbilities.blockCosts
  blockPermissions <- Fields.defaulted "blockPermissions" [] (Common.list BlockPermission.codec) RuleAbilities.blockPermissions
  blockRequirements <- Fields.defaulted "blockRequirements" [] (Common.list BlockRequirement.codec) RuleAbilities.blockRequirements
  combatRestrictions <- Fields.defaulted "combatRestrictions" [] (Common.list CombatRestriction.codec) RuleAbilities.combatRestrictions
  counterRestrictions <- Fields.defaulted "counterRestrictions" [] (Common.list CounterRestriction.codec) RuleAbilities.counterRestrictions
  crewRestrictions <- Fields.defaulted "crewRestrictions" [] (Common.list CrewRestriction.codec) RuleAbilities.crewRestrictions
  entryRestrictions <- Fields.defaulted "entryRestrictions" [] (Common.list EntryRestriction.codec) RuleAbilities.entryRestrictions
  sacrificeRestrictions <- Fields.defaulted "sacrificeRestrictions" [] (Common.list SacrificeRestriction.codec) RuleAbilities.sacrificeRestrictions
  untapRestrictions <- Fields.defaulted "untapRestrictions" [] (Common.list UntapRestriction.codec) RuleAbilities.untapRestrictions
  pure
    RuleAbilities.MkRuleAbilities
      { RuleAbilities.activationProhibitions = activationProhibitions,
        RuleAbilities.attachRestrictions = attachRestrictions,
        RuleAbilities.attackCosts = attackCosts,
        RuleAbilities.attackRequirements = attackRequirements,
        RuleAbilities.blockCosts = blockCosts,
        RuleAbilities.blockPermissions = blockPermissions,
        RuleAbilities.blockRequirements = blockRequirements,
        RuleAbilities.combatRestrictions = combatRestrictions,
        RuleAbilities.counterRestrictions = counterRestrictions,
        RuleAbilities.crewRestrictions = crewRestrictions,
        RuleAbilities.entryRestrictions = entryRestrictions,
        RuleAbilities.sacrificeRestrictions = sacrificeRestrictions,
        RuleAbilities.untapRestrictions = untapRestrictions
      }
