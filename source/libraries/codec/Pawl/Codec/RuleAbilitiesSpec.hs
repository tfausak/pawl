module Pawl.Codec.RuleAbilitiesSpec where

import qualified Data.Set as Set
import qualified Pawl.Codec.RuleAbilities as RuleAbilities
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackCostScope as AttackCostScope
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockCost as BlockCost
import qualified Pawl.Types.BlockPermission as BlockPermission
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.CrewRestriction as CrewRestriction
import qualified Pawl.Types.EntryRestriction as EntryRestriction
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.PerCreature as PerCreature
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RequirementArity as RequirementArity
import qualified Pawl.Types.RuleAbilities as RuleAbilities
import qualified Pawl.Types.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.UntapRestriction as UntapRestriction
import qualified Pawl.Types.Zone as Zone

-- | Every one of the twelve keys defaults to the empty list, so one fixture with
-- all twelve populated is what holds each key's spelling to
-- Pawl.Codec.Face's -- the two write the same wire shape and a copy snapshot
-- would otherwise round trip through a key no card ever writes.

-- | The values Pawl.Codec.FaceSpec's populated face carries, so a drift between
-- the two encodings shows up as a difference in this file.
testRuleAbilities :: RuleAbilities.RuleAbilities
testRuleAbilities =
  RuleAbilities.MkRuleAbilities
    { RuleAbilities.activationProhibitions = [ActivationProhibition.MkActivationProhibition Affected.Attached Nothing Nothing],
      RuleAbilities.attachRestrictions = [AttachRestriction.MkAttachRestriction Affected.Attached (Filter.HasSubtype Subtype.Aura)],
      RuleAbilities.attackCosts = [AttackCost.MkAttackCost Affected.Attached (PerCreature.Fixed (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [])) AttackCostScope.Controller],
      RuleAbilities.attackRequirements = [AttackRequirement.MkAttackRequirement Affected.Attached Nothing Nothing RequirementArity.EachSubject],
      RuleAbilities.blockCosts = [BlockCost.MkBlockCost Affected.Attached (PerCreature.Fixed (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 3])) []))],
      RuleAbilities.blockPermissions = [BlockPermission.MkBlockPermission Affected.Attached (Just (Quantity.Literal 1)) Nothing],
      RuleAbilities.blockRequirements = [BlockRequirement.MkBlockRequirement Nothing (Just Affected.Attached) Nothing RequirementArity.EachSubject],
      RuleAbilities.combatRestrictions = [CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless Affected.Attached Nothing Nothing)],
      RuleAbilities.counterRestrictions = [CounterRestriction.MkCounterRestriction Affected.Attached (Just CounterKind.MinusOneMinusOne)],
      RuleAbilities.crewRestrictions = [CrewRestriction.MkCrewRestriction Affected.Attached],
      RuleAbilities.entryRestrictions = [EntryRestriction.MkEntryRestriction Affected.Attached (Set.singleton Zone.Graveyard)],
      RuleAbilities.sacrificeRestrictions = [SacrificeRestriction.MkSacrificeRestriction Affected.Attached],
      RuleAbilities.untapRestrictions = [UntapRestriction.MkUntapRestriction Affected.Attached]
    }

testRuleAbilitiesJson :: String
testRuleAbilitiesJson =
  "{\"activationProhibitions\":[{\"affected\":{\"type\":\"Attached\"}}],"
    <> "\"attachRestrictions\":[{\"affected\":{\"type\":\"Attached\"},\"attachers\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Aura\"}}}],"
    <> "\"attackCosts\":[{\"subject\":{\"type\":\"Attached\"},\"perAttacker\":{\"type\":\"Fixed\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}},\"scope\":{\"type\":\"Controller\"}}],"
    <> "\"attackRequirements\":[{\"subject\":{\"type\":\"Attached\"}}],"
    <> "\"blockCosts\":[{\"subject\":{\"type\":\"Attached\"},\"perBlocker\":{\"type\":\"Fixed\",\"value\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]}}}],"
    <> "\"blockPermissions\":[{\"affected\":{\"type\":\"Attached\"},\"additional\":{\"type\":\"Literal\",\"value\":1}}],"
    <> "\"blockRequirements\":[{\"attacker\":{\"type\":\"Attached\"}}],"
    <> "\"combatRestrictions\":[{\"type\":\"CantAttack\",\"value\":{\"affected\":{\"type\":\"Attached\"}}}],"
    <> "\"counterRestrictions\":[{\"affected\":{\"type\":\"Attached\"},\"kind\":{\"type\":\"MinusOneMinusOne\"}}],"
    <> "\"crewRestrictions\":[{\"affected\":{\"type\":\"Attached\"}}],"
    <> "\"entryRestrictions\":[{\"affected\":{\"type\":\"Attached\"},\"origins\":[{\"type\":\"Graveyard\"}]}],"
    <> "\"sacrificeRestrictions\":[{\"affected\":{\"type\":\"Attached\"}}],"
    <> "\"untapRestrictions\":[{\"affected\":{\"type\":\"Attached\"}}]}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RuleAbilities" $ do
  Spec.it s "MkRuleAbilities, every one of the thirteen lists populated" $
    Common.assertCodec s RuleAbilities.codec testRuleAbilities testRuleAbilitiesJson
  Spec.it s "an empty bundle omits every key" $
    Common.assertCodec s RuleAbilities.codec mempty " {} "
