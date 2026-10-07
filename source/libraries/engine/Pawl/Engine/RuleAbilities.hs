-- | The names rule abilities carry, read below the projection so that both
-- Pawl.Engine.Projection's layer fold and the restriction gatherers
-- (Pawl.Engine.CombatRestriction and its siblings) can ask them.
module Pawl.Engine.RuleAbilities where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.AffectedUnless as AffectedUnless
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer
import qualified Pawl.Types.CantBeBlockedBy as CantBeBlockedBy
import qualified Pawl.Types.CantBlockCreatures as CantBlockCreatures
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.RuleAbilities as RuleAbilities

-- CR 116.2d: the name the source's face gives the ability stating this
-- restriction, so a payment can say WHICH effect it ignores, and a CR 613.1f
-- removal which one it removes. Read off any arm that names a SUBJECT, because
-- the clause is the same one on all of them -- Volrath's Curse's single sentence
-- names both halves of "can't attack or block" alike, and one payment covers
-- both.
--
-- The two BOUNDING arms answer Nothing and carry no such field: CR 116.2d's
-- offer goes to a player, every printed producer's sentence derives that player
-- from the object the effect is aimed at ("that creature's controller"), and a
-- bound names no creature for anyone to be the controller of. So a named bound
-- would be a name nothing could ever pay to ignore.
nameOf :: CombatRestriction.CombatRestriction -> Maybe AbilityName.AbilityName
nameOf cr = case cr of
  CombatRestriction.CantAttack (AffectedUnless.MkAffectedUnless _ _ n) -> n
  CombatRestriction.CantBlock (AffectedUnless.MkAffectedUnless _ _ n) -> n
  CombatRestriction.CantBeBlockedBy (CantBeBlockedBy.MkCantBeBlockedBy _ _ _ n) -> n
  CombatRestriction.CantBlockCreatures (CantBlockCreatures.MkCantBlockCreatures _ _ _ n) -> n
  CombatRestriction.CantAttackPlayer (CantAttackPlayer.MkCantAttackPlayer _ _ _ _ n) -> n
  CombatRestriction.CantAttackAlone (AffectedUnless.MkAffectedUnless _ _ n) -> n
  CombatRestriction.CantAttackMoreThan {} -> Nothing
  CombatRestriction.CantBlockMoreThan {} -> Nothing

-- CR 613.1f: the rows whose name `keep` admits. A family whose rows carry no name
-- asks `keep Nothing` for all of them. A record CONSTRUCTION rather than an
-- update, so a new family fails to compile here instead of passing every row.
keepNamed :: (Maybe AbilityName.AbilityName -> Bool) -> RuleAbilities.RuleAbilities -> RuleAbilities.RuleAbilities
keepNamed keep rules =
  let nameless rows = if keep Nothing then rows else []
   in RuleAbilities.MkRuleAbilities
        { RuleAbilities.activationProhibitions = filter (keep . ActivationProhibition.name) (RuleAbilities.activationProhibitions rules),
          RuleAbilities.attachRestrictions = nameless (RuleAbilities.attachRestrictions rules),
          RuleAbilities.attackCosts = nameless (RuleAbilities.attackCosts rules),
          RuleAbilities.attackPermissions = nameless (RuleAbilities.attackPermissions rules),
          RuleAbilities.attackRequirements = nameless (RuleAbilities.attackRequirements rules),
          RuleAbilities.blockCosts = nameless (RuleAbilities.blockCosts rules),
          RuleAbilities.blockPermissions = nameless (RuleAbilities.blockPermissions rules),
          RuleAbilities.blockRequirements = nameless (RuleAbilities.blockRequirements rules),
          RuleAbilities.combatRestrictions = filter (keep . nameOf) (RuleAbilities.combatRestrictions rules),
          RuleAbilities.counterRestrictions = nameless (RuleAbilities.counterRestrictions rules),
          RuleAbilities.crewRestrictions = nameless (RuleAbilities.crewRestrictions rules),
          RuleAbilities.entryRestrictions = nameless (RuleAbilities.entryRestrictions rules),
          RuleAbilities.sacrificeRestrictions = nameless (RuleAbilities.sacrificeRestrictions rules),
          RuleAbilities.untapRestrictions = nameless (RuleAbilities.untapRestrictions rules)
        }
