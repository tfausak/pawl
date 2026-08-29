module Pawl.Types.PrintedReplacement where

import qualified Data.Set as Set
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Zone as Zone

-- | CR 604.2: one replacement effect a permanent's static ability creates, plus
-- the "as long as" clause gating it. Pawl.Types.Face lists these; the floating
-- twin a resolution installs is Pawl.Types.Replace, and the one the CR 616.1 loop
-- carries is Pawl.Types.ActiveReplacement.
--
-- A wrapper rather than a field on each Pawl.Types.ReplacementEffect arm, because
-- the clause gates the ABILITY: CR 604.2 states one rule for every continuous
-- effect a static ability creates, and nothing about it is particular to the event
-- class the effect intercepts.
--
-- Parametric in the EFFECT, passing Pawl.Types.ReplacementEffect's parameter
-- through for the reason Pawl.Types.DamageR gives.
data PrintedReplacement effect = MkPrintedReplacement
  { -- | The ability's "as long as" clause -- Jared Carthalion, True Heir's "while
    -- you're the monarch" -- or Nothing for one that functions from wherever CR
    -- 604.2 leaves it functioning, which is every other producer in the pool.
    --
    -- A SECOND gate, on top of the one CR 604.2 already states, exactly as
    -- Pawl.Types.StaticAbility.condition is: the rule keeps the effect active
    -- while the permanent is on the battlefield and has the ability, OR while the
    -- object with the ability stays in the zone rule 113.6 names for it, which
    -- for an emblem is the command zone (CR 113.6p / 114.4). Both limbs are
    -- Pawl.Engine.Projection.replacementsAffecting's walk, and this narrows what
    -- that walk gathers further.
    --
    -- The second limb's SCOPE is `functionsFrom` below, which this clause is
    -- asked on top of exactly as it is on the battlefield.
    --
    -- Asked when the event would happen, never latched: CR 604.1 makes a static
    -- ability "simply true", so there is no moment at which the clause is checked
    -- and stored. Pawl.Engine.Projection.replacementsOf asks it against
    -- Pawl.Engine.Projection.boardAsEntering, which is the live board outside an
    -- entry loop -- so the monarchy changing hands turns Jared's shield off with
    -- no trigger and no resolution in between -- and the live board minus every
    -- permanent that is materialized but not entered while one runs (CR 614.12a).
    --
    -- The twin on Pawl.Types.Replace reads the same way, and off the same board,
    -- for CR 614.1's reason rather than CR 604.1's: it rides the floating row a
    -- resolution installs and Pawl.Engine.Replacement.collect asks it as the
    -- event would happen. What differs is CR 109.5's "you" -- that row bakes its
    -- controller, this one reads it live off the battlefield.
    condition :: Maybe Condition.Condition,
    effect :: ReplacementEffect.ReplacementEffect effect,
    -- | CR 113.6b: the zones this row STATES its ability functions in -- Nexus of
    -- Fate's "would be put into a graveyard from anywhere" -- and empty for a row
    -- that states none, which is every other producer in the pool.
    --
    -- Structural rather than a Condition, for the reason
    -- Pawl.Types.StaticAbility.functionsFrom is: CR 113.6 decides which zone's
    -- walk gathers a row at all, BEFORE CR 604.2's clause is asked of anything, so
    -- a zone written as a condition could narrow a gather but never widen one.
    -- Empty leaves CR 113.6's own defaults standing -- the battlefield for a
    -- permanent, the command zone for an emblem (CR 113.6p / 114.4) -- and CR
    -- 113.6b's "only" is why a stated set REPLACES those defaults rather than
    -- adding to them.
    --
    -- The reader is Pawl.Engine.Projection.replacementsAffecting, which asks the
    -- set with its default folded in on the battlefield and in the command zone,
    -- and asks the bare set in the four zones no default reaches.
    functionsFrom :: Set.Set Zone.Zone,
    -- | The name another clause of the SAME card uses to refer to the static
    -- ability this replacement effect comes from, or Nothing for one nothing
    -- refers to, which is nearly all of them.
    --
    -- The same REFERENCE Pawl.Types.ActivatedAbility.name is, read by the same
    -- CR 613.1f removal (Modification.LoseNamedAbility) and carrying that field's
    -- reasoning whole: not an identity, so two abilities may share a name and a
    -- removal naming it takes both. Glittering Lion is the producer -- its "{3}:
    -- Until end of turn, this creature loses 'Prevent all damage that would be
    -- dealt to this creature.'" names a PREVENTION ability, which CR 614.1 /
    -- 615.1 make a static ability's continuous effect rather than an activated or
    -- triggered one.
    name :: Maybe AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
