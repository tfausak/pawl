module Pawl.Types.PrintedReplacement where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

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
    -- you're the monarch" -- or Nothing for one that functions whenever its
    -- permanent is on the battlefield, which is every other producer in the pool.
    --
    -- A SECOND gate, on top of the one CR 604.2 already states, exactly as
    -- Pawl.Types.StaticAbility.condition is: the rule keeps the effect active
    -- while the permanent is on the battlefield and has the ability, which
    -- Pawl.Engine.Projection.replacementsAffecting applies by walking the
    -- battlefield, and this narrows it further.
    --
    -- Asked when the event would happen, never latched: CR 604.1 makes a static
    -- ability "simply true", so there is no moment at which the clause is checked
    -- and stored. Pawl.Engine.Projection.replacementsOf asks it against the live
    -- board, which is why the monarchy changing hands turns Jared's shield off
    -- with no trigger and no resolution in between.
    --
    -- The twin on Pawl.Types.Replace reads the same way, for CR 614.1's reason
    -- rather than CR 604.1's: it rides the floating row a resolution installs and
    -- Pawl.Engine.Replacement.collect asks it as the event would happen. What
    -- differs is CR 109.5's "you" -- that row bakes its controller, this one
    -- reads it live off the battlefield.
    condition :: Maybe Condition.Condition,
    effect :: ReplacementEffect.ReplacementEffect effect
  }
  deriving (Eq, Ord, Show)
