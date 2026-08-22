module Pawl.Types.PreventAllDamage where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 615.1 / 615.3's UNBOUNDED prevention shield: over whom, of what kind,
-- for how long, and CR 615.5's additional effect riding it.

-- Pawl.Types.PreventNextDamage with the Quantity removed, and its own type
-- rather than Pawl.Types.DurationRef because both the kind and the rider are
-- fields GainControl -- that payload's other sharer -- does not want. That is
-- DurationRef's own instruction.
--
-- Parametric in the EFFECT rather than in the card, for
-- Pawl.Types.PreventNextDamage's reason: Pawl.Types.Effect holds this record and
-- this record holds effects, so naming Effect here would need an hs-boot file.
-- The parameter is instantiated at `Effect card` where the arm is declared.
data PreventAllDamage effect = MkPreventAllDamage
  { duration :: Duration.Duration,
    -- | PRINTED, not assumed -- Inkshield says "all COMBAT damage", where
    -- Selfless Squire says only "damage". Nothing is a shield naming no kind,
    -- taking combat and noncombat alike, and is elided rather than written null.
    kind :: Maybe DamageKind.DamageKind,
    ref :: ObjectRef.ObjectRef,
    -- | Which SIDE of the damage event the objects @ref@ names sit on -- the
    -- recipients (Inkshield, Selfless Squire) or the SOURCE (Dovin, Hand of
    -- Control's "and dealt by target permanent"). DealtTo for every shield that
    -- names only a recipient, so the key is elided rather than written on all
    -- but the by-direction.
    direction :: DamageDirection.DamageDirection,
    -- | CR 609.7a's "by a source of your choice" (Auriok Replica), as the
    -- PROPERTIES the chosen source must have, exactly as
    -- Pawl.Types.PreventNextDamage carries it on the countdown shield. Nothing is
    -- a shield naming no source at all (Inkshield), which watches every source;
    -- `Just` makes the shield's controller choose ONE source when the effect is
    -- created, and Pawl.Engine.Resolve bakes that id into
    -- Pawl.Types.DamagePattern.whichSource.
    --
    -- The Filter is BOTH halves of CR 615.9: it narrows the candidates offered,
    -- and it is written into DamagePattern.whatSource so CR 609.7b's recheck
    -- happens at the damage event rather than at the choice. Auriok Replica says
    -- only "a source", so its Filter is the trivial `And []` -- present, not
    -- absent, because "any source of your choice" is still a choice.
    --
    -- A DealtBy @direction@ beside a `Just` here is a contradiction -- that
    -- direction already fills the source half of the row off @ref@ (Dovin, Hand
    -- of Control) -- and Pawl.Engine.Resolve resolves it by asking CR 609.7a's
    -- choice only on the DealtTo side.
    chosenSource :: Maybe (Filter.Filter Keyword.Keyword),
    -- | CR 615.5's additional effect -- Brace for Impact's "for each 1 damage
    -- prevented this way, put a +1/+1 counter on that creature". Unlike CR
    -- 615.7's countdown shield this one has no amount to spend, so "the damage
    -- prevented this way" is per APPLICATION rather than a running total. Empty
    -- for a shield with no such clause, so the key is elided rather than written
    -- as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
