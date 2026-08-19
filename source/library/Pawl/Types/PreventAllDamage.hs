module Pawl.Types.PreventAllDamage where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
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
    -- | CR 615.5's additional effect -- Brace for Impact's "for each 1 damage
    -- prevented this way, put a +1/+1 counter on that creature". Unlike CR
    -- 615.7's countdown shield this one has no amount to spend, so "the damage
    -- prevented this way" is per APPLICATION rather than a running total. Empty
    -- for a shield with no such clause, so the key is elided rather than written
    -- as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
