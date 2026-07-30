module Pawl.Type.ReplacementEffect where

import Pawl.Type.CounterPattern (CounterPattern)
import Pawl.Type.DamagePattern (DamagePattern)
import Pawl.Type.DamageRewrite (DamageRewrite)
import Pawl.Type.DestructionRewrite (DestructionRewrite)
import Pawl.Type.EntryRewrite (EntryRewrite)
import Pawl.Type.PhasePattern (PhasePattern)
import Pawl.Type.Scaling (Scaling)
import Pawl.Type.TokenPattern (TokenPattern)
import Pawl.Type.Zone (Zone)
import Pawl.Type.ZoneChangePattern (ZoneChangePattern)

-- CR 614.1a: a replacement effect, classified by the EVENT CLASS it intercepts
-- and the REWRITE SHAPE it applies. One arm per replaceable event class -- the
-- arm count tracks the ~40 classes the comprehensive rules define, never the card
-- pool. Rest in Peace is DATA (`ZoneChangeR (MkZoneChangePattern Graveyard
-- Anyones AnyObject) Exile`), not a constructor; so is Fog, so is regeneration,
-- so is Hardened Scales, and so is rule 702.34a's flashback exile
-- (Pawl.Keyword.flashbackExile, which differs from Rest in Peace only in its
-- pattern). The scenario the first invariant forbids --
-- `case effect of RedirectZoneChange Graveyard Exile -> restInPeace` -- is no
-- longer expressible.
--
-- A (effect, event) pair whose arms disagree simply does not apply, so the type
-- rules out "redirect a damage event" without a validity pass.
--
-- EntryR and DestructionR carry NO pattern: both are self-only in the pool today
-- (CR 614.1c's "[this permanent] enters as"; CR 201.5/201.5c make "regenerate
-- this creature" name the ability's own source). CR 614.1d's other-objects form
-- ("[Objects] enter the battlefield ...", Essence of the Wild) has no producer,
-- so the field appears when a card needs it rather than as speculative structure.
--
-- PhaseR carries a pattern but NO rewrite, and that is the rule rather than an
-- omission: CR 614.1b says skips "indicate what events, steps, phases, or turns
-- will be replaced with NOTHING", and CR 614.10 spells the same thing out --
-- "'skip [something]' is the same as 'instead of doing [something], do
-- nothing'". There is only one thing a skip can do, so there is nothing for a
-- PhaseRewrite to choose between. (CR 614.10b's "skip, then take another
-- action" is a separate ability alongside the skip, not a different rewrite of
-- it, and has no producer.)
--
-- Nor does the pattern say "next": CR 614.10a's once-and-gone skip (Fatigue) is
-- Uses.Once on the ActiveReplacement holding it, which is what lets a permanent's
-- unbounded skip (Eon Hub) and a resolution's single-occurrence one share one
-- constructor. A "next" field here would say the same thing in a second place.
--
-- The sole rules-casing site is Pawl.Replacement (CR 616.1's loop). Pawl.Codec
-- also cases on every constructor, but only as the JSON data boundary.
data ReplacementEffect
  = ZoneChangeR ZoneChangePattern Zone
  | EntryR EntryRewrite
  | DamageR DamagePattern DamageRewrite
  | DestructionR DestructionRewrite
  | CounterR CounterPattern Scaling
  | TokenR TokenPattern Scaling
  | PhaseR PhasePattern
  deriving (Eq, Ord, Show)
