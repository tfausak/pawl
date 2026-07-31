module Pawl.Types.Countering where

import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- CR 701.6a: one act of COUNTERING a spell -- "to counter a spell or ability
-- means to cancel it, removing it from the stack. It doesn't resolve and none of
-- its effects occur." Recorded by Pawl.Engine.Event.counter, the one funnel every
-- countering in the engine goes through.
--
-- A record rather than three positional fields on GameEvent.SpellCountered, the
-- Pawl.Types.DamageEvent and Pawl.Types.ZoneChange posture: two of the three are
-- ObjectIds, and `spell` and `source` are the two ends of the same act, so a
-- caller that swapped them would still typecheck.
--
-- Only the SPELL half of rule 701.6a is here. That rule covers countering an
-- ABILITY too, and nothing in this pool counters one -- no effect targets an
-- ability on the stack -- so a countered ability records nothing today (#43 is
-- the issue this type closes; the ability half is #486).
data Countering = MkCountering
  { -- CR 701.6a: the spell that was countered, as it was on the stack. The id is
    -- already dead by the time any reader sees this: countering removes it from
    -- the stack through Pawl.Engine.Event.changeZone, and CR 400.7 mints a fresh
    -- incarnation in the graveyard. Carried anyway, because it is WHAT HAPPENED
    -- -- the event otherwise says only that somebody countered something, and
    -- two counters in one batch would be indistinguishable entries.
    spell :: ObjectId,
    -- CR 113.7a: the spell or ability that did the countering -- "a spell or
    -- ability YOU CONTROL counters a spell" (Baral, Chief of Compliance) names
    -- this object. Also dead by the time the CR 117.5 trigger scan reads the
    -- log: a countering SPELL has gone to its owner's graveyard by CR 608.2n,
    -- and a countering ABILITY has ceased to exist by the same rule.
    source :: ObjectId,
    -- CR 109.5 / 603.3a: who controlled `source` AT THE MOMENT IT COUNTERED --
    -- the "you" in "a spell or ability you control".
    --
    -- Captured here rather than re-derived from `source` at match time, the
    -- deal-time rider posture Pawl.Types.DamageEvent's dealtByDeathtouch takes,
    -- and for that comment's reason: it may be unaskable later. CR 608.2n
    -- removes a resolving ABILITY from the stack and it "ceases to exist" --
    -- Pawl.Engine.Resolve.cease deletes the object without a zone change, so no
    -- CR 608.2h last known information is ever filed for it and there is
    -- nothing left to read a controller off. Rule 701.6a's own text is what
    -- makes that case real rather than hypothetical: a spell OR ABILITY does
    -- the countering.
    controller :: PlayerId
  }
  deriving (Eq, Ord, Show)
