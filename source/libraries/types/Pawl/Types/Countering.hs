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
-- ABILITY too, and nothing in this pool counters one: CR 113.9's first sentence
-- says an ability on the stack "can't be countered by anything that counters
-- only spells", and Pawl.Types.Pool has no constructor admitting one as a target
-- either -- so a countered ability records nothing today (#486).
data Countering = MkCountering
  { -- CR 701.6a: the spell that was countered, as it was on the stack. The id is
    -- already dead by the time any reader sees this: countering removes it from
    -- the stack through Pawl.Engine.Event.changeZone, and CR 400.7 mints a fresh
    -- incarnation in the graveyard. Carried anyway, because it is WHAT HAPPENED
    -- -- the event otherwise says only that somebody countered something, and
    -- two counters in one batch would be indistinguishable entries.
    spell :: ObjectId,
    -- The spell or ability that did the countering, which is what Baral, Chief
    -- of Compliance's "a spell or ability YOU CONTROL counters a spell" names.
    -- Whichever object Pawl.Engine.Resolve calls the effect's source: the
    -- resolving spell itself for a spell, and CR 113.7's SOURCE PERMANENT -- not
    -- the ability object -- for an ability.
    --
    -- Nothing reads it yet. Carried because it is the other end of the act this
    -- record describes, and because a condition scoped to the bearer ("whenever
    -- THIS creature counters a spell") is the one thing `controller` below could
    -- not answer.
    source :: ObjectId,
    -- CR 405.4: who controlled `source` AT THE MOMENT IT COUNTERED -- "the
    -- controller of a spell is the player who cast it", and for an ability the
    -- player who activated it or who controlled its source when it triggered.
    -- The player a "a spell or ability YOU CONTROL" condition compares against
    -- CR 109.5's "you".
    --
    -- Captured here rather than re-derived from `source` at match time, the
    -- deal-time rider posture Pawl.Types.DamageEvent's dealtByDeathtouch takes,
    -- and for that comment's reason: it may be unaskable later. Both halves of
    -- rule 701.6a's "a spell or ability" have a case where it is:
    --
    --   * a countering SPELL, always. `source` is the spell object itself, and
    --     CR 608.2n puts it into its owner's graveyard as the final part of its
    --     own resolution -- before the CR 117.5 scan reads this log, and under a
    --     new id (CR 400.7). Nothing live answers for the old one.
    --   * a countering ABILITY, when its source has left. `source` is then CR
    --     113.7's source permanent, which is usually still on the battlefield --
    --     but need not be, and Pawl.Engine.Resolve's own note says why ("for an
    --     ability, the source permanent may already be sacrificed as a cost").
    --     CR 608.2h last known information would then answer with the controller
    --     as of the DEPARTURE rather than as of the countering.
    --
    -- Even for a source that is still there, re-deriving would read control at
    -- the scan boundary rather than at the event -- #47's elision, which this
    -- field simply does not have.
    controller :: PlayerId
  }
  deriving (Eq, Ord, Show)
