module Pawl.Types.BecameBlocking where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 509.1g: a creature became a blocking creature, and which attacking
-- creature it is blocking.

-- BOTH ids are an ObjectId and they are NOT interchangeable, so they are named
-- rather than positional: a swap would say the attacker blocked the blocker.
data BecameBlocking = MkBecameBlocking
  { blocker :: ObjectId.ObjectId,
    attacker :: ObjectId.ObjectId,
    -- | CR 509.4: whether this creature was PUT ONTO THE BATTLEFIELD blocking
    -- rather than declared under CR 509.1a. Such a creature is "blocking" but,
    -- for the purposes of trigger events, it never "blocked", and two of this
    -- event's readers split on exactly that: CR 509.3b's "whenever [a creature]
    -- blocks a creature" won't trigger ("It won't trigger if the creature is put
    -- onto the battlefield blocking"), where CR 509.3d's "whenever [a creature]
    -- becomes blocked by a creature" will ("In addition, it will trigger if a
    -- creature is put onto the battlefield blocking that creature").
    --
    -- The THIRD reader wants the flag SET, which is the one reading rule 509.4
    -- does not govern: CR 509.3e's "effects that add or remove blockers", where
    -- the arrival is what pushes an already-blocked attacker over a count. That
    -- one is about the ATTACKER's tally rather than about whether this creature
    -- blocked, so rule 509.4's denial does not reach it.
    --
    -- The flag names the PRODUCER rather than negating "declared", because the
    -- rules' third producer -- an effect that causes a creature to block -- is
    -- neither a declaration nor an entry, and CR 509.3b does trigger for it. No
    -- such effect is in the pool (#1146); it would record this event with the
    -- flag clear.
    putOntoBattlefield :: Bool,
    -- | CR 509.1h: whether the ATTACKER was already a blocked creature
    -- immediately before this event, read before the write that made it.
    --
    -- Not derivable from Combat.blockers by the time a trigger condition is
    -- scanned. The write has already put this blocker into the attacker's entry,
    -- so Map.member answers True for every arrival; and rule 509.1h's last
    -- sentence ("a creature remains blocked even if all the creatures blocking
    -- it are removed from combat") means the surviving key can hold an EMPTY
    -- set, so no count over that entry can tell "was blocked, nothing blocking
    -- it now" from "was unblocked" either. Pawl.Engine.Game.removeFromCombat is
    -- what spells that emptied key.
    --
    -- The one reader is rule 509.3e's filtered form
    -- (TriggerCondition.SelfBecomesBlockedByOneOrMore), which fires off an
    -- arrival only when no GameEvent.AttackerBlocked rode the same arrival --
    -- CR 509.3c withholds that event exactly when this field is True, so the
    -- field is how the two arms avoid answering one becoming-blocked twice.
    attackerWasBlocked :: Bool,
    -- | CR 509.3e's comparand: the creatures blocking the ATTACKER immediately
    -- before this event, read before the write that added this blocker.
    --
    -- On CR 509.4's road, the one both readers guard on, exactly one creature
    -- joins per event -- so the attacker is blocked by one more than this many
    -- the moment after, a count trigger's floor is crossed here when this many
    -- plus one equals it, and a filtered one's when nothing here matched the
    -- Filter. CR 509.1's declaration is simultaneous instead, so every pair it
    -- records carries the same pre-declaration set; no reader looks.
    --
    -- Not derivable from Combat.blockers when the condition is scanned, and for
    -- a sharper reason than the flag above: several creatures can be put onto
    -- the battlefield blocking one attacker BEFORE any of them is scanned (CR
    -- 509.2a, and CR 614.16 doubling one token-making effect is the pooled
    -- producer), so the entry read at the scan holds the whole batch and every
    -- arrival in it reads the same set. Subtracting this arrival from that set
    -- leaves the others in, which is what made a doubled arrival invisible to
    -- both readers.
    --
    -- The SET rather than a count, because the two readers want different
    -- things off it: TriggerCondition.CreatureBecomesBlockedByAtLeast counts,
    -- and TriggerCondition.SelfBecomesBlockedByOneOrMore asks its Filter about
    -- each id (through CR 608.2h last known information, a blocker that has
    -- since left keeping its id here).
    blockersBefore :: Set.Set ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
