module Pawl.Types.BecameBlocking where

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
    putOntoBattlefield :: Bool
  }
  deriving (Eq, Ord, Show)
