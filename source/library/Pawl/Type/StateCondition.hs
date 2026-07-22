module Pawl.Type.StateCondition where

import Pawl.Type.Subtype (Subtype)

-- CR 603.8 / 603.4: a predicate over game STATE rather than over an event.
-- Three customers, one vocabulary: a state TRIGGER's condition (CR 603.8,
-- checked at every CR 117.5 boundary), an INTERVENING "if" (CR 603.4 when the
-- trigger event occurs, CR 608.2a again on resolution), and a "for as long as"
-- duration (CR 611.2b, Pawl.Expiry.arm and Pawl.Expiry.sweepConditional).
--
-- Hand-carved, one variant per card -- the TargetSpec.WallTarget and CountSpec
-- posture, specific before general. Only Pawl.Event may case on it, and it reads
-- the PROJECTION: a subtype is CR 613 layer 4 and control is layer 2, so a card
-- that changed either must change the answer.
--
-- Retired wholesale by P9's criterion/filter language (#38).
data StateCondition
  = -- CR 603.8: Barbarian Outcast, "you control no Swamps". Scoped to the
    -- ABILITY's controller (CR 603.3a), not to the board.
    YouControlNo Subtype
  | -- CR 603.4: Sarcomancy, "if there are no Zombies on the battlefield". ANY
    -- player's permanents -- deliberately distinct from YouControlNo.
    NoPermanentsOfSubtype Subtype
  | -- CR 611.2b: Master Thief, "for as long as you control this creature". The
    -- OBJECT is the effect's own source, supplied by the caller; "you" is the
    -- PlayerId the surrounding Expiry.While carries. Both halves are
    -- load-bearing: the source must still be on the battlefield, and its
    -- PROJECTED controller (CR 613.1b) must be that player. CR 400.7 makes the
    -- first robust for free -- a permanent that dies and returns is a new object
    -- with a new id, so the stored source can never be satisfied by it.
    YouControlSource
  deriving (Eq, Ord, Show)
