module Pawl.Type.StateCondition where

import Pawl.Type.Subtype (Subtype)

-- CR 603.8 / 603.4: a predicate over game STATE rather than over an event. Two
-- customers, one vocabulary: a state TRIGGER's condition (CR 603.8, checked at
-- every CR 117.5 boundary) and an INTERVENING "if" (CR 603.4 when the trigger
-- event occurs, CR 608.2a again on resolution).
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
  deriving (Eq, Ord, Show)
