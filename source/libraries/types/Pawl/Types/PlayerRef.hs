module Pawl.Types.PlayerRef where

import Pawl.Types.PlayerRelation (PlayerRelation)
import Pawl.Types.SlotName (SlotName)

-- CR 400.1: "each player has their own library, hand, and graveyard. The other
-- zones are shared by all players." This says WHOSE zone a scope folds over.
--
-- Deliberately NOT Pawl.Types.PlayerScope, which is You | Opponents | EachPlayer
-- and looks like the same type. PlayerScope is resolved against a controller and
-- nothing else (Pawl.Engine.PlayerEffect.inScope); this can also name a binding slot,
-- which PlayerEffect has no way to answer. Adding InSlot there would give
-- Pawl.Types.PlayerEffect a constructor its evaluator cannot resolve.
data PlayerRef
  = -- Every player's copy of the zone. For a SHARED zone (CR 400.1: battlefield,
    -- stack, exile, command) this is the only meaningful value; the pairing is
    -- checked by the card lint, not by this type (#161).
    EachPlayer
  | -- CR 109.5 / 102.2, resolved against the evaluation context's perspective.
    Relative PlayerRelation
  | -- The player bound in a slot -- Sudden Impact's "that player's hand", where
    -- the slot was filled by targeting (CR 601.2c).
    InSlot SlotName
  deriving (Eq, Ord, Show)
