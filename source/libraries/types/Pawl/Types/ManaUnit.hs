module Pawl.Types.ManaUnit where

import Pawl.Types.ManaType (ManaType)

-- One unit of mana in a pool.
--
-- Grows TWO separate collections, and conflating them would be a mistake:
--
--   * Production-time tags -- CLOSED half. Snow-ness; "this activation caused
--     you to lose life" (Yawgmoth's Day Planner: "Only mana produced by
--     abilities that caused you to lose life may be spent to cast spells this
--     way"). These are observable facts about the production event, which the
--     engine determines itself with no card knowledge.
--   * Spending restrictions -- OPEN half. "Spend this mana only to cast a
--     creature spell of the chosen type" (Cavern of Souls) is a predicate over
--     spells. Payment must ask a classification ("may this unit pay for that
--     spell?") and must NEVER case on the restriction itself -- that would be
--     the rules core learning an effect's identity.
--
-- Both are sets/lists, never fixed fields: a unit can carry several at once.
--
-- Neither collection exists yet: snow is #305 and the spending restrictions are
-- #252.
--
-- Deliberately no source ObjectId. Snow cares about a PROPERTY of the source,
-- not its identity, and a reference would dangle by construction: mana outlives
-- its source (tap a land, the land is destroyed in response, the mana remains)
-- and CR 400.7 mints a fresh id on every zone change. Properties are captured at
-- production time. Becomes a `data` when the first collection lands.
newtype ManaUnit = MkManaUnit
  { manaType :: ManaType
  }
  deriving (Eq, Ord, Show)
