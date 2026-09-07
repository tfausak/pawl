module Pawl.Types.MoveMana where

import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Effect's MoveMana arm (CR 106.13): the players
-- `from` names lose all their unspent mana, and the players `to` names add the
-- mana lost this way.
--
-- WHOLE UNITS cross, which is how the rule's second sentence is kept: "which
-- permanents, spells, and\/or abilities produced that mana are unchanged, as are
-- any restrictions or additional effects associated with any of that mana." A
-- transfer written as "count the types and add that much" would launder every
-- axis Pawl.Types.ManaUnit carries but the type itself -- its CR 106.3 production
-- tags, its CR 106.4 retention, and both of CR 106.6's clauses.
--
-- TWO references and not one player each, because CR 106.13's parenthetical says
-- "these may be the same player": a card naming the same player on both sides
-- moves the mana onto itself, which the arm performs as an empty and a re-add of
-- the same units rather than as a doubling.
data MoveMana = MkMoveMana
  { from :: PlayerRef.PlayerRef,
    to :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
