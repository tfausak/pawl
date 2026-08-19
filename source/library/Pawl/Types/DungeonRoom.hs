module Pawl.Types.DungeonRoom where

import qualified Data.Set as Set
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.RoomIndex as RoomIndex

-- | CR 309.4: one room of a dungeon card -- a name, an ability, and the arrows
-- pointing away from it.
--
-- Parametric in @card@ for 'Pawl.Types.Face.Face'\'s reason: the ability's payload
-- names a whole card (a token it creates), and @Pawl.Types.Card@ ties the knot.
--
-- The room ability is carried as a bare 'Modal.Modal' rather than a whole
-- 'Pawl.Types.TriggeredAbility.TriggeredAbility' because CR 309.4c fixes
-- everything else about it: every room ability has the same unprinted trigger
-- condition ("When you move your venture marker into this room") and no
-- intervening \"if\". Storing a condition per room would let card data disagree
-- with the rule; Pawl.Engine.Dungeon mints the triggered ability instead.
data DungeonRoom card = MkDungeonRoom
  { -- | CR 309.4b: the room's printed name. Flavor text that does not affect game
    -- play, kept so a transcribed dungeon reads like the card -- and reused as the
    -- label a branch prompt offers, which is what a player picking an arrow
    -- actually sees. An AbilityName rather than a CardName: it names a piece of
    -- this card's text, not a card, which is the same job the key of
    -- 'Pawl.Types.Face.delayedAbilities' does.
    name :: AbilityName.AbilityName,
    -- | CR 309.4c: the effect printed on this room, which its room ability puts on
    -- the stack when the venture marker arrives.
    ability :: Modal.Modal card,
    -- | CR 309.4 \/ 309.5a: the arrows pointing AWAY from this room, as the rooms
    -- they lead to. Empty for the bottommost room, which is where CR 309.6 ends the
    -- dungeon. A Set because the arrows out of a room are unordered -- CR 309.5a
    -- has the player choose one, and nothing ranks them.
    exits :: Set.Set RoomIndex.RoomIndex
  }
  deriving (Eq, Ord, Show)
