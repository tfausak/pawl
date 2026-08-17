module Pawl.Types.Deck where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Printing as Printing

-- | A deck: the multiset of printings a player's library is built from, plus CR
-- 903.3's commander designation.
--
-- The cards are a multiset because a shuffle erases any order among them, so
-- counts are the honest model. `Printing` and everything beneath it derive `Ord`,
-- so it is a lawful `Map` key.
--
-- The COMMANDER is here rather than beside the deck because CR 903.3 makes it a
-- deck-construction property -- it is designated before the game begins, from the
-- cards the player brought -- and because every caller that builds a game already
-- threads a Deck per player. Nothing but a Commander game sets it, so `Nothing`
-- is every other format.
--
-- It is NOT one of the `cards`: CR 903.6 starts it in the command zone rather
-- than the library, so counting it among the library's printings would put a
-- second copy in the deck. Pawl.Engine.Setup.createDeck reads the two fields into
-- the two zones.
--
-- ONE commander, not a set: CR 903.3 says "a legendary creature card", and the
-- partner and background rules that let a deck have two (CR 702.124, CR 702.149)
-- have no producer here (#939).
data Deck = MkDeck
  { cards :: Map.Map Printing.Printing Natural.Natural,
    -- | CR 903.3: the card designated as this deck's commander, which CR 903.6
    -- starts in the command zone.
    commander :: Maybe Printing.Printing,
    -- | CR 309.2: a dungeon card this player owns from OUTSIDE the game, which CR
    -- 701.49a brings into the command zone the first time they venture.
    --
    -- Here for the commander's reason and not the same one. CR 309.2 keeps dungeon
    -- cards out of the deck and the sideboard both, so this is not one of `cards`
    -- either -- but outside the game is not a zone (CR 400.11), so unlike the
    -- commander there is nowhere for it to sit until it is needed. Setup records it
    -- on the player and mints no object; Pawl.Engine.Dungeon.venture is what brings
    -- it into the game.
    --
    -- ONE dungeon, where CR 309.2a says "a dungeon card they own" out of however
    -- many they brought and CR 701.49a therefore asks them to choose. With one there
    -- is nothing to ask. The multi-dungeon choice is unimplemented (#1335).
    dungeon :: Maybe Printing.Printing
  }
  deriving (Eq, Ord, Show)

-- | A deck with no commander and no dungeon -- every format but Commander, and
-- every game nobody ventures in.
fromCards :: Map.Map Printing.Printing Natural.Natural -> Deck
fromCards m = MkDeck {cards = m, commander = Nothing, dungeon = Nothing}
