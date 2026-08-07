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
    commander :: Maybe Printing.Printing
  }
  deriving (Eq, Show)

-- | A deck with no commander -- every format but Commander.
fromCards :: Map.Map Printing.Printing Natural.Natural -> Deck
fromCards m = MkDeck {cards = m, commander = Nothing}
