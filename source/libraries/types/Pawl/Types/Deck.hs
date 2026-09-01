module Pawl.Types.Deck where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Printing as Printing

-- | A deck: the multiset of printings a player's library is built from, plus CR
-- 903.3's commander designation and the two things the player brings alongside
-- it -- CR 309.2's dungeon cards and CR 100.4's sideboard.
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
-- partner and background rules that let a deck have two (CR 702.124, CR 702.124k)
-- have no producer here (#939).
data Deck = MkDeck
  { cards :: Map.Map Printing.Printing Natural.Natural,
    -- | CR 903.3: the card designated as this deck's commander, which CR 903.6
    -- starts in the command zone.
    commander :: Maybe Printing.Printing,
    -- | CR 902.3: the vanguard card this player brings, which CR 313.2 keeps in
    -- the command zone for the whole game.
    --
    -- Here for the commander's reason, and NOT one of the `cards` for the
    -- commander's reason either: rule 902.3 calls it a game material each player
    -- needs "in addition to the normal game materials", placed beside the library
    -- before the game begins, so counting it among the library's printings would
    -- deal it into a hand. Nothing but a Vanguard game sets it, so `Nothing` is
    -- every other format.
    --
    -- ONE, like the commander and unlike the dungeons: CR 902.1 gives each player
    -- "one face-up vanguard card".
    vanguard :: Maybe Printing.Printing,
    -- | CR 309.2: the dungeon cards this player owns from OUTSIDE the game, which
    -- CR 701.49a draws on the first time they venture.
    --
    -- Here for the commander's reason and not the same one. CR 309.2 keeps dungeon
    -- cards out of the deck and the sideboard both, so these are not among `cards`
    -- either -- but outside the game is not a zone (CR 400.11), so unlike the
    -- commander there is nowhere for them to sit until one is needed. Setup records
    -- them on the player and mints no object; Pawl.Engine.Dungeon.venture is what
    -- brings one into the game.
    --
    -- SEVERAL, unlike the commander, because CR 309.2a says "a dungeon card they
    -- own" out of however many they brought and CR 701.49a therefore asks them to
    -- choose. CR 309.3's limit of one is on the COMMAND ZONE and is enforced by
    -- Pawl.Engine.Dungeon.inDungeon, not here: this field is the supply outside the
    -- game, which no rule bounds.
    --
    -- A Set and not `cards`' multiset: two copies of one dungeon printing offer the
    -- same choice and enter the same card, so a count would be a distinction CR
    -- 309.2a's choice cannot see.
    dungeons :: Set.Set Printing.Printing,
    -- | CR 100.4 \/ 103.2a: the cards this player sets aside before the game, which
    -- CR 400.11a puts outside the game. What is left after they are set aside is
    -- the starting deck, so these are not among `cards`.
    --
    -- Here for the commander's and the dungeons' reason: it is what the player
    -- BROUGHT, settled before the game begins, and every caller that builds a game
    -- already threads a Deck per player.
    --
    -- A MULTISET and not `dungeons`' set, which is the one place this field parts
    -- from the one above it. CR 100.4a caps a sideboard at fifteen CARDS and
    -- applies CR 100.2a's four-card limit across deck and sideboard together, so
    -- two copies of a card are two cards; and Pawl.Engine.OutsideTheGame spends
    -- them, so the second Burning Wish of a game can find the second copy and only
    -- the second copy. A dungeon is a supply nothing spends (CR 309.5b), which is what lets
    -- that field forget its counts.
    --
    -- Pawl.Engine.Setup.createDeck interns these into Player.outsideTheGame, which
    -- is the pool the rules read; this field is the deck-building half and nothing
    -- in the engine reads it after setup.
    sideboard :: Map.Map Printing.Printing Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | A deck with no commander, no vanguard, no dungeons and no sideboard -- every
-- format but Commander and Vanguard, every game nobody ventures in, and every
-- game nobody wishes in.
fromCards :: Map.Map Printing.Printing Natural.Natural -> Deck
fromCards m = MkDeck {cards = m, commander = Nothing, vanguard = Nothing, dungeons = Set.empty, sideboard = Map.empty}
