module Pawl.Types.GameSettings where

import qualified Pawl.Types.Teams as Teams

-- | CR 800.2: the options a game was started with -- "a series of options that
-- can be added to a multiplayer game and a number of variant styles of
-- multiplayer play. A single game may use multiple options but only one
-- variant."
--
-- A RECORD of options and not a sum of format names, because that is how the
-- CR factors itself: a named format is a preset over these fields, and CR
-- 903.12a says so outright for the Brawl field ("Brawl is an option for a
-- different style of Commander game"). Each further option -- CR 803/804's
-- remaining attack options, CR 801.2a's range of influence, CR 810's teams --
-- is one more field rather than one more name (#175).
--
-- Settled before the game begins and never written afterwards: nothing in the
-- CR turns an option on mid-game. It is on GameState rather than beside it
-- because every rule that reads it is reading the game it is in -- a subgame
-- (CR 729) and a restarted game (CR 727) each carry their own copy.
data GameSettings = MkGameSettings
  { -- | CR 903.12a: whether this is a Brawl game. Three rules read it -- CR
    -- 903.12f's starting life, CR 903.12g's free first mulligan, and CR
    -- 903.12h's removal of CR 704.6c's state-based action.
    --
    -- Not implemented: CR 903.12b--e, which are deck construction (the card
    -- pool, the legendary designation, the 60-card deck, the basic-land
    -- exception). Pawl enforces no deck legality at all (#940), so a Brawl deck
    -- is unchecked exactly as a Commander deck is.
    brawl :: Bool,
    -- | CR 802.1: whether the attack multiple players option is used, so that
    -- CR 802.2 makes every one of the attacking player's opponents a defending
    -- player instead of CR 507.1 naming one.
    --
    -- True by default (Pawl.Engine.Setup.newGame). At two seats the option and
    -- CR 506.2's base rule coincide -- the one opponent is the defending player
    -- either way and CR 507.1 asks nothing -- so it changes nothing there and
    -- corrects three or more, where CR 806.2b requires one of CR 802's and CR
    -- 803's three options and a free choice among every opponent is none of
    -- them.
    --
    -- Not implemented: CR 803.1a's attack left and CR 803.1b's attack right,
    -- the other two options CR 806.2b allows (#2830). False therefore
    -- leaves CR 507.1's free choice, which is legal at two seats and is no
    -- legal multiplayer game at three or more.
    attackMultiplePlayers :: Bool,
    -- | CR 808.1: which team each player is on, so that CR 102.3 can take a
    -- player's teammates out of their opponents.
    --
    -- Teams.none by default (Pawl.Engine.Setup.newGame), which is CR 102.4's
    -- game that is not played between teams -- every game pawl started before
    -- this field existed, and the one CR 806.1's free-for-all reading of
    -- "opponent" is exact for.
    --
    -- Not implemented: CR 808.2's seating and CR 808.4's starting player, which
    -- are the rest of the Team vs. Team variant -- pawl's turn order is the
    -- caller's list and no random team is chosen from it (#2847). CR 808.3a's
    -- attack multiple players option is the field above, on by default, so a
    -- game with teams gets it without asking. CR 808.5 needs nothing: pawl
    -- shares no resource between players, and one player has never been able to
    -- touch another's cards.
    teams :: Teams.Teams
  }
  deriving (Eq, Ord, Show)
