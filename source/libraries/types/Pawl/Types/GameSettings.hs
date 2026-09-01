module Pawl.Types.GameSettings where

import qualified Pawl.Types.AttackOption as AttackOption

-- | CR 800.2: the options a game was started with -- "a series of options that
-- can be added to a multiplayer game and a number of variant styles of
-- multiplayer play. A single game may use multiple options but only one
-- variant."
--
-- A RECORD of options and not a sum of format names, because that is how the
-- CR factors itself: a named format is a preset over these fields, and CR
-- 903.12a says so outright for the Brawl field ("Brawl is an option for a
-- different style of Commander game"). Each further option -- CR 804's deploy
-- creatures, CR 801.2a's range of influence, CR 810's teams -- is one more field
-- rather than one more name (#175). CR 802's and CR 803's three attack options
-- share ONE field, because CR 806.2b makes them alternatives rather than
-- independent switches (Pawl.Types.AttackOption).
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
    -- | CR 806.2b: which of the three attack options this game uses, if any.
    -- 'AttackOption.MultiplePlayers' by default (Pawl.Engine.Setup.newGame),
    -- and read in one place -- Pawl.Engine.Combat.attackableOpponents, which is
    -- the candidate list every other combat rule reaches through
    -- Pawl.Engine.Defender.defendingPlayers.
    --
    -- Nothing is CR 507.1's free choice among every opponent, which is what CR
    -- 506.2's two-player game plays by and what CR 806.2b forbids at three or
    -- more seats. At two seats it and 'AttackOption.MultiplePlayers' coincide --
    -- the one opponent is the defending player either way -- so the default
    -- changes nothing there.
    attackOption :: Maybe AttackOption.AttackOption
  }
  deriving (Eq, Ord, Show)
