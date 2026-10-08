module Pawl.Types.GameSettings where

import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Emperors as Emperors
import qualified Pawl.Types.RangeOfInfluence as RangeOfInfluence
import qualified Pawl.Types.Teams as Teams

-- | CR 800.2: the options a game was started with -- "a series of options that
-- can be added to a multiplayer game and a number of variant styles of
-- multiplayer play. A single game may use multiple options but only one
-- variant."
--
-- A RECORD of options and not a sum of format names, because that is how the
-- CR factors itself: a named format is a preset over these fields, and CR
-- 903.12a says so outright for the Brawl field ("Brawl is an option for a
-- different style of Commander game"). Each further option -- CR 804's deploy
-- creatures below -- is one more field rather than one more name. CR 802's
-- and CR 803's three attack options share ONE field, because CR 806.2b makes
-- them alternatives rather than independent switches (Pawl.Types.AttackOption).
--
-- Settled before the game begins and never written afterwards: nothing in the
-- CR turns an option on mid-game. It is on GameState rather than beside it
-- because every rule that reads it is reading the game it is in -- a subgame
-- (CR 729) and a restarted game (CR 727) each carry their own copy.
data GameSettings = MkGameSettings
  { -- | CR 903.12a: whether this is a Brawl game. Four rules read it -- CR
    -- 903.12c's designation (Pawl.Engine.Commander.soleCommander), CR 903.12f's
    -- starting life, CR 903.12g's free first mulligan, and CR 903.12h's removal
    -- of CR 704.6c's state-based action.
    --
    -- Not implemented: CR 903.12b, CR 903.12d and CR 903.12e, which are deck
    -- construction (the card pool, the 60-card deck, the basic-land exception).
    -- Pawl enforces no deck legality at all (#940), so a Brawl deck is unchecked
    -- exactly as a Commander deck is.
    brawl :: Bool,
    -- | CR 806.2b: which of the three attack options this game uses, if any.
    -- 'AttackOption.MultiplePlayers' by default ('plain'), and read in two
    -- places, both in Pawl.Engine.Combat: attackableOpponents cuts the
    -- candidate list every other combat rule reaches through
    -- Pawl.Engine.Defender.defendingPlayers, and designateDefenders reads it
    -- again to decide whether CR 507.1 has a choice to prompt for at all.
    --
    -- Nothing is CR 507.1's free choice among every opponent, which is what CR
    -- 506.2's two-player game plays by and what CR 806.2b forbids at three or
    -- more seats. At two seats it and 'AttackOption.MultiplePlayers' coincide --
    -- the one opponent is the defending player either way -- so the default
    -- changes nothing there.
    attackOption :: Maybe AttackOption.AttackOption,
    -- | CR 808.1: which team each player is on, so that CR 102.3 can take a
    -- player's teammates out of their opponents.
    --
    -- Teams.none by default ('plain'), which is CR 102.4's game that is not
    -- played between teams -- every game pawl started before this field
    -- existed, and the one CR 806.1's free-for-all reading of "opponent" is
    -- exact for.
    -- Pawl.Engine.Archenemy.setUp writes CR 904.2's two teams.
    --
    -- Not implemented: CR 808.2's seating is unchecked (#4497), and CR 808.4's
    -- starting player, no random team being chosen from the caller's list
    -- (#2847). CR 808.3a's attack multiple players option is the field above,
    -- which is on by default, so a game with teams gets it without asking. CR 808.5 needs nothing: no
    -- Team vs. Team resource is shared (sharedTeamLife is CR 810's and CR 904's),
    -- and one player has never been able to touch another's cards.
    teams :: Teams.Teams,
    -- | CR 805.1: whether each team takes its turns together, every member of
    -- the active team being an active player (CR 805.4a, 805.9). Off by
    -- default ('plain'), on in an Archenemy game
    -- (Pawl.Engine.Archenemy.setUp, CR 904.2), and read through
    -- Pawl.Engine.Turn.sharesTurn.
    --
    -- Not implemented: CR 805.1's adjacent seating is not checked, the turn
    -- order being the caller's list (#4497).
    sharedTeamTurns :: Bool,
    -- | CR 810.4 / 904.13b: whether each team shares one life total. Off by
    -- default ('plain'); Pawl.Engine.Game.lifeSharers reads it.
    --
    -- Not implemented: CR 810.9b's joint payment cap, CR 810.9d's team choosing
    -- the one member a "set each player's life total" affects, and CR 810.9f's
    -- one member per team in a redistribution (#4493).
    sharedTeamLife :: Bool,
    -- | CR 801.2a: each player's range of influence.
    -- 'RangeOfInfluence.unlimited' by default ('plain'), since
    -- CR 801.1 makes a limited range an option. Read through
    -- Pawl.Engine.Game.inRangeOf.
    rangeOfInfluence :: RangeOfInfluence.RangeOfInfluence,
    -- | CR 804.2: whether each creature has "{T}: Target teammate gains control
    -- of this creature. Activate only as a sorcery." Off by default
    -- ('plain'), and read through Pawl.Engine.Deploy.
    deployCreatures :: Bool,
    -- | CR 809.2: each team's emperor. Emperors.none by default ('plain');
    -- Pawl.Engine.Emperor.setUp writes it with the rest of CR 809.3's options,
    -- and Pawl.Engine.Departure reads it for CR 809.5b and 809.5c.
    emperors :: Emperors.Emperors,
    -- | CR 810: whether this is a Two-Headed Giant game. Read through
    -- Pawl.Engine.Engine.skipsDraw for CR 810.6's skipped first draw, and
    -- Pawl.Engine.Departure.teamFallsWith for CR 810.8a / 810.8b's team loss,
    -- Pawl.Engine.Game.counterSharers for CR 810.10's shared poison and
    -- Pawl.Engine.Sba.poisonThreshold for CR 704.6b's. The variant's other
    -- options are their own fields: a caller sets teams (CR 810.1),
    -- sharedTeamTurns (CR 810.2) and sharedTeamLife (CR 810.4) beside it.
    twoHeadedGiant :: Bool,
    -- | CR 811: whether this is an Alternating Teams game. Read through
    -- Pawl.Engine.Combat.attackableOpponents for CR 811.4's attack limit, which
    -- filters whatever the attack option above allows (CR 811.2b).
    -- Pawl.Engine.AlternatingTeams.setUp writes it with teams (CR 811.1) over a
    -- seating it checks (CR 811.3).
    alternatingTeams :: Bool,
    -- | CR 407.1: whether this game is played for ante. Off by default
    -- ('plain'); read by Pawl.Engine.Setup's CR 407.2 step and
    -- Pawl.Engine.Ante.barred (CR 407.3).
    ante :: Bool
  }
  deriving (Eq, Ord, Show)

-- | CR 800.2 / CR 103: a game with no option in use, which is what rule 103
-- describes on its own. CR 802.1 is the attack option in use by default: at two
-- seats it coincides exactly with CR 506.2, and at three or more CR 806.2b
-- requires one of the three and this is the one that needs no seating to be
-- agreed. CR 102.4 / CR 808.1: and a game not played between teams, which every
-- variant but CR 808's, CR 809's, CR 810's and CR 811's is.
plain :: GameSettings
plain =
  MkGameSettings
    { brawl = False,
      attackOption = Just AttackOption.MultiplePlayers,
      teams = Teams.none,
      sharedTeamTurns = False,
      sharedTeamLife = False,
      rangeOfInfluence = RangeOfInfluence.unlimited,
      deployCreatures = False,
      emperors = Emperors.none,
      twoHeadedGiant = False,
      alternatingTeams = False,
      ante = False
    }
