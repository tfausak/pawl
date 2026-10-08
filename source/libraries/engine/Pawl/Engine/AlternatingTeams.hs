-- | CR 811: the Alternating Teams variant -- its options, set over a seating
-- it checks.
module Pawl.Engine.AlternatingTeams where

import qualified Data.Set as Set
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.GameSettings as GameSettings
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Teams as Teams

-- | CR 811.2: an Alternating Teams game over this game's seating
-- (GameState.turnOrder) and these teams, or Nothing when the game cannot be
-- one: a seating 'seated' refuses, or an attack option other than CR 811.2b's
-- three. The attack option is the caller's; CR 811.2a's range of influence is
-- only recommended and CR 811.2c's deploy creatures option is off in
-- GameSettings.plain, so neither is written here.
setUp :: Teams.Teams -> GameState -> Maybe GameState
setUp teams gs =
  let settings = GameState.settings gs
      optionAllowed = case GameSettings.attackOption settings of
        Just AttackOption.MultiplePlayers -> True
        Just AttackOption.Leftward -> True
        Just AttackOption.Rightward -> True
        Just AttackOption.Adjacent -> False
        Nothing -> False
   in if optionAllowed && seated teams (GameState.turnOrder gs)
        then Just gs {GameState.settings = settings {GameSettings.teams = teams, GameSettings.alternatingTeams = True}}
        else Nothing

-- | CR 811.1 / 811.3: two or more teams of equal size covering the table,
-- seated so that no one is next to a teammate and each team is equally spaced
-- out. With @n@ teams that is the seating repeating every @n@ seats round the
-- table, CR 811.3's example's A1, B1, C1, A2, B2, C2: a repeat that wraps cleanly
-- forces every team into each run of @n@ seats once, so equal sizes and no
-- teammate beside you follow. Every seat must be on a team.
seated :: Teams.Teams -> [PlayerId.PlayerId] -> Bool
seated teams seats = case traverse (Teams.teamOf teams) seats of
  Nothing -> False
  Just byTeam ->
    let n = Set.size (Set.fromList byTeam)
     in n >= 2 && and (zipWith (==) byTeam (drop n (cycle byTeam)))
