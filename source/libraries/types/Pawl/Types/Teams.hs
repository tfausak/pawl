module Pawl.Types.Teams where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TeamId as TeamId

-- | CR 808.1: which team each player is on. Empty is CR 102.4's game that is
-- not played between teams, which is every game pawl played before teams
-- existed.
--
-- A MAP from player to team and not a list of teams, because that is the shape
-- rule 102.3 asks about -- "the other players on their team" is one lookup --
-- and because it cannot represent a player on two teams at once, which no
-- partition of the table admits. A player with no entry is on no team: rule
-- 808.1 has every player on one, so a mixed table is not a legal Team vs. Team
-- game, and CR 102.3 still answers for it (every player not on your team is an
-- opponent, and a player on no team shares one with nobody).
newtype Teams = MkTeams
  { unwrap :: Map.Map PlayerId.PlayerId TeamId.TeamId
  }
  deriving (Eq, Ord, Show)

-- | CR 102.4: a game that isn't played between teams.
none :: Teams
none = MkTeams Map.empty

-- | CR 102.3: is @candidate@ an opponent of @you@ -- a player not on your team?
--
-- The ONE definition of the relation, so that a new reader cannot answer it as
-- "not me" by accident: that reading is CR 806.1's free-for-all, exact only
-- while no player has a teammate. Pawl.Types.PlayerRelation.holds and
-- Pawl.Engine.Game.opponentsOf are both this function, and every site in the
-- engine that asks who somebody's opponents are goes through one of the three.
--
-- Sits beside the type for Pawl.Types.PlayerRelation.holds's reason: it is a
-- fact about what team membership MEANS rather than about the board, and
-- callers on both sides of the module graph need it.
--
-- Teammates are not opponents and neither are you (CR 102.3 says "the other
-- players"), so a player is never their own opponent whether or not they are on
-- a team.
areOpponents :: Teams -> PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
areOpponents teams you candidate =
  candidate /= you && not (sameTeam teams you candidate)

-- | CR 102.3: are these two players teammates? False for a player asked about
-- themself, and false whenever either is on no team -- CR 102.4's game has no
-- teammates in it at all.
sameTeam :: Teams -> PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
sameTeam teams you candidate = case (teamOf teams you, teamOf teams candidate) of
  (Just yours, Just theirs) -> candidate /= you && yours == theirs
  _ -> False

-- | Which team a player is on, or Nothing for one on no team.
teamOf :: Teams -> PlayerId.PlayerId -> Maybe TeamId.TeamId
teamOf teams pid = Map.lookup pid (unwrap teams)
