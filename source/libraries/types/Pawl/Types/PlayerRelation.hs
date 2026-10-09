module Pawl.Types.PlayerRelation where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Teams as Teams

-- | How a player a card names -- an object's controller, a trigger event's
-- player, a replacement's watched player, a player effect's affected player --
-- stands to the perspective the evaluation carries (the source's controller
-- when targeting; the effect's controller for a continuous effect). CR 109.5
-- fixes "you" as the object's controller; Opponent is CR 102.3's every player
-- not on the perspective's team, which is every other player in CR 102.4's game
-- with no teams -- CR 806.1's free-for-all reading and CR 102.2's two-player
-- one, the same predicate either way.
--
-- The ONE player-relation vocabulary: Pawl.Types.ControllerRelation and
-- Pawl.Types.PlayerScope carry it as their Related arm, and every judge of
-- either goes through 'holds' below.
data PlayerRelation
  = You
  | Opponent
  | -- | CR 102.1's bare "a player" -- every player in the game, the perspective
    -- INCLUDED. The union of the two arms above, which is not derivable from
    -- either: You and Opponent partition the table, so a card printing "whenever
    -- a player loses life" (The Master of Lake-town's) says something neither
    -- states and both are observably narrower than.
    --
    -- Perspective-free, and the only arm that is ('perspectiveFree'): the
    -- others compare a candidate against CR 109.5's "you", and this one asks
    -- nothing about the candidate at all. It is still carried as a relation
    -- rather than hoisted out of the type, because the card text it transcribes
    -- sits in exactly the position the other arms do.
    --
    -- Not a licence to name a DEPARTED seat. As a predicate this arm judges a
    -- candidate the caller already holds, and the callers that fold a player SET
    -- instead (Pawl.Engine.Count.playersFor, Pawl.Engine.Resolve.Slots.playerRefPlayers,
    -- Pawl.Engine.PlayerEffect.playersInScope) answer off Game.stillPlaying,
    -- which CR 102.1 has already narrowed to the players still in the game.
    AnyPlayer
  | -- | CR 102.3's teammates: the OTHER players on the perspective's team, so
    -- never the perspective itself and nobody in CR 102.4's game without teams.
    -- CR 804.2's "target teammate" (Pawl.Engine.Deploy).
    Teammate
  | -- | CR 102.4's "your team": you and/or your teammates, which in a game not
    -- played between teams is just you. Pir, Imaginative Rascal's "a permanent
    -- your team controls"; Pawl.TeamSpec's Pir group proves it.
    YourTeam
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Does @candidate@ stand in this relation to @you@, the perspective? The one
-- definition of what each arm MEANS, so a new arm is answered once rather than at
-- every reader -- and the readers are spread across Pawl.Engine.Filter,
-- Pawl.Engine.Event, Pawl.Engine.Count, Pawl.Engine.Replacement,
-- Pawl.Engine.PlayerEffect and Pawl.Engine.Resolve.
--
-- Sits beside the type for the reason Pawl.Types.Recipient.objectOf does: it is a
-- fact about what the shape means rather than about the board -- it reads no game
-- state beyond the roster of teams, which CR 800.2 settles before the game begins
-- -- and callers on both sides of the module graph need it.
--
-- A departed candidate is judged like any other -- a trigger on "an opponent
-- loses the game" has to recognise the loser -- so narrowing to the players
-- still in the game is a set-folding caller's job, not this predicate's.
holds :: Teams.Teams -> PlayerRelation -> PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
holds teams relation you candidate = case relation of
  You -> candidate == you
  Opponent -> Teams.areOpponents teams you candidate
  AnyPlayer -> True
  Teammate -> Teams.sameTeam teams you candidate
  YourTeam -> candidate == you || Teams.sameTeam teams you candidate

-- | Does this relation hold whoever the perspective is -- so that a reader with
-- no CR 109.5 "you" to supply can still answer it? Only AnyPlayer, which asks
-- the perspective nothing.
perspectiveFree :: PlayerRelation -> Bool
perspectiveFree relation = case relation of
  You -> False
  Opponent -> False
  AnyPlayer -> True
  Teammate -> False
  YourTeam -> False

-- | 'holds' where either player may be absent: CR 109.5's "you" with nobody to
-- be, or a candidate naming no player. Only a 'perspectiveFree' relation holds
-- then; every other arm compares the two and has nothing to compare.
holdsFor :: Teams.Teams -> PlayerRelation -> Maybe PlayerId.PlayerId -> Maybe PlayerId.PlayerId -> Bool
holdsFor teams relation you candidate = case (you, candidate) of
  (Just y, Just c) -> holds teams relation y c
  _ -> perspectiveFree relation
