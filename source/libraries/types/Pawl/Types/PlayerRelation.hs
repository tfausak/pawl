module Pawl.Types.PlayerRelation where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Teams as Teams

-- | How a player a card names -- an object's controller, a trigger event's
-- player -- stands to the perspective the evaluation carries (the source's
-- controller when targeting; the effect's controller for a continuous effect).
-- CR 109.5 fixes "you" as the object's controller; Opponent
-- is CR 102.3's every player not on the perspective's team, which is every other
-- player in CR 102.4's game with no teams -- CR 806.1's free-for-all reading and
-- CR 102.2's two-player one, the same predicate either way.
-- Resolved at Pawl.Engine.Count.playersFor and Pawl.Engine.Filter.matches, both
-- of which go through 'holds' below.
data PlayerRelation
  = You
  | Opponent
  | -- | CR 102.1's bare "a player" -- every player in the game, the perspective
    -- INCLUDED. The union of the two arms above, which is not derivable from
    -- either: You and Opponent partition the table, so a card printing "whenever
    -- a player loses life" (The Master of Lake-town's) says something neither
    -- states and both are observably narrower than.
    --
    -- Perspective-free, and the only arm that is: the two above compare a
    -- candidate against CR 109.5's "you", and this one asks nothing about the
    -- candidate at all. It is still carried as a relation rather than hoisted out
    -- of the type, because the card text it transcribes sits in exactly the
    -- position the other two do -- a trigger condition's payload, a filter's atom
    -- -- and every reader already has the relation in hand.
    --
    -- Not a licence to name a DEPARTED seat. As a predicate this arm judges a
    -- candidate the caller already holds, and the callers that fold a player SET
    -- instead (Pawl.Engine.Count.playersFor, Pawl.Engine.Resolve.Slots.playerRefPlayers)
    -- answer off Game.stillPlaying, which CR 102.1 has already narrowed to the
    -- players still in the game -- the roster Opponent is filtered from there.
    AnyPlayer
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Does @candidate@ stand in this relation to @you@, the perspective? The one
-- definition of what each arm MEANS, so a new arm is answered once rather than at
-- every reader -- and the readers are spread across Pawl.Engine.Filter,
-- Pawl.Engine.Event, Pawl.Engine.Count and Pawl.Engine.Resolve.
--
-- Sits beside the type for the reason Pawl.Types.Recipient.objectOf does: it is a
-- fact about what the shape means rather than about the board -- it reads no game
-- state beyond the roster of teams, which CR 800.2 settles before the game begins
-- -- and callers on both sides of the module graph need it.
holds :: Teams.Teams -> PlayerRelation -> PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
holds teams relation you candidate = case relation of
  You -> candidate == you
  Opponent -> Teams.areOpponents teams you candidate
  AnyPlayer -> True
