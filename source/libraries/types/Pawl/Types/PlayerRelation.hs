module Pawl.Types.PlayerRelation where

import qualified Pawl.Types.PlayerId as PlayerId

-- | Who an object's controller is, relative to the perspective the evaluation
-- carries (the source's controller when targeting; the effect's controller for a
-- continuous effect). CR 109.5 fixes "you" as the object's controller; Opponent
-- is every player who is not the perspective -- CR 806.1 in a free-for-all, CR
-- 102.2 in a two-player game, the same predicate either way. CR 102.3's teams
-- are the ONE reading it is wrong for, and pawl has none to express (#175).
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
    -- Not a licence to name a DEPARTED seat. This arm is a predicate over a
    -- candidate the caller already has; the callers that fold a player SET
    -- (Pawl.Engine.Count.playersFor, Pawl.Engine.Resolve.playerRefPlayers) filter
    -- it out of a roster CR 102.1 has already narrowed to the players still in the
    -- game, exactly as they do for Opponent.
    AnyPlayer
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Does @candidate@ stand in this relation to @you@, the perspective? The one
-- definition of what each arm MEANS, so a new arm is answered once rather than at
-- every reader -- and there are a dozen readers, across Pawl.Engine.Filter,
-- Pawl.Engine.Event, Pawl.Engine.Count and Pawl.Engine.Resolve.
--
-- Sits beside the type for the reason Pawl.Types.Recipient.objectOf does: it is a
-- fact about what the shape means rather than about the board -- it reads no game
-- state, only the two ids its caller has already resolved -- and callers on both
-- sides of the module graph need it.
holds :: PlayerRelation -> PlayerId.PlayerId -> PlayerId.PlayerId -> Bool
holds relation you candidate = case relation of
  You -> candidate == you
  Opponent -> candidate /= you
  AnyPlayer -> True
