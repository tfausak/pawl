module Pawl.Engine.Ante where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.CardIdentity as CardIdentity
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameSettings as GameSettings
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Result as Result

-- | CR 407.3: when not playing for ante, an ante card "can't be brought into
-- the game from outside the game". Read off the printed face's flag, never its
-- text.
barred :: GameSettings.GameSettings -> Face.Face card -> Bool
barred settings face = Face.anteOnly face && not (GameSettings.ante settings)

-- | CR 108.3 / 407.3: every card in the game whose owner is not the player who
-- began the game owning it, as that player and its owner. A token, an emblem
-- and a copy carry no identity, so never appear.
--
-- Not implemented: a card that left the game with a departed owner (#4847).
ownershipChanges :: GameState.GameState -> Map.Map ObjectId (PlayerId, PlayerId)
ownershipChanges gs =
  let changed obj = case Object.identity obj of
        Just identity | CardIdentity.startingOwner identity /= Object.owner obj -> Just (CardIdentity.startingOwner identity, Object.owner obj)
        _ -> Nothing
   in Map.mapMaybe changed (GameState.objects gs)

-- | CR 407.2: "at the end of the game, the winner becomes the owner of all the
-- cards in the ante zone", as Departure.settle sets the result. A draw has no
-- winner (CR 104.4a), so every card keeps its owner.
--
-- Not implemented: a team win's payout (#4849).
payOut :: GameState.GameState -> GameState.GameState
payOut gs =
  let payTo winner g = Foldable.foldl' (\acc oid -> Game.setOwner oid winner acc) g (Set.toList (GameState.ante g))
   in case GameState.result gs of
        Just (Result.Won winner) -> payTo winner gs
        Just (Result.TeamWon _) -> gs
        Just Result.Drawn -> gs
        Nothing -> gs
