module Pawl.Departure where

import Control.Applicative ((<|>))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import Pawl.Type.Departure (Departure)
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status

-- CR 104.2a / 104.3: who is still in the game, and what happens when someone
-- leaves it.
--
-- Split out of Pawl.Sba because leaving is not always a state-based action. CR
-- 104.3b (life <= 0) is one and arrives through the SBA pass; CR 104.3a (concede)
-- is IMMEDIATE and Pawl.Engine reaches it directly. Having Engine call into
-- Pawl.Sba for something the rules say is not a state-based action would misstate
-- the rules in the module graph.
stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in fmap fst (filter isPlaying (Map.toList (GameState.players gs)))

-- Mark a player as having left, with the reason they left. Pure, because the SBA
-- pass folds it over several players before recomputing the outcome once.
depart :: Departure -> PlayerId -> GameState -> GameState
depart reason pid gs =
  let lose p = p {Player.status = Status.Departed reason}
   in gs {GameState.players = Map.adjust lose pid (GameState.players gs)}

-- CR 104.2a: "A player still in the game wins the game if that player's opponents
-- have all left the game."
--
-- `gs` is the state AFTER the departures have been applied, so the survivors are
-- `stillPlaying gs`. `leaving` is who just left, and is needed only to tell
-- "nobody is playing because they all left at once" (a draw) from "nobody was
-- playing to begin with" (no result at all).
outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result
outcomeAfterLeaving leaving gs = case stillPlaying gs of
  [winner] -> Just (Result.Won winner)
  [] -> if null leaving then Nothing else Just Result.Drawn
  _ -> Nothing

-- CR 104.3a: leave the game IMMEDIATELY, and settle CR 104.2a right now rather
-- than at the next state-based action check -- which is the whole distinction
-- between 104.3a and 104.3b. An already-decided result is kept rather than
-- overwritten: CR 104.1 says the game already ended the moment a result was
-- set, and CR 104.2a's "overrides all effects that would preclude that player
-- from winning" describes the win itself, not a license to replace a result
-- the game already has. Pawl.Sba's pass settles its own outcome the same way.
leaveGame :: Departure -> PlayerId -> Game ()
leaveGame reason pid = State.modify' $ \gs ->
  let departed = depart reason pid gs
   in departed {GameState.result = GameState.result departed <|> outcomeAfterLeaving [pid] departed}
