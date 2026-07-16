module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Type.Departure as Departure
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status

stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in map fst (filter isPlaying (Map.toList (GameState.players gs)))

-- CR 704.5a (life <= 0) and CR 704.5b (drawing from an empty library).
losesNow :: GameState -> PlayerId -> Bool
losesNow gs pid = case Map.lookup pid (GameState.players gs) of
  Nothing -> False
  Just player ->
    Player.status player == Status.Playing
      && (Player.life player <= 0 || Set.member pid (GameState.drewFromEmpty gs))

depart :: PlayerId -> GameState -> GameState
depart pid gs =
  let lose p = p {Player.status = Status.Departed Departure.Lost}
   in gs {GameState.players = Map.adjust lose pid (GameState.players gs)}

checkStateBasedActions :: GameState -> GameState
checkStateBasedActions gs =
  let leaving = filter (losesNow gs) (stillPlaying gs)
      departed = foldr depart gs leaving
      remaining = stillPlaying departed
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
   in departed {GameState.result = outcome <|> GameState.result departed}
