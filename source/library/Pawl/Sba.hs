module Pawl.Sba where

import Control.Applicative ((<|>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Type.Departure as Departure
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Zone as Zone

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

-- CR 704.5f (toughness 0 or less) and CR 704.5g (damage marked >= toughness).
--
-- The isCreature guard is not redundant. Only creatures have printed toughness
-- today, so toughnessOf already implies it -- but a Vehicle (CR 301.7) has P/T
-- while not being a creature, and 704.5f/g must not touch it. Ask the
-- classification, never the identity.
creatureDies :: GameState -> ObjectId -> Bool
creatureDies gs oid =
  let isCreature = fmap Card.isCreature (Game.cardOf oid gs) == Just True
   in isCreature && case Game.toughnessOf oid gs of
        -- An unevaluable toughness means NO state-based action, not a crash.
        -- Unreachable in M1b (every toughness is a Literal); reachable at M3.
        Nothing -> False
        Just toughness ->
          -- CR 704.5f: toughness 0 or less.
          (toughness <= 0)
            -- CR 704.5g: damage marked is lethal.
            || case Game.lookupObject oid gs of
              Nothing -> False
              Just obj -> toInteger (Object.damage obj) >= toughness

-- CR 704.3 says to repeat until no state-based action is performed. One pass is
-- enough in M1b: a creature dying cannot cause another SBA, because nothing
-- gains or loses life when a creature dies. Revisit when it can.
checkStateBasedActions :: GameState -> GameState
checkStateBasedActions gs =
  let -- CR 704.5f/g are checked against the state BEFORE any of them apply: SBAs
      -- are simultaneous.
      dying = filter (creatureDies gs) (Set.toList (GameState.battlefield gs))
      bury g oid = Game.changeZone oid Zone.Graveyard g
      buried = List.foldl' bury gs dying
      leaving = filter (losesNow buried) (stillPlaying buried)
      departed = foldr depart buried leaving
      remaining = stillPlaying departed
      outcome = case remaining of
        [winner] -> Just (Result.Won winner)
        [] -> if null leaving then Nothing else Just Result.Drawn
        _ -> Nothing
   in departed {GameState.result = outcome <|> GameState.result departed}
