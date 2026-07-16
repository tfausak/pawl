module Pawl.Action where

import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import Pawl.Type.Action (Action)
import qualified Pawl.Type.Action as Action
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Phase as Phase
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

isMainPhase :: Phase.Phase -> Bool
isMainPhase phase = case phase of
  Phase.PrecombatMain -> True
  Phase.PostcombatMain -> True
  _ -> False

playableLands :: PlayerId -> GameState -> [ObjectId]
playableLands pid gs =
  let isLandObject :: ObjectId -> Bool
      isLandObject oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isLand (Printing.card printing)
        Nothing -> False
   in filter isLandObject (Game.zoneMembers Zone.Hand pid gs)

legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let canPlayLand =
        isMainPhase (GameState.phase gs)
          && GameState.activePlayer gs == pid
          && not (Set.member pid (GameState.landPlayed gs))
      lands = if canPlayLand then map Action.Play (playableLands pid gs) else []
   in Action.Pass : lands
