module Pawl.Action where

import qualified Data.Set as Set
import qualified Pawl.Activate as Activate
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Turn as Turn
import Pawl.Type.Action (Action)
import qualified Pawl.Type.Action as Action
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone

playableLands :: PlayerId -> GameState -> [ObjectId]
playableLands pid gs =
  let isLandObject oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isLand (Printing.card printing)
          Source.OfToken card -> Card.isLand card
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
        Nothing -> False
   in filter isLandObject (Game.zoneMembers Zone.Hand pid gs)

legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let canPlayLand =
        Turn.isMainPhase (GameState.phase gs)
          && GameState.activePlayer gs == pid
          && not (Set.member pid (GameState.landPlayed gs))
      lands = if canPlayLand then fmap Action.Play (playableLands pid gs) else []
      spells = fmap Action.Cast (Cast.castableSpells pid gs)
      -- CR 702.29a: a HAND is a source of activations too, not just the
      -- battlefield -- that rule's cycling "functions only while the card with
      -- cycling is in a player's hand". Which abilities an object offers from
      -- where is Activate.abilitiesFor's question, not this one's; this list only
      -- says where to look. The two are disjoint, since an object is in exactly
      -- one zone.
      activations =
        let forObject oid =
              fmap (Action.Activate oid) (filter (\ab -> Activate.activatable pid oid ab gs) (Activate.abilitiesFor oid gs))
         in concatMap forObject (Projection.controls pid gs <> Game.zoneMembers Zone.Hand pid gs)
   in Action.Pass : lands <> spells <> activations
