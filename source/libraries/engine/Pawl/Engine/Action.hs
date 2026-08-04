module Pawl.Engine.Action where

import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Turn as Turn
import Pawl.Types.Action (Action)
import qualified Pawl.Types.Action as Action
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Zone as Zone

playableLands :: PlayerId -> GameState -> [ObjectId]
playableLands pid gs =
  let isLandObject oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isLand (Card.combined (Printing.card printing))
          Source.OfToken card -> Card.isLand (Card.combined card)
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
          Source.OfEmblem _ -> False
          Source.OfInherentTrigger _ _ -> False
        Nothing -> False
   in filter isLandObject (Game.zoneMembers Zone.Hand pid gs)

legalActions :: PlayerId -> GameState -> [Action]
legalActions pid gs =
  let -- CR 702.8a's window says "play this card", which reaches a land as well
      -- as a spell; this gate does not consult the keyword, so a land card with
      -- flash would still be playable only at sorcery speed (#566).
      canPlayLand =
        Turn.isMainPhase (GameState.phase gs)
          && GameState.activePlayer gs == pid
          && not (Set.member pid (GameState.landPlayed gs))
      lands = if canPlayLand then fmap Action.Play (playableLands pid gs) else []
      spells = fmap Action.Cast (Cast.castableSpells pid gs)
      -- CR 702.29a: a HAND is a source of activations too, not just the
      -- battlefield -- cycling functions only while the card is in a player's
      -- hand. Which abilities an
      -- object offers from where is Activate.abilitiesFor's question; this list
      -- only says where to look, and the two zones are disjoint.
      --
      -- ONE control-grant walk and ONE whole-board projection for the whole
      -- enumeration, threaded into both halves of the loop. Each object would
      -- otherwise be projected three times plus a grant walk for its
      -- controller, at a boundary the priority loop reaches on every pass;
      -- sharing them rested on GHC's CSE before and is now stated (#200, #315,
      -- #316). The board is a snapshot of this one `gs` and this is a pure
      -- function of it, so nothing can move between the projection and its
      -- uses.
      activations =
        let grants = Projection.controlGrants gs
            pcs = Projection.projectAll gs
            forObject oid =
              fmap (Action.Activate oid) (filter (\ab -> Activate.activatableGiven grants pcs pid oid ab gs) (Activate.abilitiesForGiven pcs oid gs))
         in concatMap forObject (Projection.controlsGiven grants pid gs <> Game.zoneMembers Zone.Hand pid gs)
   in Action.Pass : lands <> spells <> activations
