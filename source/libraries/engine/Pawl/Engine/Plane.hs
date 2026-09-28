-- | CR 311 and CR 312, plane and phenomenon cards: which objects are planar
-- cards, and who controls one that is face up.
--
-- A card type and not a format, Pawl.Engine.Vanguard's posture: the planar
-- controller is read off the game, and a card of either type announces itself.
module Pawl.Engine.Plane where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 311.1 / 312.1: does this face print the plane or the phenomenon card
-- type? A classification over the printed type line.
isPlanarFace :: Face.Face Card.Card -> Bool
isPlanarFace face =
  let types = TypeLine.types (Face.typeLine face)
   in Set.member CardType.Plane types || Set.member CardType.Phenomenon types

-- | CR 312.1: does this face print the phenomenon card type?
isPhenomenonFace :: Face.Face Card.Card -> Bool
isPhenomenonFace face = Set.member CardType.Phenomenon (TypeLine.types (Face.typeLine face))

-- | CR 311.2 / 312.2: is this object a plane or phenomenon card? Read off the
-- printed face, for Pawl.Engine.Vanguard.isVanguard's reason: the card never
-- leaves the command zone, where nothing can copy it or change its type.
isPlanarCard :: ObjectId -> GameState -> Bool
isPlanarCard oid gs = maybe False isPlanarFace (Game.faceOf oid gs)

-- | CR 311.5 / 312.4 / 901.6: the planar controller, normally the active
-- player. While that seat has left (CR 800.4j keeps it active), CR 800.4p's
-- heir: the next still-playing seat in turn order. Derived, not stored: the
-- designation moves only off a departing holder and reverts at the next turn,
-- so the walk from the active seat always finds it. Pawl.PlanechaseSpec's "CR
-- 311.5 a departing active player's plane is replaced from the next seat's
-- planar deck" proves it.
planarController :: GameState -> PlayerId
planarController gs =
  let active = GameState.activePlayer gs
      playing = Game.stillPlaying gs
      order = GameState.turnOrder gs
      after = drop 1 (List.dropWhile (/= active) order) <> order
   in if List.elem active playing then active else Maybe.fromMaybe active (List.find (`List.elem` playing) after)

-- | CR 109.4 for a command-zone object: the planar controller controls a
-- face-up plane or phenomenon card (CR 901.6), and the owner controls
-- everything else there (CR 114.2, CR 902.6).
commandControllerOf :: ObjectId -> Object.Object -> GameState -> PlayerId
commandControllerOf oid obj gs
  | isPlanarCard oid gs = planarController gs
  | otherwise = Object.owner obj
