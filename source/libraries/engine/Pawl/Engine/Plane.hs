-- | CR 311 and CR 312, plane and phenomenon cards: which objects are planar
-- cards, and who controls one that is face up.
--
-- A card type and not a format, Pawl.Engine.Vanguard's posture: the planar
-- controller is read off the game, and a card of either type announces itself.
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 311.5's retention: a planar controller who would leave the game hands
--     the designation to the next player in turn order, who keeps it until the
--     active player changes (#4311).
module Pawl.Engine.Plane where

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
-- player.
planarController :: GameState -> PlayerId
planarController = GameState.activePlayer

-- | CR 109.4 for a command-zone object: the planar controller controls a
-- face-up plane or phenomenon card (CR 901.6), and the owner controls
-- everything else there (CR 114.2, CR 902.6).
commandControllerOf :: ObjectId -> Object.Object -> GameState -> PlayerId
commandControllerOf oid obj gs
  | isPlanarCard oid gs = planarController gs
  | otherwise = Object.owner obj
