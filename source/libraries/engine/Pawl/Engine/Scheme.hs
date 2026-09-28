-- | CR 314, scheme cards: which objects are schemes, and which are ongoing.
--
-- A card type and not a format, Pawl.Engine.Vanguard's posture: the archenemy
-- is whoever brought a scheme deck (Pawl.Engine.Archenemy).
module Pawl.Engine.Scheme where

import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 314.1: does this face print the scheme card type?
isSchemeFace :: Face.Face Card.Card -> Bool
isSchemeFace face = Set.member CardType.Scheme (TypeLine.types (Face.typeLine face))

-- | CR 314.2: is this object a scheme card? Read off the printed face, for
-- Pawl.Engine.Vanguard.isVanguard's reason: the card never leaves the command
-- zone, where nothing can copy it or change its type.
isScheme :: ObjectId -> GameState -> Bool
isScheme oid gs = maybe False isSchemeFace (Game.faceOf oid gs)

-- | CR 205.4h: is this scheme ongoing?
isOngoing :: ObjectId -> GameState -> Bool
isOngoing oid gs = maybe False (Set.member Supertype.Ongoing . TypeLine.supertypes . Face.typeLine) (Game.faceOf oid gs)
