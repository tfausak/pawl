-- | CR 315, conspiracy cards: which objects and faces are conspiracies, the
-- classification CR 315.2's placement, CR 315.3's hold on the command zone and
-- CR 113.6p's functioning there all read.
--
-- A card type and not a format, Pawl.Engine.Vanguard's posture: every rule in
-- CR 315 is stated of a conspiracy CARD, so no GameSettings field says "this is a
-- Conspiracy Draft game".
--
-- WHAT IS NOT IMPLEMENTED:
--
--   * CR 315.2's face-down placement for hidden agenda, and CR 315.5b \/ 315.7's
--     face-down conspiracy (#3497).
module Pawl.Engine.Conspiracy where

import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.TypeLine as TypeLine

-- | CR 205.2a: does this face print the conspiracy card type? A classification
-- over the printed type line, never a case on which card it is.
isConspiracyFace :: Face.Face Card.Card -> Bool
isConspiracyFace face = Set.member CardType.Conspiracy (TypeLine.types (Face.typeLine face))

-- | CR 315.3: is this object a conspiracy card? Read off the printed face, for
-- Pawl.Engine.Vanguard.isVanguard's reason: CR 315.3 keeps it in the command
-- zone, where nothing can copy it or change its type.
isConspiracy :: ObjectId -> GameState -> Bool
isConspiracy oid gs = maybe False isConspiracyFace (Game.faceOf oid gs)
