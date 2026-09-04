module Pawl.Types.OutsideObject where

import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 729.4: one card in a game that is on hold, as the game being played sees
-- it -- "all objects in the main game ... are considered outside the subgame".
-- Keyed in GameState.outsideObjects by the id the card has OUT THERE, which is
-- the only handle the outer frame needs to apply the departure when this game
-- ends.
--
-- No characteristics of its own beyond the status below: CR 729.1b gives a
-- main-game effect no meaning here, so the card is read from its PRINTED FACE
-- (Pawl.Engine.Projection.View.viewOfCard), exactly as CR 103.2a's sideboard pool is.
-- The owner is carried because CR 108.3b scopes every reach outside the game to
-- the acting player's OWN cards.
data OutsideObject = MkOutsideObject
  { owner :: PlayerId.PlayerId,
    printing :: PrintingId.PrintingId,
    -- | CR 110.5: the out-there object's face-up/face-down STATUS, which is the
    -- one thing about it CR 729.1b does not keep out. A status is not an effect
    -- or a definition -- CR 708.2 makes the listed characteristics the object's
    -- own COPIABLE VALUES -- so it travels with the object the way ownership
    -- does, and Pawl.Engine.Event.eligible reads a face-down entry
    -- through Pawl.Engine.Card.faceDownFace rather than through the printing.
    --
    -- Only `eligible` reads it. What CROSSES is the card, face up: CR 708.9 has
    -- the owner reveal a face-down permanent as it leaves the battlefield, and
    -- CR 400.7 makes the card that arrives in the other game a new object.
    facing :: Facing.Facing
  }
  deriving (Eq, Ord, Show)
