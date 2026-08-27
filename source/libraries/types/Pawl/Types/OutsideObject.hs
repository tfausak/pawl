module Pawl.Types.OutsideObject where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 729.4: one card in a game that is on hold, as the game being played sees
-- it -- "all objects in the main game ... are considered outside the subgame".
-- Keyed in GameState.outsideObjects by the id the card has OUT THERE, which is
-- the only handle the outer frame needs to apply the departure when this game
-- ends.
--
-- Two fields and no characteristics: CR 729.1b gives a main-game effect no
-- meaning here, so the card is read from its PRINTED FACE
-- (Pawl.Engine.Projection.viewOfCard), exactly as CR 103.2a's sideboard pool is.
-- The owner is carried because CR 108.3b scopes every reach outside the game to
-- the acting player's OWN cards.
data OutsideObject = MkOutsideObject
  { owner :: PlayerId.PlayerId,
    printing :: PrintingId.PrintingId
  }
  deriving (Eq, Ord, Show)
