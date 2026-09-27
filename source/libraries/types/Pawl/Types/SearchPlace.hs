module Pawl.Types.SearchPlace where

import qualified Pawl.Types.Zone as Zone

-- | CR 701.23a / 701.23j: one of the places a printed "and/or" lets a searcher
-- look through -- a zone, or the cards they own outside the game, which CR
-- 400.11 says is not a zone.
data SearchPlace
  = -- | A zone the search names (CR 701.23a).
    InZone Zone.Zone
  | -- | The cards the searcher owns outside the game (CR 701.23j).
    OutsideTheGame
  deriving (Eq, Ord, Show)
