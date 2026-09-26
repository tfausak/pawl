module Pawl.Types.ZonePair where

import qualified Pawl.Types.Zone as Zone

-- | The two zones of one player that CR 701.12f's "exchange two zones" swaps.
-- Unordered and never the same zone twice, so each printed pair is one
-- constructor. Both are the player's own zones (CR 400.3), which is what meets
-- CR 701.12d's "owned by the same player".
data ZonePair
  = -- | Harness Infinity's "exchange your hand and graveyard".
    HandAndGraveyard
  | -- | Morality Shift's "exchange your graveyard and library".
    GraveyardAndLibrary
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The pair as two zones, in no meaningful order.
zones :: ZonePair -> (Zone.Zone, Zone.Zone)
zones pair = case pair of
  HandAndGraveyard -> (Zone.Hand, Zone.Graveyard)
  GraveyardAndLibrary -> (Zone.Graveyard, Zone.Library)
