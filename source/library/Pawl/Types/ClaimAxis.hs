module Pawl.Types.ClaimAxis where

import qualified Pawl.Types.Zone as Zone

-- | WHICH resource a Pawl.Types.Claim contends for. Two claims contend only when
-- they name the same axis, and that is what keeps a cost's tapping half apart
-- from its sacrificing half: CR 601.2h pays a cost's parts "in any order" and
-- both are performed, so Springleaf Drum may tap the creature Village Rites then
-- sacrifices.
data ClaimAxis
  = -- | The objects one zone holds, which a payment takes OUT of it: CR 701.21a's
    -- sacrifice, CR 601.2f's discard, CR 406.2's exile from a graveyard. The ZONE
    -- alone keys it even though a hand and a graveyard are per-player (CR 400.3,
    -- CR 108.4), for the reason Pawl.Engine.Cost.claimOf gives: every claim pawl
    -- builds is on one player's own copy of the zone.
    Removal Zone.Zone
  | -- | The UNTAPPED permanents on the battlefield, which CR 601.2f's "tapping
    -- permanents" spends without moving anything. A permanent tapped by one
    -- payment is not there to be tapped by the next (CR 118.3's own example), and
    -- it is on nobody's removal axis, since it never left the battlefield.
    Tapping
  deriving (Eq, Ord, Show)
