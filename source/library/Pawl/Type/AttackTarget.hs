module Pawl.Type.AttackTarget where

import Pawl.Type.PlayerId (PlayerId)

-- What an attacking creature is attacking (CR 508.1).
--
-- Grows: OfPlaneswalker (CR 306.6, #301), OfBattle (CR 310.5, #302). A newtype
-- only because OfPlayer is the only case today; it becomes a `data` when the
-- second lands.
newtype AttackTarget = OfPlayer PlayerId
  deriving (Eq, Ord, Show)
