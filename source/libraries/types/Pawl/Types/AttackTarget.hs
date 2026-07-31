module Pawl.Types.AttackTarget where

import Pawl.Types.PlayerId (PlayerId)

-- What an attacking creature is attacking (CR 508.1).
--
-- Grows: OfPlaneswalker (CR 306.6, #493), OfBattle (CR 310.5, #302). A newtype
-- only because OfPlayer is the only case today; it becomes a `data` when the
-- second lands. The planeswalker CARD TYPE now exists (Jace Beleren), so the
-- first of those is no longer waiting on anything but the combat work itself.
newtype AttackTarget = OfPlayer PlayerId
  deriving (Eq, Ord, Show)
