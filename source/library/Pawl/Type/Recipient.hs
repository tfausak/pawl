module Pawl.Type.Recipient where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 510.1: combat damage is assigned to a blocking creature or to the player
-- being attacked. Grows toward planeswalkers and battles (other attack targets)
-- when those card types exist; ToDefender is all M2c's single-opponent,
-- planeswalker-free board can produce. Ord because it is a Map key.
data Recipient
  = ToCreature ObjectId
  | ToDefender PlayerId
  deriving (Eq, Ord, Show)
