module Pawl.Type.Recipient where

import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 510.1: combat damage is assigned to a blocking creature or to the player
-- being attacked. Grows toward planeswalkers and battles (other attack targets)
-- when those card types exist; ToPlayer is all M2c's single-opponent,
-- planeswalker-free board can produce. Ord because it is a Map key.
--
-- Bolt targeting a player is the second consumer (M3a): "defender" was combat's
-- name for this recipient, not the type's meaning, so ToPlayer names the object.
data Recipient
  = ToCreature ObjectId
  | ToPlayer PlayerId
  | -- A spell on the stack or a permanent, named generically (Magical Hack's
    -- "target spell or permanent", the fixture's "target land"). Text-changing
    -- does not care about creature-ness, so it does not reuse ToCreature.
    ToObject ObjectId
  deriving (Eq, Ord, Show)
