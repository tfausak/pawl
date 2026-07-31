module Pawl.Types.Recipient where

import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

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

-- The object a recipient names, if any -- Nothing for a player (CR 612 targets a
-- spell or permanent, not a player; CR 704.5n asks the same question of an
-- Equipment's host). The one projection of this type worth naming, because the
-- two object arms are the same answer to it and every caller that wants "which
-- object" would otherwise re-case on both. Sits beside the type for the reason
-- Binding.empty does: it is a fact about the shape, not about the game, and
-- callers on both sides of the module graph need it (Pawl.Projection cannot
-- import Pawl.Resolve, where this used to live).
objectOf :: Recipient -> Maybe ObjectId
objectOf r = case r of
  ToCreature oid -> Just oid
  ToObject oid -> Just oid
  ToPlayer _ -> Nothing
