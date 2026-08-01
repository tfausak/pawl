module Pawl.Types.Recipient where

import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)

-- | CR 510.1: combat damage is assigned to a blocking creature or to the player
-- being attacked. Grows toward battles (#302), the remaining attack target. Ord
-- because it is a Map key.
--
-- Bolt targeting a player is the second consumer (M3a): "defender" was combat's
-- name for this recipient, not the type's meaning, so ToPlayer names the object.
data Recipient
  = ToCreature ObjectId
  | -- | CR 115.4's third kind of "any target". A tag of its own rather than a
    -- ToObject, because CR 120.3c gives damage to a planeswalker a DIFFERENT
    -- result from CR 120.3e's marked damage -- so the classification that says
    -- which one applies is made once, where the recipient is built, and
    -- Pawl.Engine.Damage.applyDamage reads it off the tag rather than
    -- re-projecting the object's card types at the moment it applies.
    --
    -- Nothing declares an attack on one yet (CR 306.6, #493), so today this is a
    -- noncombat-damage tag only.
    ToPlaneswalker ObjectId
  | ToPlayer PlayerId
  | -- | A spell on the stack or a permanent, named generically (Magical Hack's
    -- "target spell or permanent", the fixture's "target land"). Text-changing
    -- does not care about creature-ness, so it does not reuse ToCreature.
    ToObject ObjectId
  deriving (Eq, Ord, Show)

-- | The object a recipient names, if any -- Nothing for a player, which CR 115.1
-- makes a recipient in its own right ("the targets are object(s) and/or
-- player(s)") rather than a missing object. CR 704.5n asks exactly this question
-- of an Equipment's host. The one projection of this type worth naming, because the
-- three object arms are the same answer to it and every caller that wants "which
-- object" would otherwise re-case on all of them. Sits beside the type for the reason
-- Binding.empty does: it is a fact about the shape, not about the game, and
-- callers on both sides of the module graph need it (Pawl.Engine.Projection cannot
-- import Pawl.Engine.Resolve, where this used to live).
objectOf :: Recipient -> Maybe ObjectId
objectOf r = case r of
  ToCreature oid -> Just oid
  ToPlaneswalker oid -> Just oid
  ToObject oid -> Just oid
  ToPlayer _ -> Nothing
