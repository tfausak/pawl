module Pawl.Types.Recipient where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 510.1: combat damage is assigned to a blocking creature, or to the player,
-- planeswalker or battle being attacked. Ord because it is a Map key.
data Recipient
  = ToCreature ObjectId.ObjectId
  | -- | CR 115.4's third kind of "any target". A tag of its own rather than a
    -- ToObject, because CR 306.6 and CR 510.1b make an attacked planeswalker a
    -- combat recipient in its own right, and because CR 115.4's pool is not CR
    -- 110.1's. It is NOT which of CR 120.3's results the damage has: a permanent
    -- may hold more than one of CR 120.1a's card types at once, so
    -- Pawl.Engine.Damage.damagedCardTypes reads the recipient's projected card
    -- types as the damage is applied and this tag is only one input to it.
    ToPlaneswalker ObjectId.ObjectId
  | -- | CR 115.4's fourth kind of "any target", and ToPlaneswalker's twin one card
    -- type over: CR 310.1 battles are their own pool, and combat produces the tag
    -- through CR 510.1b's AttackTarget.OfBattle. CR 120.3h's defense-counter
    -- removal is settled the same way the planeswalker arm's is, off the
    -- projection rather than off the tag.
    ToBattle ObjectId.ObjectId
  | ToPlayer PlayerId.PlayerId
  | -- | A spell on the stack or a permanent, named generically (Magical Hack's
    -- "target spell or permanent", the fixture's "target land"). Text-changing
    -- does not care about creature-ness, so it does not reuse ToCreature.
    ToObject ObjectId.ObjectId
  deriving (Eq, Ord, Show)

-- | The object a recipient names, if any -- Nothing for a player, which CR 115.1
-- makes a recipient in its own right rather than a missing object. Sits beside
-- the type for the reason Binding.empty does: it is a fact about the shape, not
-- about the game, and callers on both sides of the module graph need it.
objectOf :: Recipient -> Maybe ObjectId.ObjectId
objectOf r = case r of
  ToCreature oid -> Just oid
  ToPlaneswalker oid -> Just oid
  ToBattle oid -> Just oid
  ToObject oid -> Just oid
  ToPlayer _ -> Nothing

-- | The player a recipient names, if any -- 'objectOf''s mirror, Nothing for
-- every object arm. Read by Pawl.Engine.Binding.playerSlots for CR 603.2's
-- "that player", and exhaustive for objectOf's reason: a new arm must break this
-- build rather than answer Nothing by default.
playerOf :: Recipient -> Maybe PlayerId.PlayerId
playerOf r = case r of
  ToPlayer pid -> Just pid
  ToCreature _ -> Nothing
  ToPlaneswalker _ -> Nothing
  ToBattle _ -> Nothing
  ToObject _ -> Nothing
