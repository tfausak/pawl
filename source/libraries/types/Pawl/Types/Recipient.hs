module Pawl.Types.Recipient where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 510.1: combat damage is assigned to a blocking creature, or to the player,
-- planeswalker or battle being attacked. Ord because it is a Map key.
data Recipient
  = ToCreature ObjectId.ObjectId
  | -- | CR 115.4's third kind of "any target". A tag of its own rather than a
    -- ToObject, because CR 120.3c gives damage to a planeswalker a different
    -- result from CR 120.3e's marked damage: the classification is made once,
    -- where the recipient is built, so Pawl.Engine.Damage.applyDamage reads the
    -- tag rather than re-projecting card types as it applies. Combat produces
    -- this tag too (CR 306.6, CR 510.1b), and CR 306.8 is the loyalty removal
    -- applyDamage then performs.
    ToPlaneswalker ObjectId.ObjectId
  | -- | CR 115.4's fourth kind of "any target", and ToPlaneswalker's twin one card
    -- type over: CR 120.3h gives damage dealt to a battle its own result -- that
    -- many defense counters come off (CR 310.6) -- so the classification is made
    -- once, where the recipient is built, and Pawl.Engine.Damage.applyDamage reads
    -- the tag. Combat produces it too, through CR 510.1b's AttackTarget.OfBattle.
    --
    -- A permanent that is both a battle and a creature would answer to CR 120.3e
    -- and CR 120.3h at once, and one Recipient cannot carry both (#503). No
    -- printing in the pool is both.
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
