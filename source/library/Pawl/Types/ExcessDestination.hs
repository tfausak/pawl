module Pawl.Types.ExcessDestination where

-- | CR 120.4a: where the excess damage an effect would deal to a permanent goes
-- INSTEAD. Rides the instruction that deals the damage
-- (Pawl.Types.DealDamage), because CR 120.4a's own subject is "an effect that's
-- causing damage to be dealt states that excess damage ... is dealt to another
-- permanent or player instead" -- the effect states it, so the effect carries
-- it.
--
-- Not a Bool, for Pawl.Types.ManaRetention's reason: CR 120.4a's own words are
-- "another permanent or player", so the thing being written down is a
-- DESTINATION, and a Bool would have to be renamed the day the pool prints one
-- that is not the controller.
--
-- Not a Pawl.Types.ObjectRef, which is the vocabulary for the objects an effect
-- READS. The destination is derived from the damage event being rewritten -- it
-- is that permanent's controller -- rather than named by the card as a slot or a
-- filter, so an ObjectRef here would be a second, unbound thing to resolve.
--
-- Not a field on Pawl.Types.DamageEvent. Combat damage can never carry the
-- instruction: CR 510.1's assignment is not an effect, and CR 120.4a's rewrite
-- only happens when an effect states it.
data ExcessDestination
  = -- | "Excess damage is dealt to that creature's controller instead" -- Flame
    -- Spill's sentence, and the only destination this type has an arm for. The
    -- printings that would want a second one are what a Scryfall
    -- @oracle:"excess damage"@ search (2026-08-18) answers: Gandalf's Sanction,
    -- Pigment Storm, Ravenous Tyrannosaurus and Ram Through, which write this
    -- same destination, Ram Through behind a trample gate. Re-run that query
    -- rather than inheriting this list. The player is read off the damaged
    -- permanent, so nothing on the card names them.
    ToRecipientController
  deriving (Bounded, Enum, Eq, Ord, Show)
