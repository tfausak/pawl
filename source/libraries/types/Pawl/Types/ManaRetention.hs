module Pawl.Types.ManaRetention where

-- | CR 106.4: whether the player LOSES this mana as a step or phase ends. Rides
-- one unit of mana (Pawl.Types.ManaUnit) and the instruction that added it
-- (Pawl.Types.ManaAddition); Shizuko, Caller of Autumn's "until end of turn,
-- they don't lose this mana as steps and phases end" is the printing.
--
-- Not a Bool, for Pawl.Types.Optionality's reason: @UntilEndOfTurn@ says which
-- rule is in play -- CR 514.2's -- where @True@ would say only that something is
-- different.
--
-- Not a Pawl.Types.ProductionTag. Pawl.Types.ManaUnit's haddock draws that line:
-- a production tag is an observable fact about the production EVENT that
-- Pawl.Engine.Mana.productionTagsGiven decides from the source's properties,
-- and retention comes from the effect's wording instead. A snow source makes
-- snow mana however the ability is worded; Shizuko's mana is retained because
-- the card says so and for no other reason.
--
-- Not a Pawl.Types.Duration, though @UntilEndOfTurn@ is that type's first
-- constructor and cites the same rule. Duration is the vocabulary of the
-- Pawl.Types.Expiry machinery: Pawl.Engine.Expiry.arm turns one into an Expiry
-- stamped on a STORED effect, and Pawl.Engine.Expiry's sweeps end it there. A
-- retained mana unit is not a stored effect -- it lives in a pool and is read by
-- Pawl.Engine.Mana.emptyManaPools -- so reusing Duration would mean a second
-- consumer that ignores five of its six arms, or a fake continuous effect per
-- unit of mana.
data ManaRetention
  = -- | CR 500.5: the mana empties with the rest of the pool as the step or
    -- phase ends.
    Ordinary
  | -- | CR 514.2: the mana survives every step and phase end until the cleanup
    -- step, where the retention itself ends (Pawl.Engine.Mana.endManaRetention)
    -- and that same step's own CR 500.5 sweep then takes the mana.
    UntilEndOfTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
