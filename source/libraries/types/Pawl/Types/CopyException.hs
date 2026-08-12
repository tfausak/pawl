module Pawl.Types.CopyException where

-- | CR 707.9: one exception to the copying process, the "except ..." clause of a
-- copy effect. Quicksilver Gargantuan's "except it's 7/7" is the one producer
-- today, and it is CR 707.9d's own worked example.
--
-- A LIST of these rides EntryRewrite.AsCopy rather than one: the printed clauses
-- are joined by "and" (Moritte of the Frost states three), and CR 707.9f reads
-- "any other exceptions that effect includes" -- plural, of one effect.
--
-- Every arm writes into the COPIABLE snapshot, never into a CR 613 layer, which
-- is what CR 707.9b requires: "The final set of values for that characteristic
-- becomes part of the copiable values of the copy." A token copy of the copy
-- therefore inherits the excepted value with no further machinery, where a
-- layer-7b write on the object would be left behind (CR 707.2's exclusion of
-- "other effects").
--
-- Not implemented: CR 707.9a's exception that makes the copy GAIN an ability
-- ("except it has changeling"), CR 707.9c's exception that declines to copy a
-- characteristic, and CR 707.9d's "in addition to its other types" carve-out
-- (#1292).
data CopyException
  = -- | CR 707.9b: the copy's power and toughness are these numbers instead of
    -- the copied object's ("except it's 7/7").
    --
    -- Two Integers rather than a Quantity, the position
    -- Pawl.Types.EntryOption's own pair takes: every printing of this clause
    -- states two literals, and CR 614.12a settles the copy before the permanent
    -- enters, so there is no board for a variable to be measured against yet.
    --
    -- Applying this arm ALSO clears the copied characteristic-defining P/T,
    -- which is CR 707.9d: an effect that "provides a specific set of values for
    -- a certain characteristic" does not copy the CDA defining it. Without that
    -- half the CDA would win at layer 7a and a Gargantuan copying a Tarmogoyf
    -- would recompute rather than stay 7/7.
    SetPowerToughness Integer Integer
  deriving (Eq, Ord, Show)
