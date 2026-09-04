module Pawl.Types.CopyException where

import qualified Data.Set as Set
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness

-- | CR 707.9: one exception to the copying process, the "except ..." clause of a
-- copy effect. Quicksilver Gargantuan's "except it's 7/7" is CR 707.9d's own
-- worked example; Dack's Duplicate's "except it has haste and dethrone" is CR
-- 707.9a's.
--
-- A LIST of these rides EntryRewrite.AsCopy rather than one: the printed clauses
-- are joined by "and" (Moritte of the Frost states three), and CR 707.9f reads
-- "any other exceptions that effect includes" -- plural, of one effect.
--
-- Every arm writes into the COPIABLE snapshot, never into a CR 613 layer, which
-- is what CR 707.9a and CR 707.9b both require: the excepted value or ability
-- "becomes part of the copiable values" of the copy. A token copy of the copy
-- therefore inherits the excepted value with no further machinery, where a
-- layer-7b write on the object would be left behind (CR 707.2's exclusion of
-- "other effects").
--
-- Not implemented: CR 707.9c's exception that declines to copy a characteristic,
-- CR 707.9d's "in addition to its other types" carve-out, and CR 707.9e's
-- exception that is an additional effect rather than a characteristic (#1292).
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
    SetPowerToughness SetPowerToughness.SetPowerToughness
  | -- | CR 707.9a: the copy gains these abilities as part of the copying process
    -- ("except it has haste and dethrone"), so they join the copiable values.
    --
    -- A Set, and one arm for the whole clause rather than one per keyword: the
    -- printed sentence names them joined by "and" (Dack's Duplicate states two).
    --
    -- CR 604.3a(2) makes an ability acquired this way CHARACTERISTIC-DEFINING,
    -- which falls out of writing it into the snapshot: the copy's keywords are
    -- in place before layer 4, so Pawl.Engine.Projection.applySubtypeDefining
    -- picks up an excepted changeling at CR 613.3's start-of-layer moment rather
    -- than in timestamp order (Omni-Changeling).
    GainKeywords (Set.Set Keyword.Keyword)
  deriving (Eq, Ord, Show)
