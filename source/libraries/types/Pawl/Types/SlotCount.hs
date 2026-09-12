module Pawl.Types.SlotCount where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.TargetCount as TargetCount

-- | CR 601.2c: how many targets one instance of the word "target" takes, as the
-- CARD states it -- a range printed in the text, a range counted by the value of
-- X the caster announced one step earlier (CR 601.2b), or a range counted by a
-- number the board supplies.
--
-- A type of its own rather than a third arm inside Pawl.Types.TargetCount,
-- because the two speak at different moments. This is card data, written before
-- any announcement exists; a TargetCount is a range of NUMBERS, and every reader
-- of one -- Prompt.AnnounceTargets, Pawl.Engine.Replay, the announcement's own
-- clamp -- stands after CR 601.2b and needs no case for a value nobody has named
-- yet. `at` below is the one crossing.
data SlotCount
  = -- | CR 601.2c: the range the card prints ("up to two target creatures").
    Printed TargetCount.TargetCount
  | -- | CR 601.2c with CR 601.2b: exactly the announced X ("each of X target
    -- creatures", Rot-Curse Rakshasa).
    AnnouncedX
  | -- | CR 601.2c with CR 601.2b: zero through the announced X ("up to X target
    -- artifacts and/or enchantments", Pest Infestation).
    UpToAnnouncedX
  | -- | CR 601.2c: zero through a number the board computes ("up to X target
    -- creatures ... where X is your devotion to black", Mogis's Marauder).
    UpToComputed Quantity.Quantity
  deriving (Eq, Ord, Show)

-- | CR 601.2b then CR 601.2c: the range once the value of X is known.
-- AnnouncedX's collapses to a point, leaving nothing for CR 601.2c to ask;
-- UpToAnnouncedX's runs from zero to X.
--
-- Zero is the value to pass where no X has been announced: an ability
-- with no {X} in its cost announces none (CR 601.2b), and a castability gate
-- asked before the announcement exists reads the card in another zone, whose
-- {X} CR 107.3g already treats as zero.
--
-- `evaluate` answers the computed arm, and is the caller's because a Quantity is
-- read against a board and an object, neither of which a type module has. It
-- answers zero for a number the board cannot supply, which is the same
-- no-targets range an "up to" count of zero already means.
at :: (Quantity.Quantity -> Natural.Natural) -> Natural.Natural -> SlotCount -> TargetCount.TargetCount
at evaluate x c = case c of
  Printed count -> count
  AnnouncedX -> TargetCount.MkTargetCount {TargetCount.least = x, TargetCount.most = Just x}
  UpToAnnouncedX -> TargetCount.upTo x
  UpToComputed q -> TargetCount.upTo (evaluate q)

-- | May this slot be answered with more than one target? True for either count
-- the announced X sets, which no card bounds at one -- so a slot taking X
-- targets must be read where a set of recipients fits, exactly as a printed
-- plural count must. True for a computed count for the same reason: nothing
-- bounds the number the board hands back.
plural :: SlotCount -> Bool
plural c = case c of
  Printed count -> TargetCount.plural count
  AnnouncedX -> True
  UpToAnnouncedX -> True
  UpToComputed _ -> True

-- | The Quantity this count reads, where it reads one. The channel every walk
-- over a slot's numbers needs -- baking, renaming, CR 612.1's text change, and
-- the lints that ask which slots a card's numbers name -- beside the one
-- TargetSlot.amount already offers.
quantity :: SlotCount -> Maybe Quantity.Quantity
quantity c = case c of
  Printed _ -> Nothing
  AnnouncedX -> Nothing
  UpToAnnouncedX -> Nothing
  UpToComputed q -> Just q

-- | `quantity` above's write side: rebuild the count with its Quantity mapped,
-- leaving a count that reads none alone.
mapQuantity :: (Quantity.Quantity -> Quantity.Quantity) -> SlotCount -> SlotCount
mapQuantity f c = case c of
  Printed count -> Printed count
  AnnouncedX -> AnnouncedX
  UpToAnnouncedX -> UpToAnnouncedX
  UpToComputed q -> UpToComputed (f q)
