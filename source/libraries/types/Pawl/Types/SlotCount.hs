module Pawl.Types.SlotCount where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.TargetCount as TargetCount

-- | CR 601.2c: how many targets one instance of the word "target" takes, as the
-- CARD states it -- either a range printed in the text, or the value of X the
-- caster announced one step earlier (CR 601.2b).
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
  deriving (Eq, Ord, Show)

-- | CR 601.2b then CR 601.2c: the range once the value of X is known. Announcing
-- X fixes the number of targets, so the range collapses to a point and there is
-- nothing left for CR 601.2c to ask.
--
-- Zero is the value to pass where no X has been announced: an ability
-- with no {X} in its cost announces none (CR 601.2b), and a castability gate
-- asked before the announcement exists reads the card in another zone, whose
-- {X} CR 107.3g already treats as zero.
at :: Natural.Natural -> SlotCount -> TargetCount.TargetCount
at x c = case c of
  Printed count -> count
  AnnouncedX -> TargetCount.MkTargetCount {TargetCount.least = x, TargetCount.most = Just x}

-- | May this slot be answered with more than one target? True for the announced
-- X, which no card bounds at one -- so a slot taking X targets must be read
-- where a set of recipients fits, exactly as a printed plural count must.
plural :: SlotCount -> Bool
plural c = case c of
  Printed count -> TargetCount.plural count
  AnnouncedX -> True
