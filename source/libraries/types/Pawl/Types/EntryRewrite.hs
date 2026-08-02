module Pawl.Types.EntryRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption

-- | CR 614.1c: how an "as this permanent enters" replacement modifies the entry.
-- AsCopy is Clone (CR 707.5, and a real "may" -- declining is legal); ChoiceOf
-- is Primal Plasma (CR 208.2b); WithCounters is CR 306.5b's intrinsic loyalty.
-- The first two write into the object's COPIABLE snapshot,
-- which is what makes CR 707.2 fall out with no further machinery: the rule says
-- copiable values are the printed values as modified by copy effects and by
-- "as ... enters" abilities that set power and toughness.
--
-- CR 707.5's second half is load-bearing for this phase, not incidental: "If the
-- text that's being copied includes any abilities that replace the
-- enters-the-battlefield event (such as 'enters with' or 'as [this] enters'
-- abilities), those abilities will take effect." That is what makes a Clone of a
-- Primal Plasma run the COPIED "as it enters" choice rather than skip it -- the
-- CR 616.2 ordering behaviour later tasks in this phase implement.
--
-- Neither constructor carries a cost: CR 614.12b ("If multiple replacement
-- effects that require choices from a player would modify how multiple
-- permanents enter the battlefield simultaneously, that player may not make
-- choices for those effects that would cause the combined costs of those
-- effects to not be payable") has no producer here, because no entry
-- replacement in this pool has a cost attached to its choice (#72).
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption.EntryOption]
  | -- | CR 614.1c's other shape: "[This permanent] enters with ...". CR 306.5b is
    -- the one producer today -- "A planeswalker has the intrinsic ability 'This
    -- permanent enters with a number of loyalty counters on it equal to its
    -- printed loyalty number.' This ability creates a replacement effect (see
    -- rule 614.1c)."
    --
    -- The counters are placed through Pawl.Engine.Replacement.putCounters, the CR 122.6
    -- funnel, and NOT written into the copiable snapshot the two arms above
    -- write to: counters are not characteristics (CR 122.1, "counters are not
    -- objects and have no characteristics") and CR 707.2 excludes them from the
    -- copiable values outright. Going through the funnel is what makes CR
    -- 614.16's second sentence hold -- a counter-scaling replacement applies
    -- "even if the original event being modified wasn't itself an effect" --
    -- which is why Doubling Season doubles a planeswalker's starting loyalty.
    --
    -- Carries the count rather than reading it back off the source, because the
    -- intrinsic ability is minted per object from the PROJECTION
    -- (Pawl.Engine.Projection.intrinsicReplacementsOf) and the number is settled
    -- there, where CR 707.2's copiable loyalty is visible.
    WithCounters CounterKind.CounterKind Natural.Natural
  deriving (Eq, Ord, Show)
