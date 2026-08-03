module Pawl.Types.EntryRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption

-- | CR 614.1c-d: how an entry replacement modifies the entry. AsCopy is Clone
-- (CR 707.5, and a real "may" -- declining is legal); ChoiceOf is Primal Plasma
-- (CR 208.2b); ChooseColor is Painter's Servant (CR 614.1c); WithCounters is CR
-- 306.5b's intrinsic loyalty; UnderSourceControl is Gather Specimens (CR
-- 616.1b). AsCopy and ChoiceOf write into the object's COPIABLE snapshot, which
-- is what makes CR 707.2 fall out with no further machinery: the rule says
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
-- No constructor carries a cost: CR 614.12b ("If multiple replacement
-- effects that require choices from a player would modify how multiple
-- permanents enter the battlefield simultaneously, that player may not make
-- choices for those effects that would cause the combined costs of those
-- effects to not be payable") has no producer here, because no entry
-- replacement in this pool has a cost attached to its choice (#72).
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption.EntryOption]
  | -- | CR 614.1c's other choosing shape: "As [this permanent] enters, choose a
    -- color" (Painter's Servant). Nullary -- CR 105.1's five colours are the
    -- offer, and no card narrows them, so there is nothing to carry.
    --
    -- Written to Object.chosenColor rather than into the copiable snapshot
    -- AsCopy and ChoiceOf write to: CR 707.5's second sentence means a copy runs the
    -- copied as-enters ability and makes its own choice, so the colour is not a
    -- copiable value.
    ChooseColor
  | -- | CR 614.1c's other shape: "[This permanent] enters with ...". CR 306.5b is
    -- the one producer today -- "A planeswalker has the intrinsic ability 'This
    -- permanent enters with a number of loyalty counters on it equal to its
    -- printed loyalty number.' This ability creates a replacement effect (see
    -- rule 614.1c)."
    --
    -- The counters are placed through Pawl.Engine.Replacement.putCounters, the CR 122.6
    -- funnel, and NOT written into the copiable snapshot AsCopy and ChoiceOf
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
  | -- | CR 616.1b's shape: "if any of the replacement and/or prevention effects
    -- would modify UNDER WHOSE CONTROL an object would enter the battlefield".
    -- Gather Specimens' "it enters under your control instead" is the one
    -- producer, and the whole of its text is this rewrite.
    --
    -- NULLARY, carrying no PlayerId, because CR 109.5 derives one: "you" is the
    -- controller of the effect's source, which for a floating row is baked at
    -- installation (Pawl.Types.ActiveReplacement's `controller`) and for a
    -- permanent's static ability is read live. A card cannot write a PlayerId
    -- anyway -- the reason Effect.SkipNextPhase is its own opcode -- and here it
    -- does not have to.
    --
    -- Written to the entering object's CR 110.2 default controller
    -- (Object.enteredUnder), not to a CR 613.1b layer-2 continuous effect: CR
    -- 800.4c distinguishes "an effect that gives a player control of an object"
    -- from "the player who controlled that object by default", and a permanent
    -- that ENTERED under your control is the second of those.
    UnderSourceControl
  deriving (Eq, Ord, Show)
