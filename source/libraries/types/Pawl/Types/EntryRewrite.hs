module Pawl.Types.EntryRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption

-- | CR 614.1c-d: how an entry replacement modifies the entry. AsCopy is Clone
-- (CR 707.5, and a real "may" -- declining is legal); ChoiceOf is Primal Plasma
-- (CR 208.2b); ChooseColor is Painter's Servant (CR 614.1c);
-- ChooseBasicLandType is Convincing Mirage (CR 614.1c); WithCounters is CR
-- 306.5b's intrinsic loyalty; UnderSourceControl is Gather Specimens (CR
-- 616.1b).
--
-- AsCopy and ChoiceOf write into the object's COPIABLE snapshot, which is what
-- makes CR 707.2 fall out with no further machinery. CR 707.5's second half is
-- load-bearing here too: a copied "as [this] enters" ability takes effect, so a
-- Clone of a Primal Plasma runs the COPIED choice rather than skipping it.
--
-- No constructor carries a cost: CR 614.12b has no producer here, because no
-- entry replacement in this pool has a cost attached to its choice (#72).
data EntryRewrite
  = AsCopy
  | ChoiceOf [EntryOption.EntryOption]
  | -- | CR 614.1c's other choosing shape: choose a colour as this enters
    -- (Painter's Servant). Nullary -- CR 105.1's five colours are the offer, and
    -- no card narrows them.
    --
    -- Written to Object.chosenColor rather than into the copiable snapshot
    -- AsCopy and ChoiceOf write to: CR 707.5 makes a copy run the copied
    -- as-enters ability and make its own choice, so the colour is not a copiable
    -- value.
    ChooseColor
  | -- | CR 614.1c again, with a subtype instead of a colour: choose a basic land
    -- type as this enters (Convincing Mirage). Nullary -- CR 305.6's five basic
    -- land types are the offer, and no card narrows them.
    --
    -- Written to Object.chosenSubtype rather than into the copiable snapshot,
    -- for ChooseColor's reason.
    ChooseBasicLandType
  | -- | CR 614.1c's other shape: "[This permanent] enters with ...". CR 306.5b's
    -- intrinsic loyalty ability is the one producer today.
    --
    -- The counters are placed through Pawl.Engine.Replacement.putCounters, the CR
    -- 122.6 funnel, and NOT written into the copiable snapshot AsCopy and
    -- ChoiceOf write to: counters are not characteristics (CR 122.1) and CR 707.2
    -- excludes them from the copiable values outright. Going through the funnel
    -- is what makes CR 614.16 hold, which is why Doubling Season doubles a
    -- planeswalker's starting loyalty.
    --
    -- Carries the count rather than reading it back off the source, because the
    -- intrinsic ability is minted per object from the PROJECTION
    -- (Pawl.Engine.Projection.intrinsicReplacementsOf) and the number is settled
    -- there, where CR 707.2's copiable loyalty is visible.
    WithCounters CounterKind.CounterKind Natural.Natural
  | -- | CR 616.1b's shape: a replacement modifying UNDER WHOSE CONTROL an object
    -- enters the battlefield. Gather Specimens is the one producer, and the whole
    -- of its text is this rewrite.
    --
    -- NULLARY, carrying no PlayerId, because CR 109.5 derives one: "you" is the
    -- controller of the effect's source, which for a floating row is baked at
    -- installation (Pawl.Types.ActiveReplacement's `controller`) and for a
    -- permanent's static ability is read live. A card cannot write a PlayerId
    -- anyway.
    --
    -- Written to the entering object's CR 110.2 default controller
    -- (Object.enteredUnder), not to a CR 613.1b layer-2 continuous effect: CR
    -- 800.4c distinguishes an effect that GIVES a player control from the player
    -- who controlled the object by default, and a permanent that ENTERED under
    -- your control is the second of those.
    UnderSourceControl
  deriving (Eq, Ord, Show)
