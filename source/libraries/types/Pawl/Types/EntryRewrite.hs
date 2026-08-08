module Pawl.Types.EntryRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1c-d: how an entry replacement modifies the entry. AsCopy is Clone
-- (CR 707.5, and a real "may" -- declining is legal); ChoiceOf is Primal Plasma
-- (CR 208.2b); ChooseColor is Painter's Servant (CR 614.1c);
-- ChooseBasicLandType is Convincing Mirage (CR 614.1c); ChooseCardNames is Null
-- Chamber (CR 614.1c with CR 201.4); WithCounters is CR 306.5b's intrinsic
-- loyalty; UnderSourceControl is Gather Specimens (CR 616.1b); Tapped is Zof
-- Bloodbog and Headless Skaab (CR 614.1d).
--
-- AsCopy and ChoiceOf write into the object's COPIABLE snapshot, which is what
-- makes CR 707.2 fall out with no further machinery. CR 707.5's second half is
-- load-bearing here too: a copied "as [this] enters" ability takes effect, so a
-- Clone of a Primal Plasma runs the COPIED choice rather than skipping it.
--
-- SacrificeAnyNumber is the one constructor whose choice COSTS something. CR
-- 614.12b's combined-affordability check across permanents entering
-- simultaneously is still not implemented -- the entry loop has no budget to
-- measure a batch against (#72).
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
  | -- | CR 614.1c with CR 201.4: as this object enters, its controller AND one
    -- opponent each choose a card name ("As this enchantment enters, you and an
    -- opponent each choose a card name other than a basic land card name" --
    -- Null Chamber). Both names are written to Object.chosenNames, for
    -- ChooseColor's reason.
    --
    -- TWO choosers in ONE arm, which is why this is not ChooseColor with a
    -- PlayerScope bolted on. "You and an opponent" is not a PlayerScope: it
    -- coincides with EachPlayer at two seats (CR 102.2) and diverges at three,
    -- where the card names one opponent and the rest of the table choose
    -- nothing.
    --
    -- The Filter is CR 201.4a's restriction on WHICH names may be chosen -- the
    -- characteristics of the card whose name is named -- read off the card,
    -- because "other than a basic land card name" is printed card text. Carried
    -- and passed to the prompt so the answerer can obey it; the engine does not
    -- check the answer against it (#663).
    ChooseCardNames (Filter.Filter Keyword.Keyword)
  | -- | CR 614.1c's other shape: "[This permanent] enters with ...". CR 306.5b's
    -- intrinsic loyalty ability is the one producer today.
    --
    -- The counters are placed through Pawl.Engine.Event.putCounters, the CR
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
  | -- | CR 614.1c's two sentences of Shimatsu the Bloodcloaked read as one
    -- rewrite: "As this creature enters, sacrifice any number of permanents.
    -- This creature enters with that many +1/+1 counters on it." The Filter is
    -- which permanents may be chosen; the CounterKind is what the count buys.
    --
    -- ONE constructor rather than a sacrifice arm beside WithCounters, because
    -- the number is not known until the choice is made and WithCounters carries a
    -- Natural settled at projection time. Splitting them would need a channel
    -- from one entry replacement to another that nothing else in CR 614.1c wants.
    --
    -- The CounterKind is a Maybe because the count need not buy counters at all.
    -- Wood Elemental sacrifices any number of untapped Forests and reads the
    -- count back through a characteristic-defining ability (CR 208.2a) instead,
    -- so it carries Nothing. The count reaches that ability the way CR 615.13's
    -- prevented amount reaches Selfless Squire's payload: the arm stamps it on
    -- the entering object under Pawl.Engine.Binding.sacrificedCount, a reserved
    -- slot the card's Quantity.InSlot reads. Stamped for BOTH shapes, so the
    -- Maybe says what the count buys and never whether it was recorded.
    --
    -- ANY NUMBER, and CR 614.13a's "choose a number of objects that will also
    -- change zones" is the rule -- so the prompt is
    -- Prompt.ChooseAnyNumberToSacrifice, which admits every subset, and the empty
    -- answer is legal. Shimatsu is printed 0/0, so declining is a real option
    -- with a real consequence (CR 704.5f buries it).
    --
    -- The permanents leave through the CR 701.21a sacrifice funnel and the
    -- counters arrive through the CR 122.6 one, so Rest in Peace and Doubling
    -- Season both see this the way they see any other sacrifice or counter.
    --
    -- CR 702.82a's devour is the same shape with a multiplier -- "N +1/+1
    -- counters for EACH creature sacrificed this way" -- so it wants this
    -- constructor plus a per-permanent count. Not carried: one is what Shimatsu
    -- needs, and no devour card is in the pool.
    SacrificeAnyNumber (Filter.Filter Keyword.Keyword) (Maybe CounterKind.CounterKind)
  | -- | CR 702.136a via CR 614.1c: riot. "You may have this permanent enter with
    -- an additional +1/+1 counter on it. If you don't, it gains haste."
    --
    -- NOT written by a card. Like CR 306.5b's loyalty, this arm is minted from
    -- the finished projection -- Pawl.Engine.Keyword.entryReplacementsOf, called
    -- by Pawl.Engine.Projection.intrinsicReplacementsOf -- so a card says only
    -- `Keyword.Riot` and rule 702.136a says what it means. It still round-trips
    -- through the codec, because every arm of this type does.
    --
    -- NULLARY, where WithCounters carries a kind and a count: rule 702.136a
    -- fixes both halves completely, so there is nothing for a card to vary.
    --
    -- The two halves land in two different places, which is why this is one arm
    -- and not two. The counter goes through Pawl.Engine.Event.putCounters, CR
    -- 122.6's funnel, so CR 614.16 applies to it (Doubling Season doubles riot's
    -- counter). The haste is a stored CR 611.2 continuous effect with CR 611.2a's
    -- "rest of the game" duration, which is what "it gains haste" with no stated
    -- end means -- neither value is copiable (CR 707.2), so neither may be
    -- written into the snapshot AsCopy and ChoiceOf use.
    Riot
  | -- | CR 614.1d: "This permanent enters tapped" (Zof Bloodbog's land half,
    -- Headless Skaab's creature). The one arm a permanent's OWN printed text
    -- writes about the STATUS it enters with, where every other writer of an
    -- entering incarnation's tap state is an EFFECT's rider
    -- (Pawl.Types.EntryRiders' `tapped`, "put it onto the battlefield tapped").
    -- CR 110.5b divides the two: a permanent enters untapped "unless a spell or
    -- ability says otherwise", and this is the ability saying otherwise rather
    -- than the spell putting it there. A land played as CR 305.1's special
    -- action goes through no effect at all, so a rider could not reach it.
    --
    -- CARD-TYPE-AGNOSTIC, and deliberately: nothing here or in
    -- Pawl.Engine.Event's arm gates on Land. CR 614.1d says "[This permanent]
    -- enters", and a creature spell printing the same sentence gets the same
    -- rewrite.
    --
    -- NULLARY. CR 614.1d fixes both halves -- which status, and that the permanent
    -- gets it -- so there is nothing for a card to vary, the position Riot and
    -- ChooseColor take.
    --
    -- Applied as "enters tapped" and NOT as "enters, then is tapped": the arm
    -- stamps Object.tapped rather than routing through the tap funnel, so no
    -- becomes-tapped event exists for anything to watch (CR 110.5b's distinction).
    -- The write lands on the already-materialized incarnation, which is
    -- observationally the same as minting it tapped -- Pawl.Engine.Event.runEntry
    -- runs before the Moved event is recorded, so no trigger scan and no
    -- state-based action can see the interim untapped object, the same footing
    -- UnderSourceControl's write to Object.enteredUnder stands on.
    --
    -- Not implemented: CR 614.1c's optional-cost variant of the same rewrite --
    -- "As this land enters, you may pay 3 life. If you don't, it enters tapped"
    -- (#950).
    Tapped
  deriving (Eq, Ord, Show)
