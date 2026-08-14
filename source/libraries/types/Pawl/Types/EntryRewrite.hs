module Pawl.Types.EntryRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.WithCounters as WithCounters

-- | CR 614.1c-d: how an entry replacement modifies the entry. AsCopy is Clone
-- (CR 707.5, and a real "may" -- declining is legal) and, with CR 707.9's
-- exceptions attached, Quicksilver Gargantuan; ChoiceOf is Primal Plasma
-- (CR 208.2b); ChooseColor is Painter's Servant (CR 614.1c);
-- ChooseBasicLandType is Convincing Mirage (CR 614.1c); ChooseCardNames is Null
-- Chamber (CR 614.1c with CR 201.4); WithCounters is CR 306.5b's intrinsic
-- loyalty; UnderSourceControl is Gather Specimens (CR 616.1b); Tapped is Zof
-- Bloodbog and Headless Skaab (CR 614.1d); PayLifeOrTapped is Razorgrass Field
-- (CR 614.1c).
--
-- AsCopy and ChoiceOf write into the object's COPIABLE snapshot, which is what
-- makes CR 707.2 fall out with no further machinery -- and CR 707.9b puts
-- AsCopy's exceptions in the same place, since the excepted value "becomes part
-- of the copiable values of the copy". CR 707.5's second half is
-- load-bearing here too: a copied "as [this] enters" ability takes effect, so a
-- Clone of a Primal Plasma runs the COPIED choice rather than skipping it.
--
-- SacrificeAnyNumber is the one constructor whose choice COSTS something, and
-- CR 614.12b's combined budget across permanents entering simultaneously holds
-- for it without a budget being carried anywhere: the choice is paid for inside
-- the entry loop that made it, so the next member of the batch cannot choose
-- what an earlier one already spent (CR 614.13b). Pawl.Engine.Event's arm
-- states the argument in full and names the board that proves it.
data EntryRewrite
  = -- | CR 707.5 / 614.1c: "you may have this permanent enter as a copy of ...".
    -- The list is CR 707.9's exceptions to the copying process -- the "except
    -- ..." clause -- and is empty for a plain Clone.
    --
    -- The exceptions ride the rewrite rather than being a rewrite of their own,
    -- because CR 707.9 makes them modifications OF the copying process: they
    -- happen only when a copy is actually made, so declining the "may" leaves
    -- the object its printed self and no exception applies.
    AsCopy [CopyException.CopyException]
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
    WithCounters WithCounters.WithCounters
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
    SacrificeAnyNumber SacrificeAnyNumber.SacrificeAnyNumber
  | -- | CR 702.136a via CR 614.1c: riot. "You may have this permanent enter with
    -- an additional +1/+1 counter on it. If you don't, it gains haste."
    --
    -- NOT written by a card. Like CR 306.5b's loyalty, this arm is minted from
    -- the finished projection -- Pawl.Engine.Keyword.mintedReplacementsOf, called
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
  | -- | CR 702.98a via CR 614.1c: unleash's FIRST static ability. "You may have
    -- this permanent enter with an additional +1/+1 counter on it."
    --
    -- Riot's arm with the declining half deleted -- rule 702.98a states no
    -- consequence for declining, where rule 702.136a grants haste -- so this is
    -- not Riot with a flag and not WithCounters with a "may": the first would
    -- make one arm answer two rules, and the second would put an optionality
    -- field on an arm CR 306.5b's loyalty must never make optional.
    --
    -- NOT written by a card, and NULLARY, for Riot's two reasons: it is minted
    -- from the finished projection by Pawl.Engine.Keyword.mintedReplacementsOf,
    -- and rule 702.98a fixes the kind and the count.
    --
    -- The counter goes through Pawl.Engine.Event.putCounters, CR 122.6's funnel,
    -- as riot's does, so CR 614.16 applies to it.
    Unleash
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
    Tapped
  | -- | CR 614.1c: "As [this permanent] enters, you may pay N life. If you don't,
    -- it enters tapped" (Razorgrass Field, the land face of Razorgrass Ambush //
    -- Razorgrass Field). Tapped's rewrite with a PRICE on avoiding it, and the
    -- one entry rewrite whose choice is paid for in life.
    --
    -- CR 614.1c and not CR 614.1d, unlike Tapped beside it: the printed sentence
    -- opens "As this land enters", which is rule 614.1c's second quoted shape,
    -- rather than the bare "[This permanent] enters . . ." rule 614.1d names.
    -- Both are replacement effects and both run through the CR 616.1 loop, so
    -- the split changes nothing about how this applies -- it is recorded because
    -- the two arms cite different subrules and a reader will ask why.
    --
    -- THE AMOUNT RIDES THE CONSTRUCTOR, and the printed cards settle it: the
    -- modal-double-faced lands print 3 while the Ravnica shocklands (Steam Vents,
    -- Godless Shrine) print the same sentence with 2. Nothing in rule 614.1c
    -- fixes the number, so this is the WithCounters position -- a payload the
    -- card writes -- and not the Riot or Tapped one, where a rule fixes both
    -- halves.
    --
    -- CARD-TYPE-AGNOSTIC for Tapped's reason: nothing here or in
    -- Pawl.Engine.Event's arm gates on Land, even though every printing of the
    -- sentence so far is one.
    --
    -- The declining half is Tapped's write, verbatim -- the status is stamped on
    -- the already-materialized incarnation rather than routed through the tap
    -- funnel -- so declining here and Zof Bloodbog's unconditional sentence leave
    -- the same board. The paying half goes through CR 119.4's life-payment door
    -- (Pawl.Engine.Event.payLife), so it records a life loss and a card watching
    -- for one sees it.
    --
    -- A Natural and not a Quantity, the position Pawl.Types.CostComponent.PayLife
    -- takes: the printed number is a literal on every card that prints this
    -- sentence, and CR 614.12a settles the choice before the permanent enters, so
    -- there is no board for a variable amount to be measured against yet.
    PayLifeOrTapped Natural.Natural
  | -- | CR 712.13a via CR 614.1c: the ability that makes a double-faced spell
    -- with its FRONT face up on the stack enter the battlefield transformed. CR
    -- 616.1d ranks it a bucket of its own
    -- (Pawl.Types.ReplacementBucket.BackFaceOnEntry), which is what distinguishes
    -- it from every other arm here.
    --
    -- A REPLACEMENT and not Pawl.Types.EntryRiders' `transformed`, which is CR
    -- 712.14a: that rule is an instruction an effect carries into a move it is
    -- PERFORMING ("put it onto the battlefield transformed"), while this one
    -- WATCHES an entry nobody instructed -- a permanent spell resolving under CR
    -- 608.3, where the only thing to rewrite is the entry itself. Neither can
    -- express the other, and CR 616.1d exists because only this one competes for
    -- an order.
    --
    -- NOT WRITTEN BY A CARD, and NULLARY, the position Riot and Unleash take: it
    -- is minted from the finished projection by
    -- Pawl.Engine.Keyword.mintedReplacementsOf, so a card says only
    -- `Keyword.Daybound` and rule 702.145b says what it means. It still
    -- round-trips through the codec, because every arm of this type does.
    --
    -- WHICH FACE is not carried, for CR 712.14a's reason one rule over: the
    -- ability names none, and the answer falls out of the card's layout
    -- (Pawl.Engine.Card.backFace). So this stays clear of CR 712.11b's choice of
    -- face when casting a modal double-faced card, which is a list of castable
    -- faces offered to the player rather than an instruction.
    --
    -- The CONDITION is not carried either, and rule 702.145b is why: "IF IT IS
    -- NIGHT and this permanent is represented by a double-faced card, it enters
    -- transformed." Both halves are the rule's, so both are asked by
    -- Pawl.Engine.Replacement.applies -- the row is collected on every entry and
    -- admits only the ones the rule admits, which is what keeps a daybound
    -- permanent entering by day out of CR 616.1d's bucket entirely.
    --
    -- Applied by Pawl.Engine.Event's arm as a write to Object.face on the
    -- already-materialized incarnation, Tapped's footing exactly: runEntry runs
    -- before the Moved event is recorded, so no trigger scan and no state-based
    -- action can see the interim front face.
    --
    -- Not implemented: CR 712.13a's second sentence, where a back face that is an
    -- instant or sorcery face sends the spell to its owner's graveyard instead of
    -- the battlefield (#1547).
    EntersTransformed
  deriving (Eq, Ord, Show)
