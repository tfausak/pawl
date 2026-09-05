module Pawl.Types.EntryRewrite where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.EntryFlip as EntryFlip
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SacrificeAnyNumber as SacrificeAnyNumber
import qualified Pawl.Types.WithCounters as WithCounters

-- | CR 614.1c-d: how an entry replacement modifies the entry. Each arm names its
-- own producer below.
--
-- AsCopy, ChoiceOf and ChoiceByCoinFlip write into the object's COPIABLE
-- snapshot, which is what makes CR 707.2 fall out with no further machinery -- and CR 707.9b puts
-- AsCopy's exceptions in the same place, since the excepted value "becomes part
-- of the copiable values of the copy". CR 707.5's second half is
-- load-bearing here too: a copied "as [this] enters" ability takes effect, so a
-- Clone of a Primal Plasma runs the COPIED choice rather than skipping it.
--
-- Parametric in the EFFECT, for the reason Pawl.Types.DamageR gives: RunEffects
-- below carries a card's effects, and neither module may name the other.
--
-- SacrificeAnyNumber is the one constructor whose choice COSTS something, and
-- CR 614.12b's combined budget across permanents entering simultaneously holds
-- for it without a budget being carried anywhere: the choice is paid for inside
-- the entry loop that made it, so the next member of the batch cannot choose
-- what an earlier one already spent (CR 614.13b). Pawl.Engine.Event's arm
-- states the argument in full and names the board that proves it.
data EntryRewrite effect
  = -- | CR 707.5 / 614.1c / Clone, Vesuva: "you may have this permanent enter as a
    -- copy of ...", the payload carrying which permanents the printed noun phrase
    -- admits and CR 707.9's exceptions.
    AsCopy AsCopy.AsCopy
  | -- | CR 208.2b / 614.1c / Primal Plasma: the controller chooses one of these
    -- entry options as it enters.
    ChoiceOf [EntryOption.EntryOption]
  | -- | CR 614.1c decided by CR 705.2's winnerless flip / Molten Sentry: the arm
    -- above's options, picked by a coin rather than by the controller.
    ChoiceByCoinFlip EntryFlip.EntryFlip
  | -- | CR 614.1c / Painter's Servant: choose a colour as this enters, written to
    -- Object.chosenColor. Nullary -- CR 105.1's five colours are the offer.
    ChooseColor
  | -- | CR 614.1c / Convincing Mirage: choose a basic land type as this enters,
    -- written to Object.chosenSubtype. Nullary -- CR 305.6's five types are the
    -- offer.
    ChooseBasicLandType
  | -- | CR 614.1c / Stuffy Doll: choose a player as this enters, written to
    -- Object.chosenPlayer. Nullary -- CR 102.1's seats are the offer, and the
    -- prompt carries the ones still in the game.
    ChoosePlayer
  | -- | CR 614.1c with CR 201.4a / Null Chamber: this object's controller and one
    -- opponent each choose a card name matching the Filter, both written to
    -- Object.chosenNames.
    --
    -- The Filter is carried and passed to the prompt so the answerer can obey it;
    -- Pawl.Interpreter.policingCardNames is what judges the answer.
    ChooseCardNames (Filter.Filter Keyword.Keyword)
  | -- | CR 614.1c with CR 201.4a / Runed Halo: this object's CONTROLLER alone
    -- chooses one card name matching the Filter, written to Object.chosenNames.
    ChooseCardName (Filter.Filter Keyword.Keyword)
  | -- | CR 614.1c / Barkhide Troll: "[This permanent] enters with ..." counters,
    -- printed or minted (CR 306.5b's loyalty, CR 310.4b's defense, CR 714.3a's
    -- lore counter).
    WithCounters WithCounters.WithCounters
  | -- | CR 614.1c / Faerie Squadron: "[This permanent] enters ... with [keywords]",
    -- the keyword half of a clause whose counter half is WithCounters above.
    --
    -- Granted as a stored CR 611.2 continuous effect with CR 611.2a's
    -- rest-of-the-game duration, which is where riot's own grant lands and for
    -- its reason (Pawl.Engine.Event's Riot arm states it). NOT into the copiable
    -- snapshot ChoiceOf writes, which would be a different rule: CR 707.2 makes
    -- an "as . . . enters" ability copiable only where it SETS POWER AND
    -- TOUGHNESS, and this clause sets neither, so a copy of the entered permanent
    -- does not have the keyword. Pawl.ReplacementSpec's "CR 707.2 a token copy
    -- of the kicked Squadron has neither the flying nor the counters" is what
    -- proves the two apart.
    --
    -- Not implemented: the same clause granting a whole quoted ability rather
    -- than a keyword -- Degavolver's "Pay 3 life: Regenerate this creature"
    -- (#3006).
    --
    -- Not implemented: the two halves as ONE row. A card printing both writes two
    -- (Faerie Squadron, Voidpouncer), which CR 616.1 counts as two replacement
    -- effects where the printed sentence is one, so the entry prompts for an order
    -- the rules never ask for (#3288).
    WithKeywords (Set.Set Keyword.Keyword)
  | -- | CR 616.1b / Gather Specimens: the object enters under the control of the
    -- effect's source's controller, written to Object.enteredUnder.
    UnderSourceControl
  | -- | CR 614.1c / 614.13a / Shimatsu the Bloodcloaked: sacrifice any number of
    -- permanents matching the Filter as this enters. The count buys the counters the
    -- payload's CounterKind names, or is read back through a
    -- characteristic-defining ability where that is Nothing (Wood Elemental).
    --
    -- Not implemented: CR 702.82a's devour, which is this shape with a
    -- per-permanent multiplier (#877).
    SacrificeAnyNumber SacrificeAnyNumber.SacrificeAnyNumber
  | -- | CR 702.136a via CR 614.1c: riot, minted from the projection rather than
    -- written by a card.
    Riot
  | -- | CR 702.155b / 714.3b via CR 614.1c: read ahead's pair of intrinsic
    -- abilities, minted by Pawl.Engine.Saga.entryReplacementsOf because rule 714.3b
    -- REPLACES CR 714.3a's lore-counter ability rather than adding to it.
    ReadAhead
  | -- | CR 702.98a via CR 614.1c: unleash's first static ability, minted from the
    -- projection -- riot's arm with the declining half deleted.
    Unleash
  | -- | CR 702.54a via CR 614.1c: bloodthirst N, minted from the projection, with
    -- Nothing standing for CR 702.54b's "bloodthirst X".
    Bloodthirst (Maybe Natural.Natural)
  | -- | CR 702.150a via CR 614.1c: compleated, minted from the projection. The
    -- payload is the number of Phyrexian mana symbols life was paid for (CR
    -- 118.13a), rule 702.150a's "two" being the rule's own.
    Compleated Natural.Natural
  | -- | CR 614.1d / Zof Bloodbog, Headless Skaab: "This permanent enters tapped",
    -- stamped on the entering incarnation rather than routed through the tap
    -- funnel, so no becomes-tapped event exists (CR 110.5b).
    Tapped
  | -- | CR 614.1c / Razorgrass Field: "you may pay N life. If you don't, it enters
    -- tapped" -- the arm above's rewrite with a price on avoiding it.
    PayLifeOrTapped Natural.Natural
  | -- | CR 614.1c / Rustic Clachan: "you may reveal a [matching] card from your
    -- hand. If you don't, it enters tapped" -- PayLifeOrTapped one price over, and
    -- not a cost, CR 701.20a's reveal changing no zone.
    RevealOrTapped (Filter.Filter Keyword.Keyword)
  | -- | CR 702.145b via CR 614.1d: daybound's static ability making a permanent
    -- enter transformed, ranked its own CR 616.1d bucket. Collected on every entry
    -- and not only the stack's, which is what lets Pawl.MeldSpec's "CR 701.27g a
    -- melded permanent that entered with its back face up is still not one" reach
    -- it from exile.
    --
    -- Not implemented: CR 712.13a's second sentence, where a back face that is an
    -- instant or sorcery face sends the spell to its owner's graveyard instead of
    -- the battlefield (#1547).
    EntersTransformed
  | -- | CR 614.1c / Monstrous War-Leech: "As [this permanent] enters, [do
    -- something]" -- the one arm that RUNS effects, in printed order, deferred onto
    -- GameState.pendingEntryEffects for Pawl.Engine.Resolve.runEntryEffects to
    -- drain.
    RunEffects (Seq.Seq effect)
  deriving (Eq, Ord, Show)
