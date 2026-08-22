module Pawl.Types.Response where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.HandActionIndex as HandActionIndex
import qualified Pawl.Types.HybridPayment as HybridPayment
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KickerDecision as KickerDecision
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaOption as ManaOption
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

-- | One answer to a prompt, serialized so a DecisionLog replays the game
-- deterministically.
--
-- Every prompt gets its OWN constructor, even where two share a payload shape:
-- decode's job is to return Nothing for a response that does not match the
-- prompt being asked, and two prompts sharing a constructor cannot do that.
data Response
  = ChoseAction Action.Action
  | -- | CR 104.3a: whether a player conceded when asked.
    Conceded Concession.Concession
  | Shuffled [ObjectId.ObjectId]
  | -- | CR 729.2: the player randomness picked to go first in a subgame.
    DeterminedFirstPlayer PlayerId.PlayerId
  | -- | The object randomness named for a
    -- Pawl.Types.ObjectRef.RandomCardInHand -- Merfolk Spy's "a card at random
    -- from their hand".
    --
    -- Its own constructor rather than ChoseCardInHand reused, though both name
    -- one card in one hand: this type's rule at the top, and here the difference
    -- is the whole point -- a transcript of a player DECIDING must not satisfy a
    -- prompt that asked randomness, which is CR 701.9b's distinction.
    SelectedAtRandom ObjectId.ObjectId
  | ChoseDiscard [ObjectId.ObjectId]
  | -- | CR 701.22a: the ordered partition a scrying player chose -- the cards
    -- going to the bottom, then the ones staying on top, each in the order the
    -- player put them there.
    ChoseScry ([ObjectId.ObjectId], [ObjectId.ObjectId])
  | -- | CR 701.25a: the ordered partition a surveilling player chose -- the cards
    -- going to their graveyard, then the ones staying on top of their library.
    ChoseSurveil ([ObjectId.ObjectId], [ObjectId.ObjectId])
  | -- | CR 701.29a: the ordered partition a fatesealing player chose over an
    -- opponent's library -- the cards going to the bottom, then the ones staying
    -- on top.
    ChoseFateseal ([ObjectId.ObjectId], [ObjectId.ObjectId])
  | -- | CR 701.44a: whether the exploring permanent's controller binned the
    -- revealed nonland card.
    ChoseExplore OptionalDecision.OptionalDecision
  | -- | CR 507.1: the opponent the active player chose to attack.
    ChoseDefender PlayerId.PlayerId
  | -- | CR 601.2g: the mana source the player chose to tap, or Nothing for CR
    -- 118.3c's refusal to activate any.
    ChoseManaSource (Maybe ObjectId.ObjectId)
  | -- | CR 605.3a, past the point the cost is covered: the further source
    -- the player chose to tap for the sake of floating what it makes, or Nothing
    -- to close the window.
    ChoseExtraManaSource (Maybe ObjectId.ObjectId)
  | -- | CR 605.3b / 105.4: the way the source's controller chose to tap it --
    -- the whole yield of one activation, so a Sol Ring's is two units, beside
    -- what CR 602.2b charged for it.
    ChoseManaYield ManaOption.ManaOption
  | -- | CR 601.2h: the one mana the payer chose to spend on the symbol being
    -- paid.
    ChoseManaToSpend ManaUnit.ManaUnit
  | -- | CR 701.34a: the permanents and players a proliferating player chose. A
    -- pair rather than two constructors, because one prompt asks one question.
    ChoseProliferation (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  | -- | CR 119.7-8's redistribution: each player the controller chose, beside the
    -- player whose previous life total they take.
    ChoseRedistribution (Map.Map PlayerId.PlayerId PlayerId.PlayerId)
  | -- | CR 701.54a: the creature a tempted player chose as their Ring-bearer.
    ChoseRingBearer ObjectId.ObjectId
  | -- | CR 701.39a: the creature a bolstering player chose to put the +1\/+1
    -- counters on.
    --
    -- Its own constructor rather than ChoseRingBearer reused, for
    -- ChoseCardInGraveyard's reason: every prompt gets its own response so a decode
    -- can reject one that does not match the prompt asked, and these two questions
    -- offer different candidates -- every creature its chooser controls against
    -- only those tied for the least toughness.
    ChoseBolster ObjectId.ObjectId
  | -- | CR 701.47a: the Army creature an amassing player chose to put the +1\/+1
    -- counters on.
    --
    -- Its own constructor for ChoseBolster's reason, and the two are not one
    -- constructor either: bolster offers the creatures tied for the least
    -- toughness, amass offers the Armies its chooser controls.
    ChoseAmass ObjectId.ObjectId
  | -- | CR 701.68a: the creature a blighting player chose to put the -1\/-1
    -- counters on.
    --
    -- Its own constructor for ChoseBolster's reason, and this is the one that
    -- needs the reason most: blight's candidates are ChoseRingBearer's exactly
    -- (every creature its chooser controls), so nothing but a distinct
    -- constructor keeps a transcript of one from replaying as the other.
    ChoseBlight ObjectId.ObjectId
  | -- | CR 107.14: how much {E} a player chose to pay to an
    -- Effect.PayAnyEnergy.
    --
    -- Its own constructor and not ChoseX below, though both carry one Natural:
    -- that one is CR 601.2b's announcement, made while CASTING, and this one is
    -- made mid-resolution. A transcript of one replaying as the other is exactly
    -- what ChoseBolster's reason forbids, and Harnessed Lightning off a
    -- Hatred-style announcement would be a silent wrong answer rather than a
    -- desync.
    ChosePaidEnergy Natural.Natural
  | -- | CR 609.7a: the damage source a player chose for a prevention effect that
    -- names one.
    --
    -- Its own constructor for ChoseBolster's reason: the candidates are neither
    -- of the counter-placing pools -- CR 609.7a's set spans the battlefield, the
    -- stack and the command zone, and is not scoped to the chooser at all.
    ChoseDamageSource ObjectId.ObjectId
  | -- | CR 608.2d: the graveyard card a player chose for an
    -- Pawl.Types.ObjectRef.ChosenCardInGraveyard. One of these per chooser, so a
    -- transcript of Exhume resolving holds one for each stocked graveyard.
    --
    -- Its own constructor rather than ChoseRingBearer reused, though both name
    -- one object: Pawl.Engine.Replay gives every prompt its own response so a
    -- decode can reject one that does not match the prompt asked, and the two
    -- questions differ in what the candidates ARE -- permanents on the
    -- battlefield against cards in a graveyard, the split
    -- Prompt.ChooseExilesFromGraveyard already draws against
    -- Prompt.ChooseSacrifices.
    ChoseCardInGraveyard ObjectId.ObjectId
  | -- | CR 608.2d: the card a player chose out of their own hand for an
    -- Pawl.Types.ObjectRef.ChosenCardInHand. One of these per chooser.
    --
    -- Its own constructor rather than ChoseCardInGraveyard reused, for that
    -- constructor's reason: the two prompts offer cards out of different zones,
    -- and a transcript of one must not satisfy the other.
    ChoseCardInHand ObjectId.ObjectId
  | -- | CR 608.2d: the card a player chose out of a bound group for an
    -- Pawl.Types.ObjectRef.ChosenCardFromAmong.
    --
    -- Its own constructor rather than ChoseCardInGraveyard or ChoseCardInHand
    -- reused, for those constructors' reason: the candidates come from a slot
    -- rather than from either zone, and a transcript of one must not satisfy
    -- another.
    ChoseCardFromAmong ObjectId.ObjectId
  | -- | CR 309.5a: the room a venturing player chose to move their marker into.
    ChoseRoom RoomIndex.RoomIndex
  | -- | CR 704.5j: the legendary permanent its controller kept.
    ChoseLegend ObjectId.ObjectId
  | DeclaredAttackers [ObjectId.ObjectId]
  | -- | CR 508.1b / CR 508.4: what one attacking creature was announced as
    -- attacking. Unlike ChoseDefender, this may name a permanent.
    ChoseAttackTarget AttackTarget.AttackTarget
  | -- | CR 508.1g / 701.43d: whether one chosen attacker with exert was exerted.
    -- Distinct from ChoseRiot and the other as-enters "may" answers for their
    -- reason: a transcript that answered one optional decision must not silently
    -- answer a different one, and this is the only one asked in a combat phase.
    ChoseExert OptionalDecision.OptionalDecision
  | DeclaredBlockers (Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId))
  | AssignedCombatDamage (Map.Map Recipient.Recipient Natural.Natural)
  | ChoseTargets (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient))
  | -- | CR 601.2c: how many targets a caster announced for each slot whose count
    -- the printed words leave variable (CR 115.6's "up to one", and every larger
    -- range). A slot answered with zero is one they declined.
    AnnouncedTargets (Map.Map SlotName.SlotName Natural.Natural)
  | -- | CR 612: the (from, to) basic land types a text-changer's caster chose.
    -- Named for the swap rather than for the pair, matching
    -- Prompt.ChooseLandTypeSwap.
    ChoseLandTypeSwap (Subtype.Subtype, Subtype.Subtype)
  | -- | CR 612 again, for the creature-type half: the (from, to) creature types
    -- Artificial Evolution's caster chose.
    ChoseCreatureTypeSwap (Subtype.Subtype, Subtype.Subtype)
  | -- | CR 614.1c: the basic land type a player chose as an object entered.
    -- Singular, and distinct from ChoseLandTypeSwap above for
    -- Prompt.ChooseBasicLandType's reason.
    ChoseBasicLandType Subtype.Subtype
  | -- | CR 701.23: the library cards a search found (empty = failed to find).
    Searched [ObjectId.ObjectId]
  | -- | CR 601.3 (Panglacial): the library card cast while searching, paired with
    -- the CR 709.3 half being cast (Nothing = declined). The name is part of the
    -- answer for ChoseAction's reason: a transcript that recorded only the card
    -- would replay a split card's other half.
    CastWhileSearched (Maybe (ObjectId.ObjectId, CardName.CardName))
  | -- | CR 601.2b: the value of X a caster chose.
    ChoseX Natural.Natural
  | -- | CR 601.2b: the mode(s) a caster chose for a modal spell. A Seq, since CR
    -- 700.2d's "You may choose the same mode more than once" makes one index
    -- appear several times.
    ChoseModes (Seq.Seq ModeIndex.ModeIndex)
  | -- | CR 707.5: the permanent a copy chose to copy (Nothing = declined).
    ChoseCopyTarget (Maybe ObjectId.ObjectId)
  | -- | CR 208.2b: the index of the entry shape a player chose as an object
    -- entered.
    ChoseEntryOption Natural.Natural
  | -- | CR 702.136a: whether a riot permanent's controller took the additional
    -- +1/+1 counter (Exercises) or the haste (Declines). Distinct from
    -- ChoseOptional, which records CR 603.5's "may" during a RESOLUTION: this one
    -- is answered inside a zone change, as the permanent enters.
    ChoseRiot OptionalDecision.OptionalDecision
  | -- | CR 702.98a: whether an unleash permanent's controller took the additional
    -- +1/+1 counter (Exercises) or entered without one (Declines). Distinct from
    -- ChoseRiot and ChosePayLifeOnEntry for their reason: a transcript that
    -- answered one as-enters "may" must not silently answer a different one.
    ChoseUnleash OptionalDecision.OptionalDecision
  | -- | CR 614.1c / 119.4: whether a permanent's controller paid the life its
    -- "as this enters, you may pay N life" ability asked for (Exercises) or let
    -- it enter tapped (Declines). Distinct from ChoseRiot for the reason
    -- ChoseRiot is distinct from ChoseOptional: a transcript that answered one
    -- as-enters "may" must not silently answer a different one.
    ChosePayLifeOnEntry OptionalDecision.OptionalDecision
  | -- | CR 614.1c / 701.20a: the card a permanent's controller revealed from
    -- their hand to keep it from entering tapped (Nothing = declined, and it
    -- entered tapped). Distinct from ChoseCopyTarget, which is the other
    -- Maybe-ObjectId as-enters answer, and from ChosePayLifeOnEntry for its
    -- reason: a transcript that answered one as-enters "may" must not silently
    -- answer a different one.
    ChoseRevealOnEntry (Maybe ObjectId.ObjectId)
  | -- | CR 303.4k: whether an Aura being turned face up exercised its printed
    -- "you may attach it" (Exercises) or left itself unattached (Declines).
    -- Distinct from ChoseRiot and ChosePayLifeOnEntry for their own reason, one
    -- event class over: turning face up is not an entry, so a transcript that
    -- answered an as-enters "may" must not answer this one.
    ChoseTurnUpAttachment OptionalDecision.OptionalDecision
  | -- | CR 614.1c: the colour a player chose as an object entered.
    ChoseColor Color.Color
  | -- | CR 105.4 / 106.3: the mana a player chose to add as a spell or ability
    -- that named no settled type resolved. Distinct from ChoseColor above for
    -- the reason Prompt.ChooseManaType gives, and it carries a ManaType rather
    -- than a Color because the answer is the type of the unit that reaches the
    -- pool.
    ChoseManaType ManaType.ManaType
  | -- | CR 201.4 / 614.1c: the card name a player chose as an object entered.
    ChoseCardName CardName.CardName
  | -- | The opponent a player chose for an as-enters choice the card assigns to
    -- "an opponent" (CR 614.12a puts that choice before the permanent enters).
    -- Unlike ChoseDefender, which also carries an opponent, this answers no
    -- combat question.
    ChoseOpponent PlayerId.PlayerId
  | -- | CR 310.9a / 704.5x / 704.5y: the player chosen to protect a battle,
    -- either as it entered or when a state-based action found the standing
    -- designation illegal. Distinct from ChoseOpponent above for the reason
    -- Prompt.ChooseProtector gives: a battle with no battle types takes its own
    -- controller, so this does not always carry an opponent.
    ChoseProtector PlayerId.PlayerId
  | -- | CR 614.1c / 614.12a: the player chosen as a permanent entered ("As this
    -- creature enters, choose a player" -- Stuffy Doll). Distinct from
    -- ChoseOpponent and ChoseProtector above for the reason Prompt.ChoosePlayer
    -- gives: "a player" offers every seat, the chooser included.
    ChosePlayer PlayerId.PlayerId
  | -- | CR 603.3b: the order a player chose for their simultaneous triggers, as
    -- a permutation of the offered indices.
    OrderedTriggers [Natural.Natural]
  | -- | CR 615.7: the order a shielded player (or a shielded permanent's
    -- controller) chose for the simultaneous damage events one prevention shield
    -- may cover, as a permutation of the offered indices.
    OrderedDamage [Natural.Natural]
  | -- | CR 601.2h: the order a player chose to pay a cost's parts in, as a
    -- permutation of the offered indices. A separate constructor from the two
    -- above, though the payload has the same shape: replaying a transcript
    -- against the wrong one would reorder a trigger batch instead of a payment.
    OrderedCostComponents [Natural.Natural]
  | -- | CR 608.2f: the relative order a resolving spell's controller chose for the
    -- members of one APNAP group a per-object body walks, as a permutation of the
    -- offered indices. A separate constructor from the three above for their
    -- reason: replaying a transcript against the wrong one would reorder a sweep
    -- instead of a trigger batch.
    OrderedForEach [Natural.Natural]
  | -- | CR 616.1: the index of the replacement effect a player chose to apply
    -- next.
    ChoseReplacement Natural.Natural
  | -- | CR 603.7c: the minted token a Create's slot bound, once a CR 614.16
    -- replacement had made several of them.
    ChoseBoundToken ObjectId.ObjectId
  | -- | CR 701.21a: the permanents a player chose to sacrifice to pay a cost.
    ChoseSacrifices (Set.Set ObjectId.ObjectId)
  | -- | CR 406.2: the cards a player chose to exile from their own graveyard to
    -- pay a cost. A separate constructor from ChoseSacrifices above, for
    -- ChoseTaps' reason below: replaying a transcript against the wrong one
    -- would sacrifice what it should have exiled.
    ChoseExilesFromGraveyard (Set.Set ObjectId.ObjectId)
  | -- | CR 702.122a: the permanents a player chose to TAP to pay a cost measured
    -- by their total power. A separate constructor from ChoseSacrifices above,
    -- though the payload has the same shape: replaying a transcript against the
    -- wrong one would tap what it should have sacrificed.
    ChoseTaps (Set.Set ObjectId.ObjectId)
  | -- | CR 701.3a: the object a player chose to attach a moving permanent to.
    ChoseAttachment ObjectId.ObjectId
  | -- | CR 601.2b: the cost a caster announced they would pay.
    ChoseCost (Cost.Cost Keyword.Keyword)
  | -- | CR 103.5: a player's mulligan declaration.
    DeclaredMulligan MulliganDecision.MulliganDecision
  | -- | CR 103.5: the cards a player put on the bottom of their library after a
    -- mulligan, in chosen order.
    PutOnBottom [ObjectId.ObjectId]
  | -- | CR 103.5b: the hand card whose mulligan-window action a player took, and
    -- WHICH of that card's actions it was (Nothing = declined).
    TookMulliganAction (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  | -- | CR 103.6: the hand card whose opening-hand action a player took, and which
    -- of that card's actions it was (Nothing = declined).
    TookOpeningHandAction (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  | -- | CR 603.5: whether the player a printed "may" asked exercised it --
    -- usually the resolving controller, and one answer per seat where the clause
    -- names several (Pawl.Types.Optionality.Optional).
    ChoseOptional OptionalDecision.OptionalDecision
  | -- | CR 608.2g: whether a player took a cast a resolving effect offered them.
    -- Distinct from ChoseOptional, which records CR 603.5's "may" over a whole
    -- CLAUSE, and from CastWhileSearched, which records the same rule's
    -- library-search producer and names which card was cast.
    ChoseOfferedCast OptionalDecision.OptionalDecision
  | -- | CR 709.3 / 712.11b / 715.3: which half of a multi-faced object a player
    -- chose to cast when a resolving effect offered it. Distinct from
    -- ChoseOfferedCast, which records whether that cast was taken at all: the two
    -- are asked one after the other about the same object, so both in one replay
    -- are not a duplicate. Distinct from CastWhileSearched, which names an object
    -- as well because CR 601.3's offer ranges over a whole library, and from
    -- ChoseCardName, which is a name named out of the whole card pool rather than
    -- one picked from the halves of an object already on offer.
    ChoseOfferedCastFace CardName.CardName
  | -- | CR 702.94a / CR 121.9: whether a player revealed the card they were
    -- drawing as they drew it. Distinct from ChoseOfferedCast, which records the
    -- LINKED ability's later "may": one reveal can be followed by a declined
    -- cast, so two answers in one replay are not a duplicate.
    ChoseMiracleReveal OptionalDecision.OptionalDecision
  | -- | CR 118.12a: whether the player a resolving spell or ability offered a
    -- cost to chose to pay it. Distinct from ChoseOptional, which records CR
    -- 603.5's "may" and is always answered by the resolving controller.
    ChoseToPay PaymentDecision.PaymentDecision
  | -- | CR 118.13a / 601.2b, and CR 118.13b: which way the payer announced they
    -- would pay a Phyrexian mana symbol, so a Mutagenic Growth paid out of life
    -- replays exactly as it was cast.
    AnnouncedPhyrexianPayment PhyrexianPayment.PhyrexianPayment
  | -- | CR 118.13a / 601.2b, and CR 118.13b: which way the payer announced they
    -- would pay a monocolored hybrid mana symbol, so a Flame Javelin cast for {6}
    -- replays as that and not as {R}{R}{R}.
    AnnouncedHybridPayment HybridPayment.HybridPayment
  | -- | CR 118.13a / 601.2b, and CR 118.13b: which half of a colour/colour hybrid
    -- mana symbol the payer announced they would pay, as the mana type it
    -- resolved to, so a Slippery Bogle paid with blue replays as that and not as
    -- green.
    AnnouncedHybridHalf ManaType.ManaType
  | -- | CR 118.7e: which half of a hybrid mana symbol in a cost REDUCTION its
    -- payer chose as CR 601.2f applied it. The symbol the half resolved to, so a
    -- {2/B} reduction taken as {2} replays as that and not as one black mana.
    ChoseReductionHalf ManaSymbol.ManaSymbol
  | -- | CR 601.2f: the order a payer applied several cost reductions in, written
    -- as the total that order reached -- which is the whole of what the choice
    -- does, so a Mishra's Foundry animated for {1} rather than for nothing
    -- replays as that.
    ChoseReducedCost ManaCost.ManaCost
  | -- | CR 702.42a / 601.2b: whether a caster used a modal spell's entwine
    -- ability.
    AnnouncedEntwine EntwineDecision.EntwineDecision
  | -- | CR 702.33a / 601.2b: whether a caster declared the intention to pay a
    -- spell's kicker cost, which is CR 702.33d's "that spell has been kicked".
    AnnouncedKicker KickerDecision.KickerDecision
  | -- | CR 903.9a's answer: whether the commander goes to the command zone.
    ReturnedCommander CommandZoneDecision.CommandZoneDecision
  | -- | CR 401.2's answer: the end of their library an owner picked for one card.
    ChoseLibraryEnd LibraryPosition.LibraryPosition
  | -- | CR 401.4's answer: the order an owner chose for the cards arriving at one
    -- end of their library, as a permutation of the offered indices.
    ArrangedLibraryArrivals [Natural.Natural]
  deriving (Eq, Ord, Show)
