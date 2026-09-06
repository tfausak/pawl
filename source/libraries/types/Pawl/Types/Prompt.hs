{-# LANGUAGE GADTs #-}

module Pawl.Types.Prompt where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CommandZoneDecision as CommandZoneDecision
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GameEvent as GameEvent
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
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.OutsideCard as OutsideCard
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEntry as ReplacementEntry
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TargetCount as TargetCount
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.Zone as Zone

-- | A question the engine puts to a player. The resolution-time choices share
-- one posture: choose, not target (CR 115.1); filter the candidates rather
-- than trusting the answer; raise the prompt only where two or more
-- candidates make it a real choice, unless a "may" or an "any number" makes
-- one candidate a real fork.
data Prompt r where
  ChooseAction :: Decider.Decider -> PlayerId.PlayerId -> [Action.Action] -> Prompt Action.Action
  -- | CR 104.3a. No Decider: CR 723.6 needs the ask to reach the true player.
  Concede :: PlayerId.PlayerId -> Prompt Concession.Concession
  Shuffle :: [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 729.2: a subgame's starting player, drawn from its turn order, which
  -- is then rotated to begin with them (CR 103.1). No Decider: randomness is
  -- not a choice.
  RandomFirstPlayer :: NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | Which object randomness named (Pawl.Types.ObjectRef.RandomCardInHand).
  -- Neither Decider nor PlayerId: randomness is not a choice, and CR 701.24a
  -- makes who it is asked of unobservable; the caller filters the answer back.
  RandomObject :: NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | Which of the resolving controller's opponents randomness named
  -- (Pawl.Types.Effect.ChooseOpponentAtRandom); RandomObject's shape, asked
  -- only for two or more.
  RandomOpponent :: NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 706.1a: what an N-sided die came up -- CR 706.2's natural result, the
  -- instruction's modifier added afterwards. RandomObject's reasons for
  -- carrying neither Decider nor PlayerId.
  RollDie :: Natural.Natural -> Prompt Natural.Natural
  -- | CR 706.4: which result of one roll instruction the roller uses (Valiant
  -- Endeavor's "roll two d6 and choose one result"); the answer indexes the
  -- results, which are in roll order and may compare equal. A choice and not a
  -- roll, so unlike RollDie above it carries a Decider and the seat. Not
  -- raised where every result is the same number, which no card can tell from
  -- either answer.
  ChooseDieResult :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty Natural.Natural -> Prompt Natural.Natural
  -- | CR 705.1: which face a flipped coin came up, asked after CallCoin (CR
  -- 705.2). RandomObject's reasons for carrying neither Decider nor PlayerId.
  FlipCoin :: Prompt CoinFace.CoinFace
  -- | CR 705.2: the face the flipping player calls before the coin comes up;
  -- a face-only flip asks no call.
  CallCoin :: Decider.Decider -> PlayerId.PlayerId -> Prompt CoinFace.CoinFace
  -- | CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 701.22a: the top cards of the scrying player's library, top-first;
  -- the answer is (bottom, top), each reading from that end inward.
  ChooseScry :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.25a: ChooseScry's payload, the first list going to the graveyard
  -- in the order it is put there (CR 404.3).
  ChooseSurveil :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.29a: the first PlayerId is the fatesealer, asked and shown the
  -- cards; the second is the library's owner. ChooseScry's answer.
  ChooseFateseal :: Decider.Decider -> PlayerId.PlayerId -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.44a: whether the exploring permanent (first ObjectId) bins the
  -- revealed nonland card (second).
  ChooseExplore :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 507.1 / 703.4h: which opponent is the defending player; not asked
  -- with one candidate (CR 506.2).
  ChooseDefender :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 601.2g / 602.2b: which mana source to activate next while the pool
  -- does not yet cover the cost, once per source; nothing declines (CR
  -- 118.3c). Never elided; the list is collapsed to one source per
  -- interchangeability class (Pawl.Engine.Interchangeable).
  ChooseManaSource :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 605.3a: which further source to activate once the pool already
  -- covers the cost, or Nothing to pay; never elided, floating being
  -- observable, and collapsed as ChooseManaSource is.
  ChooseExtraManaSource :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 733.1: whether the mana abilities activated while making a reversed
  -- action are reversed too; the mana stays in the pool either way (CR
  -- 106.4). All or nothing rather than a subset (gap #3134).
  ReverseManaAbilities :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 605.3b: which mana the source produces, as the mana ability resolves;
  -- candidates are deduplicated by the whole option (Mana.manaOptionsOf).
  ChooseManaYield :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaOption.ManaOption -> Prompt ManaOption.ManaOption
  -- | CR 601.2h: which pool unit pays one symbol of a cost, once per mana
  -- spent; elided where every way of paying leaves the same pool
  -- (Pawl.Engine.Mana.leftovers).
  ChooseManaToSpend :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ManaUnit.ManaUnit -> Prompt ManaUnit.ManaUnit
  -- | CR 701.34a: which of the permanents and players holding a counter get
  -- one more of each kind; elided only when both lists are empty.
  ChooseProliferate :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [PlayerId.PlayerId] -> Prompt (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  -- | Reverse the Sands' redistribution of life totals (CR 119.7-8): each
  -- chosen player maps to the player whose previous total they take, a
  -- permutation Pawl.Engine.Resolve checks. Elided below two candidates.
  --
  -- Not implemented: CR 810.9f's "not more than one member of each team", which
  -- is a Two-Headed Giant rule (#2849).
  ChooseRedistribution :: Decider.Decider -> PlayerId.PlayerId -> [(PlayerId.PlayerId, Integer)] -> Prompt (Map.Map PlayerId.PlayerId PlayerId.PlayerId)
  -- | CR 701.54a: which creature becomes the tempted player's Ring-bearer;
  -- not raised for zero, where CR 701.54d still tempts.
  ChooseRingBearer :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.39a: which of the creatures tied for least toughness is
  -- bolstered; the ObjectId is the resolving object.
  ChooseBolster :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.47a: which Army gets the amass counters, read after the rule's
  -- token is created; not raised for zero (CR 701.47b).
  ChooseAmass :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.68a: which creature is blighted, the candidates unnarrowed; not
  -- raised for zero (CR 101.3, CR 701.68b).
  ChooseBlight :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 122.5: which kind of counter a move takes off the first object onto
  -- the second, where the card leaves the kind open.
  ChooseMovedCounter :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> NonEmpty.NonEmpty (CounterKind.CounterKind Keyword.Keyword) -> Prompt (CounterKind.CounterKind Keyword.Keyword)
  -- | CR 122.5: how many counters of each kind a move takes off the first
  -- object onto the second, where the card leaves the count open; the Map is
  -- what the first object has, the answer clamped to it. Raised for one
  -- candidate too, "any number" including none.
  ChooseMovedCounters :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> Prompt (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)
  -- | CR 122.5: ChooseMovedCounters where the card states a floor ("one or
  -- more"), so an empty answer is repaired to one counter of the first kind.
  ChooseMovedCountersAtLeastOne :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> Prompt (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)
  -- | CR 122.5: how many counters of each kind a move puts onto each of
  -- several destinations; the sum comes off the first object, the tallies
  -- clamped in offered order.
  ChooseDistributedMovedCounters :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural))
  -- | CR 122.5: ChooseMovedCounter with declining added ("up to one"); raised
  -- for one candidate too, and an unoffered kind reads as declining.
  ChooseMovedCounterOrNone :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> NonEmpty.NonEmpty (CounterKind.CounterKind Keyword.Keyword) -> Prompt (Maybe (CounterKind.CounterKind Keyword.Keyword))
  -- | CR 107.14: how much {E} the player pays to an Effect.PayAnyEnergy as
  -- the object resolves; the Natural is their energy, enforced (CR 118.3),
  -- and zero declines the "may". Not raised for a bound of 0, where that same
  -- rule leaves 0 as the only payable amount.
  ChoosePaidEnergy :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 702.155b / 714.3b: which chapter a Saga with read ahead enters on; the
  -- Natural is the final chapter (CR 714.2d), the answer clamped into the
  -- range. Not raised for a range of one, nor for a bound of 0.
  ChooseReadAheadChapter :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 609.7a: which source of damage a prevention or redirection effect
  -- that names one applies to; not raised for zero, where no shield is
  -- installed.
  ChooseDamageSource :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 603.7b: which of several simultaneous occurrences triggers a delayed
  -- triggered ability with no stated duration; the answer indexes the events,
  -- which are in log order and may compare equal.
  ChooseDelayedTriggerEvent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty GameEvent.GameEvent -> Prompt Natural.Natural
  -- | CR 608.2d: which graveyard card a Pawl.Types.ObjectRef.ChosenCardInGraveyard
  -- takes; the PlayerId is the chooser, who need not own the graveyard, asked
  -- in APNAP order (CR 101.4).
  ChooseCardInGraveyard :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 608.2d: which card of their own hand a Pawl.Types.ObjectRef.ChosenCardInHand
  -- takes; also CostComponent.PutCardFromHandOntoBattlefield's question while
  -- a CR 118.12 cost is paid, where the ObjectId is the object the cost is on.
  ChooseCardInHand :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 608.2d: which card of a bound group a Pawl.Types.ObjectRef.ChosenCardFromAmong
  -- takes, asked once per card with what earlier asks did not take; the
  -- PlayerId is the ref's chooser.
  ChooseCardFromAmong :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 309.2a \/ 701.49a: which of the dungeon printings they own a venturing
  -- player brings in from outside the game (CR 400.11), there being no object
  -- for it yet.
  ChooseDungeon :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PrintingId.PrintingId -> Prompt PrintingId.PrintingId
  -- | CR 103.2b \/ 702.139a: which card, if any, a player reveals from outside
  -- the game as their companion, before the game begins. A PrintingId for
  -- ChooseDungeon's reason -- outside the game is not a zone (CR 400.11), so
  -- there is no object.
  --
  -- A Maybe, unlike every other pre-game choice: rule 103.2b's "if any players
  -- WISH to reveal" makes declining an answer, so the prompt is raised even where
  -- one card is offered.
  ChooseCompanion :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PrintingId.PrintingId -> Prompt (Maybe PrintingId.PrintingId)
  -- | CR 400.11c \/ 729.4: which card a player brings in from outside the game;
  -- an OutsideCard rather than a printing because which zone it leaves decides
  -- what triggers (CR 729.4a). Two copies of one printing are one offer.
  ChooseFromOutsideTheGame :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty OutsideCard.OutsideCard -> Prompt OutsideCard.OutsideCard
  -- | CR 309.5a \/ 701.49b: which arrow out of the current room a venturing
  -- player follows; the ObjectId is the dungeon card.
  ChooseRoom :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty RoomIndex.RoomIndex -> Prompt RoomIndex.RoomIndex
  -- | CR 709.5f \/ 709.5g: which half of a permanent an effect locks or
  -- unlocks, named by the half's own name (CR 709.4a); not raised for an
  -- instruction naming every admitted half.
  ChooseHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty CardName.CardName -> Prompt CardName.CardName
  -- | CR 704.5j: which of two or more same-named legendary permanents its
  -- controller keeps, one prompt per name. Never elided.
  ChooseLegend :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 508.1a: which of the legal attackers attack; what each attacks is
  -- ChooseAttackTarget's step (CR 508.1b).
  DeclareAttackers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 508.1b: what one attacking creature attacks, the defending player
  -- first; also CR 508.4's entering-attacking question (CR 506.3b). Elided at
  -- one candidate.
  ChooseAttackTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty AttackTarget.AttackTarget -> Prompt AttackTarget.AttackTarget
  -- | CR 508.1g / 701.43d: whether one chosen attacker with exert is exerted;
  -- never elided, CR 701.43b making an exerted creature exertable again.
  ChooseExert :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 509.1: the legal blockers, then the attackers; the answer maps each
  -- blocking creature to the set it blocks (CR 509.1a's one can be raised), a
  -- non-blocker absent. Pawl.Engine.Combat.legalBlockDeclaration judges it.
  DeclareBlockers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> Prompt (Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId))
  -- | CR 510.1 / 702.19b: the assigning creature divides its power among the
  -- recipients; the Map is recipient to lethal threshold (0 on the blocking
  -- side, CR 510.1d). Damage.wellFormedAssignment and Damage.tiersCleared
  -- validate.
  AssignCombatDamage :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map Recipient.Recipient Natural.Natural -> Natural.Natural -> Prompt (Map.Map Recipient.Recipient Natural.Natural)
  -- | CR 601.2c: per named slot of the object being cast, how many targets it
  -- takes and the legal recipients; only the slots AnnounceTargets settled
  -- are offered, so a declined CR 115.6 slot is absent.
  ChooseTargets :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map SlotName.SlotName (Natural.Natural, Set.Set Recipient.Recipient) -> Prompt (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient))
  -- | CR 601.2c, before ChooseTargets: how many targets each variable slot
  -- takes, within a range the board can supply
  -- (Pawl.Types.TargetCount.ceilingOn).
  AnnounceTargets :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map SlotName.SlotName (TargetCount.TargetCount, Set.Set Recipient.Recipient) -> Prompt (Map.Map SlotName.SlotName Natural.Natural)
  -- | CR 612: the two basic land types a text-changing effect's slot swaps,
  -- asked as the effect is applied (CR 608.2d); the Set is the words the new
  -- type may not be.
  ChooseLandTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 612: ChooseLandTypeSwap's sibling for CR 612.2's creature types, the
  -- family named rather than enumerated.
  ChooseCreatureTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 614.1c: the one basic land type an entering object's controller
  -- chooses, written to Object.chosenSubtype; no candidate list (CR 305.6).
  ChooseBasicLandType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Subtype.Subtype
  -- | The printed "and/or" of a multi-zone search: which of the offered zones
  -- this searcher looks through, a nonempty subset (Boonweaver Giant's "may"
  -- is why nonempty). Asked ahead of CR 601.3's offer and the search.
  ChooseSearchZones :: Decider.Decider -> PlayerId.PlayerId -> Set.Set Zone.Zone -> Prompt (Set.Set Zone.Zone)
  -- | CR 701.23 / 701.23b: which of the matching cards across the zones
  -- searched the searcher finds, at most the Natural. Fewer is legal for a
  -- hidden zone (CR 400.2) stating a quality, an "up to", or no count;
  -- Pawl.Engine.Resolve completes a short answer from a public zone.
  Search :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 608.2g: the re-entrant cast during a library search (Panglacial Wurm),
  -- offered in a loop before the find; one entry per castable half with the
  -- name CR 709.3 needs.
  CastWhileSearching :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, CardName.CardName)] -> Prompt (Maybe (ObjectId.ObjectId, CardName.CardName))
  -- | CR 601.2b / 602.2b: the value of X, before targets. The Natural is the
  -- greatest value legally announceable now (Cast.affordableX,
  -- Activate.affordableX; Cost.maximumX for CR 101.1's card-stated ceiling).
  --
  -- Advisory: the answer is filtered against it nowhere. Announcing past what
  -- the player can pay is answered by CR 601.2h's reversal (#741); past what
  -- the card permits, by Cast's own gate (CR 101.1, CR 101.2).
  ChooseX :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 702.42a: whether the modal spell is entwined, before ChooseModes; the
  -- Cost is what entwining adds (CR 601.2f).
  ChooseEntwine :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Cost.Cost Keyword.Keyword -> Prompt EntwineDecision.EntwineDecision
  -- | CR 702.33a: how many times one kicker cost is paid, after ChooseModes and
  -- before ChooseCost (CR 601.2b); the Maybe Natural is the limit, Nothing for
  -- CR 702.33c's multikicker, and an answer past it is rejected.
  ChooseKicker :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Cost.Cost Keyword.Keyword -> Maybe Natural.Natural -> Prompt KickerDecision.KickerDecision
  -- | CR 903.9a: whether the owner returns their commander from a graveyard or
  -- exile; declining leaves it until it moves there afresh.
  ReturnCommander :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt CommandZoneDecision.CommandZoneDecision
  -- | CR 401.2: which end of a library a card arrives at where the effect
  -- leaves it open; the owner is asked (CR 400.3), in APNAP order. Never
  -- elided; a stated end (Pawl.Types.LibraryPlacement.Stated) is not asked.
  ChooseLibraryEnd :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt LibraryPosition.LibraryPosition
  -- | CR 401.4: the owner arranges two or more cards arriving at one end of a
  -- library at once; the answer permutes the indices, reading from that end
  -- inward.
  ArrangeLibraryArrivals :: Decider.Decider -> PlayerId.PlayerId -> LibraryPosition.LibraryPosition -> [ObjectId.ObjectId] -> Prompt [Natural.Natural]
  -- | CR 601.2b / 700.2a: the modes, before X and targets, from the legal ones;
  -- a Seq because CR 700.2d may let one mode be chosen twice. A forced
  -- selection is not asked.
  ChooseModes :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Set.Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> Prompt (Seq.Seq ModeIndex.ModeIndex)
  -- | CR 707.5 / 614.1c / 614.12a: which permanent an object entering as a copy
  -- copies, Nothing being the card's "may" declined; asked at one candidate,
  -- not at none.
  ChooseCopyTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 208.2b / 614.1c: which of the offered shapes an entering object becomes
  -- (Primal Plasma), an index into the list, written into the copiable
  -- snapshot (CR 707.2).
  ChooseEntryOption :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [EntryOption.EntryOption] -> Prompt Natural.Natural
  -- | CR 702.136a / 614.1c: whether riot takes the +1\/+1 counter (Exercises)
  -- or haste. Never elided.
  ChooseRiot :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 702.98a / 614.1c: whether unleash takes the +1\/+1 counter; its own
  -- constructor so a transcript cannot answer one as-enters "may" as another.
  ChooseUnleash :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 614.1c with CR 119.4: whether N life is paid as the permanent enters
  -- so it enters untapped; not asked below N life, a zero amount excepted (CR
  -- 119.4b).
  ChoosePayLifeOnEntry :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt OptionalDecision.OptionalDecision
  -- | CR 614.1c with CR 701.20a: which matching hand card is revealed as the
  -- permanent enters so it enters untapped, Nothing declining; raised at one
  -- candidate too.
  ChooseRevealOnEntry :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 614.1c: the colour an entering object's controller chooses; no
  -- candidate list (CR 105.1).
  ChooseColor :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Color.Color
  -- | CR 105.4 / 106.3: which mana a resolving object adds when the type it
  -- names is not settled, from Pawl.Engine.Mana.producedTypes.
  ChooseManaType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaType.ManaType -> Prompt ManaType.ManaType
  -- | CR 201.4: the card name a player chooses, as an object enters (CR 614.1c)
  -- or as a resolution instructs (CR 608.2c); the Filter is CR 201.4a's
  -- restriction, and the PlayerId is the chooser, asked in APNAP order.
  --
  -- No candidate list: rule 201.4's offer is every card in the Oracle card
  -- reference, which is not a set the engine holds. The answer is judged on the
  -- far side of Pawl.Engine.Engine.runGameAsked instead, by
  -- Pawl.Interpreter.policingCardNames, which resolves the name and matches the
  -- Filter against it.
  ChooseCardName :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Filter Keyword.Keyword -> Prompt CardName.CardName
  -- | Which opponent a card's text names, as a permanent enters (CR 614.12a),
  -- at resolution for CR 701.29a's fateseal, or for a resolving
  -- Pawl.Types.Effect.ChoosePlayer whose scope leaves the chooser out
  -- (Skullwinder); the chooser is CR 109.5's "you" at entry and the rule's
  -- actor at the fateseal.
  ChooseOpponent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 310.9a: which player protects the battle, chosen by its controller as
  -- it enters and again under CR 310.11's state-based action; the candidates
  -- are Pawl.Engine.Battle.protectorCandidates.
  ChooseProtector :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 614.1c with CR 614.12a: which player the entering permanent's
  -- controller chooses (Stuffy Doll), or, at resolution, which player a
  -- resolving effect's controller chooses (CR 608.2d; Stadium Vendors) -- the
  -- ObjectId is the entering permanent on the first road and the resolving
  -- source on the second. The candidates come from the caller, and this
  -- constructor rather than ChooseOpponent above is the one raised exactly when
  -- they include the chooser.
  ChoosePlayer :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | An order over one player's triggered abilities, a permutation of the
  -- entries' indices. At Pawl.Engine.Engine.orderPending it is CR 603.3b's
  -- stack order, the last named resolving first, elided unless two or more
  -- and observable (Pawl.Engine.Engine.orderInert, CR 117.3b); at
  -- Pawl.Engine.Event.Trigger.delayedPending it is CR 101.4c's asking order, the first
  -- named asked first, elided on the count alone.
  OrderTriggers :: Decider.Decider -> PlayerId.PlayerId -> [TriggerEntry.TriggerEntry] -> Prompt [Natural.Natural]
  -- | CR 615.7: the order a shield is offered two or more simultaneous damage
  -- events, a permutation of their indices; asked only where the order is
  -- observable.
  OrderDamage :: Decider.Decider -> PlayerId.PlayerId -> [DamageEvent.DamageEvent] -> Prompt [Natural.Natural]
  -- | CR 616.1: which of the applicable replacement effects applies next, an
  -- index into the candidates, re-asked per iteration (CR 616.1f); elided
  -- where the candidates are indistinguishable in effect and, where
  -- Replacement.readsApplier, in CR 109.5's "you".
  ChooseReplacement :: Decider.Decider -> PlayerId.PlayerId -> [ReplacementEntry.ReplacementEntry] -> Prompt Natural.Natural
  -- | CR 603.7c: which of several minted tokens a Create's slot binds for a
  -- delayed trigger's "it"; reachable only through a replacement scaling the
  -- count (CR 614.16), and a choice by CR 707.10e's analogue.
  ChooseBoundToken :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.21a: which of the payer's matching permanents are sacrificed to
  -- pay a cost, the Natural how many; asked only with more candidates than the
  -- count. Not ChooseTargets: CR 115.1 makes a target only what the word
  -- names.
  ChooseSacrifices :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 406.2: which cards are exiled from the payer's graveyard to pay a
  -- cost; ChooseSacrifices' payload, posture and elision.
  ChooseExilesFromGraveyard :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 614.1c with CR 614.13a: any number of the candidates sacrificed as the
  -- permanent enters, the empty set included; asked at one candidate, skipped
  -- at zero. Answers as Response.ChoseSacrifices.
  ChooseAnyNumberToSacrifice :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 608.2d: any number of the matching permanents a resolving effect acts
  -- on (Pawl.Types.ObjectRef's AnyNumberMatching); ChooseAnyNumberToSacrifice's
  -- shape. Not ChooseTargets (CR 115.1, CR 115.10a).
  ChooseAnyNumberOfPermanents :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 608.2d: which one of the matching permanents a resolving effect acts
  -- on (Pawl.Types.ObjectRef.ChosenPermanent); at none the instruction is
  -- impossible (CR 101.3). Not ChooseTargets (CR 115.1, CR 115.10a).
  ChoosePermanent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 702.122a: which untapped permanents are tapped to crew, the Natural a
  -- power threshold rather than a size (Pawl.Engine.Cost validates the sum).
  -- Never elided, forcedness being a question about subsets. Answers as
  -- Response.ChoseTaps.
  ChooseTapsForTotalPower :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | Which permanents are tapped to pay a cost naming how many (CR 601.2f);
  -- ChooseSacrifices' shape and elision. Answers as Response.ChoseTaps.
  ChooseTaps :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | Which permanents are returned to their owners\' hands to pay a cost naming
  -- how many (CR 118.1); ChooseTaps\' shape, answering as Response.ChoseReturns
  -- so a replay cannot tap what it should have returned.
  ChooseReturns :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 701.3a: where an effect moving an attached permanent puts it, the
  -- current host excluded (CR 701.3b); the offer is the card's text, so CR
  -- 303.4j is left to the player. Elided at one candidate. The PlayerId is the
  -- resolving controller for one named destination (CR 608.2d) and the
  -- subject's controller for several (CR 303.4d, CR 301.5c).
  ChooseAttachment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 303.4k with CR 614.1e: whether an Aura turned face up exercises its
  -- printed "you may attach it"; never elided, declining leaving it to CR
  -- 704.5m.
  ChooseTurnUpAttachment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 601.2b: which of the payable alternative or additional costs is paid,
  -- after modes and before X and targets (CR 118.9b makes an alternative cost
  -- optional).
  ChooseCost :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [Cost.Cost Keyword.Keyword] -> Prompt (Cost.Cost Keyword.Keyword)
  -- | CR 601.2h: the order the non-mana components of a total cost are paid in,
  -- a permutation of their printed indices, once per pass
  -- (Pawl.Engine.Cost.paidInSecondPass); asked only where observable
  -- (Pawl.Engine.Cost.orderObservable).
  OrderCostComponents :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [CostComponent.CostComponent Keyword.Keyword] -> Prompt [Natural.Natural]
  -- | CR 508.1j / 509.1f: the order a combat toll's charges are paid in, one
  -- entry per taxing permanent; asked only where observable
  -- (Pawl.Engine.Cost.tollOrderObservable).
  OrderCombatTolls :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [Natural.Natural]
  -- | CR 712.21a: the order the owner puts a melded permanent's component cards
  -- into their graveyard or library, first named put in first. CR 730.3a says
  -- the sentence again for a merged permanent (#874), so the name is the
  -- leaving permanent's components rather than meld's.
  OrderComponentCards :: Decider.Decider -> PlayerId.PlayerId -> Zone.Zone -> [PrintingId.PrintingId] -> Prompt [Natural.Natural]
  -- | The relative order of a per-object batch over one player's objects, a
  -- permutation of the indices; the chooser is CR 608.2f's resolving
  -- controller, CR 701.44d's exploring seat, or CR 707.10d's copying
  -- controller. Groups are asked in APNAP order (CR 101.4), each for two or
  -- more members.
  OrderForEach :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [Recipient.Recipient] -> Prompt [Natural.Natural]
  -- | CR 613.7m: the relative order of the timestamps one player's objects
  -- receive at one moment, first named stamped earlier; asked by
  -- Pawl.Engine.Restamp.order, per group in APNAP order, for two or more.
  OrderTimestamps :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [Natural.Natural]
  -- | CR 103.5: whether the player takes a mulligan, once per round in turn
  -- order until they keep; the offer carries what a player at a table sees.
  DeclareMulligan :: Decider.Decider -> PlayerId.PlayerId -> MulliganOffer.MulliganOffer -> Prompt MulliganDecision.MulliganDecision
  -- | CR 103.5: which `count` cards of the redrawn hand go to the bottom of the
  -- library, ordered; asked only at two or more cards.
  Bottom :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 103.5b: an action a card lets a player take "any time they could
  -- mulligan", offered before each DeclareMulligan and after each action
  -- taken; Nothing declines.
  MulliganAction :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, HandActionIndex.HandActionIndex)] -> Prompt (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  -- | CR 103.6: an opening-hand action once mulligans are complete, offered in
  -- turn order until the player declines; a card acted on is not offered again
  -- (CR 103.6b, Pawl.Types.HandWindowCap), and each action's own condition
  -- narrows the list.
  OpeningHandAction :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, HandActionIndex.HandActionIndex)] -> Prompt (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  -- | CR 603.5 / 608.2d: whether the named player exercises a printed "may",
  -- on the clause the ModeIndex and ClauseIndex name (CR 608.2e), asked of
  -- each player it covers in APNAP order. Elided only where the clause is
  -- inert (Pawl.Engine.Resolve.clauseIsInert, off the effects'
  -- classification).
  ChooseOptional :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> Prompt OptionalDecision.OptionalDecision
  -- | CR 608.2d's "or": which of a mode's mutually exclusive clauses happens,
  -- announced at resolution once per group and per chooser
  -- (Pawl.Types.OrElse.chooser), before ChooseOptional. Never elided. Not
  -- ChooseModes, which CR 700.2 fixes at cast.
  ChooseClause :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> NonEmpty.NonEmpty ClauseIndex.ClauseIndex -> Prompt ClauseIndex.ClauseIndex
  -- | CR 608.2g: whether the player casts the card a resolving effect allows
  -- them to (CR 310.12b), the CardName being the half CR 712.11a puts on the
  -- stack; never elided, and not raised for CR 608.2g's "instructs".
  OfferedCast :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> CardName.CardName -> Prompt OptionalDecision.OptionalDecision
  -- | CR 601.3 / 709.3 / 712.11b / 715.3: WHICH CAST an OfferedCast makes, from
  -- those Cast.castableWhenOffered admits; asked before the "may".
  --
  -- An object AND a name per option, because CR 601.3's offer ranges over a set
  -- of cards (Shell of the Last Kappa's exiled pile) as readily as over the
  -- halves of one (CR 709.3), and one prompt has to carry both -- a card with two
  -- castable halves contributes two options naming the same object. Elided when
  -- only one survives the gate.
  ChooseOfferedCastSpell :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty (ObjectId.ObjectId, CardName.CardName) -> Prompt (ObjectId.ObjectId, CardName.CardName)
  -- | CR 702.94a / 121.9: whether the drawn card is revealed for miracle, the
  -- cast being OfferedCast's separate "may"; never elided where asked.
  OfferedMiracleReveal :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> CardName.CardName -> Prompt OptionalDecision.OptionalDecision
  -- | CR 118.12 / 118.12a: whether the named player -- the one the effects are
  -- aimed at, not the resolving controller -- pays the cost a resolving object
  -- offers, once per payment in APNAP order. Not asked where CR 118.3 leaves
  -- only declining, nor for CR 118.12's mandatory limb
  -- (Pawl.Types.PayObligation). Pawl.ResolveSpec's PayGate group fails if this
  -- prompt is raised there.
  ChooseToPay :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> Cost.Cost Keyword.Keyword -> Prompt PaymentDecision.PaymentDecision
  -- | CR 601.2b: whether 2 life or mana pays each Phyrexian symbol, one prompt
  -- per symbol in printed order at CR 118.13's moments and for a combat toll
  -- (Pawl.Engine.Cost.announceToll); a hybrid Phyrexian symbol (CR 107.4f)
  -- then asks AnnounceHybridHalf. Elided when only one route is payable.
  AnnouncePhyrexianPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty PhyrexianPayment.PhyrexianPayment -> Prompt PhyrexianPayment.PhyrexianPayment
  -- | CR 601.2b: the nonhybrid equivalent for CR 107.4e's monocolored hybrid
  -- ({2\/R}); AnnouncePhyrexianPayment's contract and elision.
  AnnounceHybridPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaType.ManaType -> NonEmpty.NonEmpty HybridPayment.HybridPayment -> Prompt HybridPayment.HybridPayment
  -- | CR 601.2b: which mana type CR 107.4e's colour\/colour hybrid ({G\/U})
  -- takes; AnnouncePhyrexianPayment's contract, elided when only one half is
  -- payable and for the degenerate `Hybrid t t`.
  --
  -- Both halves spend one mana, so this never changes CR 601.2f's total. What
  -- it changes is WHICH unit of an oversupplied pool is consumed, and so what
  -- floats -- proven by Pawl.ManaSpec's "CR 601.2b whichever half of {G\/U} is
  -- announced, the OTHER floats" Gyre Engineer case.
  AnnounceHybridHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty ManaType.ManaType -> Prompt ManaType.ManaType
  -- | CR 118.7e: which half of a hybrid symbol a cost reduction is applied as,
  -- at CR 601.2f, answered with the resulting symbol; not filtered by
  -- payability, and elided only for the degenerate `Hybrid t t`.
  ChooseReductionHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty ManaSymbol.ManaSymbol -> Prompt ManaSymbol.ManaSymbol
  -- | CR 601.2f: which of the totals the cost reductions' orders reach the
  -- payer takes, cheapest first (Pawl.Engine.Cost.applyAdjustments); raised
  -- only where two totals differ, and not filtered by payability.
  ChooseReducedCost :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaCost.ManaCost -> Prompt ManaCost.ManaCost
