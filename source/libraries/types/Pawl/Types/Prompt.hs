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

data Prompt r where
  ChooseAction :: Decider.Decider -> PlayerId.PlayerId -> [Action.Action] -> Prompt Action.Action
  -- | CR 104.3a. Carries no Decider: CR 723.6 needs the ask to reach the true
  -- player, with nowhere to put a controller.
  Concede :: PlayerId.PlayerId -> Prompt Concession.Concession
  Shuffle :: [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 729.2, for a subgame's start. The NonEmpty is the turn order; the
  -- answer is the starting player, and the order is rotated to begin with them
  -- (CR 103.1). Carries no Decider: randomness is not a choice.
  RandomFirstPlayer :: NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | Which object randomness named, for
  -- Pawl.Types.ObjectRef.RandomCardInHand. Carries neither a Decider nor a
  -- PlayerId: randomness is not a choice, and CR 701.24a's reasoning makes who
  -- it is asked of unobservable. The engine never rolls -- the interpreter
  -- supplies the outcome and the caller filters it back against the offer.
  RandomObject :: NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | Which opponent randomness named, for
  -- Pawl.Types.Effect.ChooseOpponentAtRandom (Ruhan of the Fomori). The
  -- NonEmpty is the resolving controller's opponents still in the game (CR
  -- 104.3a); the answer is the one bound into the effect's slot. RandomObject's
  -- shape and reasons, asked only for two or more (CR 102.2).
  RandomOpponent :: NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 706.1a: what a die of the given size came up. The Natural is N, the
  -- die's face count, so the offer is the whole range 1 to N and an answer
  -- outside it takes rule 706.1a's floor. RandomObject's reasons for carrying
  -- neither a Decider nor a PlayerId. What comes back is CR 706.2's NATURAL
  -- result, the instruction's own modifier being added by the engine
  -- afterwards.
  RollDie :: Natural.Natural -> Prompt Natural.Natural
  -- | CR 705.1: which face a flipped coin came up. No payload at all, where
  -- RollDie carries N: the coin has exactly two equally likely sides, so the
  -- offer is the whole of Pawl.Types.CoinFace. RandomObject's reasons for
  -- carrying neither a Decider nor a PlayerId. Asked SECOND, after CallCoin,
  -- which is CR 705.2's order.
  FlipCoin :: Prompt CoinFace.CoinFace
  -- | CR 705.2: the face the flipping player calls before the coin comes up,
  -- winning the flip if it matches FlipCoin's answer. Carries a Decider,
  -- unlike FlipCoin, because the call IS a choice; the PlayerId is the
  -- flipper's seat, rule 705.2's last sentence involving no other player. CR
  -- 705.2's face-only effects have no winner and ask no call at all.
  CallCoin :: Decider.Decider -> PlayerId.PlayerId -> Prompt CoinFace.CoinFace
  -- | CR 514.2. The [ObjectId] is the hand; the Natural is how many to discard.
  ChooseDiscard :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 701.22a. The [ObjectId] is the top of the scrying player's own
  -- library, top-first; showing it to this seat IS CR 701.20e's look. The
  -- answer is (bottom, top), each in the order the cards end up reading from
  -- that end inward.
  ChooseScry :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.25a. ChooseScry's payload and look, with the FIRST list going to
  -- the player's graveyard in the order they are put there (CR 404.1, CR
  -- 404.3's arrangement).
  ChooseSurveil :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.29a. The first PlayerId is the fatesealer -- the seat asked and
  -- the only seat shown the cards -- and the second is the library's owner, who
  -- is asked nothing. The answer is ChooseScry's (bottom, top).
  ChooseFateseal :: Decider.Decider -> PlayerId.PlayerId -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt ([ObjectId.ObjectId], [ObjectId.ObjectId])
  -- | CR 701.44a: whether an exploring permanent's controller bins the revealed
  -- nonland card. The first ObjectId is the permanent, the second the revealed
  -- card. Reached only for a nonland card, the rule's own fork.
  ChooseExplore :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 507.1 / 703.4h: which opponent is the defending player. Not asked with
  -- one candidate, where CR 506.2 settles it instead.
  ChooseDefender :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 601.2g (CR 602.2b for an ability): which mana source to activate next
  -- while the pool does NOT yet cover the cost. Asked once per source TAPPED,
  -- against a shrinking candidate list; nothing declines (CR 118.3c), the
  -- payment then coming up short and CR 601.2h reversing it.
  --
  -- Never elided, single candidates included. The candidate LIST is collapsed
  -- to one source per interchangeability class
  -- (Pawl.Engine.Interchangeable), so two Llanowar Elves may be one candidate
  -- unless an Equipment, an Aura or a counter tells them apart.
  ChooseManaSource :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 605.3a, asked once the pool ALREADY covers the cost: which further
  -- source to activate, or Nothing to close CR 601.2g's window and pay.
  -- Floating is observable (Omnath, Locus of Mana), so this is never elided,
  -- and its candidate list is collapsed exactly as ChooseManaSource's is.
  ChooseExtraManaSource :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 605.3b: which mana the source produces, asked as the mana ability
  -- resolves. A ManaOption is the whole mana one activation adds together with
  -- what CR 602.2b charges for it, so "{T}: Add {C}{C}" is one candidate.
  -- Candidates are deduplicated by the whole option (Mana.manaOptionsOf).
  ChooseManaYield :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaOption.ManaOption -> Prompt ManaOption.ManaOption
  -- | CR 601.2h: which mana in the payer's pool goes toward one symbol of a
  -- cost, asked once per mana spent; CR 107.4b makes a generic symbol one of
  -- these too. Choose, not target. Candidates are the units that could pay this
  -- symbol and still leave the rest of the cost payable, DEDUPLICATED. Elided
  -- where every way of paying leaves the same pool
  -- (Pawl.Engine.Mana.leftovers).
  ChooseManaToSpend :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ManaUnit.ManaUnit -> Prompt ManaUnit.ManaUnit
  -- | CR 701.34a. The lists are every permanent and player holding a counter;
  -- the answer is the subset of each that gets one more of every kind it has.
  -- Choose, not target. Elided only when both lists are empty: "any number"
  -- makes even one candidate a real yes or no.
  ChooseProliferate :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [PlayerId.PlayerId] -> Prompt (Set.Set ObjectId.ObjectId, Set.Set PlayerId.PlayerId)
  -- | Reverse the Sands' redistribution (CR 119.7-8 name the action). The
  -- [(PlayerId, Integer)] is every player beside the total they hold, read
  -- before anything moves (CR 608.2h). The answer maps each CHOSEN player to
  -- the player whose previous total they take -- a permutation of the chosen
  -- subset, which Pawl.Engine.Resolve checks. Choose, not target; elided only
  -- below two candidates.
  --
  -- Not implemented: CR 810.9f's "not more than one member of each team", which
  -- is a Two-Headed Giant rule (#2849).
  ChooseRedistribution :: Decider.Decider -> PlayerId.PlayerId -> [(PlayerId.PlayerId, Integer)] -> Prompt (Map.Map PlayerId.PlayerId PlayerId.PlayerId)
  -- | CR 701.54a: which creature a tempted player controls becomes their
  -- Ring-bearer. Choose, not target. Raised only for two or more candidates.
  -- Not raised for zero, where CR 701.54d still tempts and still grants the
  -- emblem.
  ChooseRingBearer :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.39a. The ObjectId is the spell or ability resolving; the NonEmpty
  -- is the creatures its controller controls tied for the LEAST toughness,
  -- already narrowed, so every candidate offered is legal. Choose, not target;
  -- ChooseRingBearer's two-or-more rule.
  ChooseBolster :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.47a. The ObjectId is the spell or ability resolving; the NonEmpty
  -- is the Army creatures its controller controls, read AFTER the rule's token
  -- is created. ChooseBolster's posture, constructor argument and elision; not
  -- raised for zero, which is CR 701.47b.
  ChooseAmass :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.68a. The ObjectId is the object the blight was asked for -- a
  -- resolving spell or ability, or the one whose cost is being paid; the
  -- NonEmpty is the creatures its chooser controls, UNNARROWED, which is the
  -- difference from ChooseBolster. ChooseBolster's posture, constructor
  -- argument and elision; not raised for zero, by CR 101.3 or CR 701.68b
  -- depending on the caller.
  ChooseBlight :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 122.5: which KIND of counter a move takes off the first object
  -- (Agent's Toolkit). The first ObjectId is the object the counter leaves, the
  -- second the object it lands on; the NonEmpty is the kinds the first object
  -- actually HAS, engine-pre-filtered. ChooseBolster's posture. Raised only
  -- where the card leaves the kind OPEN and two or more kinds are available;
  -- rule 122.5 moves nothing where the first object has no appropriate counter.
  ChooseMovedCounter :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> NonEmpty.NonEmpty (CounterKind.CounterKind Keyword.Keyword) -> Prompt (CounterKind.CounterKind Keyword.Keyword)
  -- | CR 122.5: WHICH counters a move takes off the first object, and how many
  -- of each (Resourceful Defense), where the card leaves the COUNT open. The
  -- first ObjectId is the object the counters leave, the second the object they
  -- land on; the Map is what the first object HAS, engine-pre-filtered to the
  -- kinds this move could really carry, and the answer is how many of each to
  -- move, clamped rather than trusted.
  --
  -- Raised for ONE candidate as well as for two, unlike ChooseMovedCounter:
  -- "any number" includes none, so a lone kind bearing a lone counter is still
  -- a real choice. Not raised where the first object has no movable kind at
  -- all.
  ChooseMovedCounters :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> Prompt (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)
  -- | CR 122.5: ChooseMovedCounters' question where the card states a FLOOR
  -- (Goldberry, River-Daughter's "move one or more counters"), so the empty
  -- answer is excluded and one moving nothing is repaired to one counter of the
  -- first kind offered. Its own constructor because an empty answer is legal to
  -- one prompt and not the other.
  --
  -- Raised wherever the first object has a movable kind at all: the player
  -- still chooses WHICH kinds and how many above the floor. Not raised where
  -- nothing could cross.
  ChooseMovedCountersAtLeastOne :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> Prompt (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural)
  -- | CR 122.5: how many counters of each kind a move puts onto EACH of the
  -- objects it names, where the second side is a whole GROUP (Forgotten
  -- Ancient). The ObjectId is the object the counters leave, the Map is what it
  -- HAS, engine-pre-filtered, and the NonEmpty is the destinations, each of
  -- which can take at least one offered kind; the answer is a tally per kind
  -- for each destination, and the sum is what comes off the first object.
  --
  -- ChooseMovedCounters' posture otherwise: choose, not target, and filtered
  -- rather than trusted -- an unoffered object or a refused kind is dropped,
  -- and the tallies are clamped in offered order.
  ChooseDistributedMovedCounters :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural))
  -- | CR 122.5: WHICH counter a move takes off the first object, OR NONE
  -- (Takesies' "move up to one counter"). ChooseMovedCounter's question with
  -- declining added, and an answer naming an unoffered kind is read as
  -- declining.
  --
  -- Raised for ONE candidate as well as for two, unlike ChooseMovedCounter:
  -- "up to one" includes none. Not raised where the first object has no movable
  -- kind at all.
  ChooseMovedCounterOrNone :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ObjectId.ObjectId -> NonEmpty.NonEmpty (CounterKind.CounterKind Keyword.Keyword) -> Prompt (Maybe (CounterKind.CounterKind Keyword.Keyword))
  -- | CR 107.14: how much {E} this player pays to an Effect.PayAnyEnergy, asked
  -- as the spell or ability RESOLVES (Harnessed Lightning). The ObjectId is the
  -- resolving object; the Natural is the payer's own energy count, ENFORCED
  -- rather than advisory (CR 118.3), Pawl.Engine.Resolve clamping the answer to
  -- it. ZERO is a legal answer and is how the "may" is declined, so this prompt
  -- carries no separate yes/no.
  --
  -- Not implemented: skipping it when the bound is 0, where the one payable
  -- amount is determined (#1920).
  ChoosePaidEnergy :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 702.155b / 714.3b: which chapter a Saga with read ahead enters on,
  -- asked as the Saga enters (Love Song of Night and Day). The ObjectId is the
  -- entering Saga; the Natural is CR 714.2d's final chapter number, the top of
  -- the inclusive range, ENFORCED rather than advisory -- Pawl.Engine.Event
  -- clamps the answer into rule 702.155b's closed range.
  --
  -- NOT RAISED where the range holds one number, EntryRewrite.ChoiceOf's
  -- elision, nor for a bound of 0, which is CR 714.2d's abilityless Saga.
  ChooseReadAheadChapter :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 609.7a: which SOURCE OF DAMAGE a player chooses for a prevention or
  -- redirection effect that names one (Healing Grace, Oracle's Attendants). The
  -- ObjectId is the spell or ability resolving; the NonEmpty is the sources it
  -- may choose from, already narrowed to the effect's printed properties.
  -- ChooseBolster's posture, constructor argument and elision -- rule 609.7a
  -- says "choice", so nothing was declared on the stack. Not raised for zero,
  -- where no shield is installed at all.
  ChooseDamageSource :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 603.7b, second sentence: which of several SIMULTANEOUS occurrences of
  -- its trigger event causes a delayed triggered ability with no stated
  -- duration to trigger. The PlayerId is the entry's controller, whom the rule
  -- names as the chooser; the ObjectId is the ability's source; the NonEmpty is
  -- the matching events inside the earliest Pawl.Types.EventGroup that holds
  -- one, in log order, and the answer INDEXES that list, since one group can
  -- hold two occurrences that compare equal.
  --
  -- ChooseBolster's posture; raised only where two or more make it a real
  -- choice. An entry WITH a stated duration never reaches it, CR 603.2c firing
  -- it once per occurrence instead.
  ChooseDelayedTriggerEvent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty GameEvent.GameEvent -> Prompt Natural.Natural
  -- | CR 608.2d: which card in a graveyard a player chooses for a
  -- Pawl.Types.ObjectRef.ChosenCardInGraveyard. The PlayerId is the CHOOSER,
  -- which the ref's Pawl.Types.Chooser decides and who need not own the
  -- graveyard; the ObjectId is the spell or ability resolving; the NonEmpty is
  -- the matching cards, engine-pre-filtered. One question per chooser, in APNAP
  -- order (CR 608.2e, CR 101.4). Choose, not target (CR 115.1). Raised only for
  -- two or more candidates.
  ChooseCardInGraveyard :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 608.2d: which card in their OWN hand a player chooses for a
  -- Pawl.Types.ObjectRef.ChosenCardInHand. ChooseCardInGraveyard's payload,
  -- posture and elision, with no second PlayerId: the chooser owns the hand and
  -- is the only seat the candidates are shown to (CR 400.2, CR 402.3).
  --
  -- Also the PAYMENT's question, over the same zone and the same posture:
  -- Pawl.Types.CostComponent's PutCardFromHandOntoBattlefield asks it while a
  -- CR 118.12 cost is paid, where the ObjectId is the object the cost is on
  -- rather than the object resolving.
  ChooseCardInHand :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 608.2d: which card of a bound GROUP a player chooses for a
  -- Pawl.Types.ObjectRef.ChosenCardFromAmong (Commune with the Gods).
  -- ChooseCardInGraveyard's payload, posture and elision, with the candidates
  -- coming from the slot an earlier clause of this resolution bound rather than
  -- from a zone -- the group's cards are still in the library the reveal showed
  -- them from (CR 701.20b).
  --
  -- The PlayerId is the seat the ref's chooser names: CR 608.2c's resolving
  -- controller by default, Animal Magnetism's opponent otherwise. Asked once
  -- PER CARD for a ref whose count is above one, each ask offering what the
  -- earlier ones did not take, and ONE prompt however many opcodes read the
  -- answer (Carth the Lion).
  ChooseCardFromAmong :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 309.2a \/ 701.49a: which dungeon card a venturing player who owns no
  -- dungeon in the command zone brings in from outside the game. The NonEmpty
  -- is the printings they own (Pawl.Types.Player's dungeons), in interning
  -- order. Choose, not target. Raised only for two or more.
  --
  -- No source ObjectId, and a PRINTING rather than an ObjectId: CR 400.11 puts
  -- the card outside the game, so no object stands for it until the answer is
  -- given.
  ChooseDungeon :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty PrintingId.PrintingId -> Prompt PrintingId.PrintingId
  -- | CR 400.11c \/ 729.4: which card a player brings in from OUTSIDE THE GAME
  -- (Burning Wish). The NonEmpty is every OutsideCard.OutsideCard the effect\'s
  -- Filter matches, in interning order: CR 103.2a\'s sideboard pool inside a
  -- main game, plus CR 729.4\'s main-game objects when this is raised from
  -- inside a subgame. Choose, not target (CR 115.10a), CR 400.11c letting
  -- nothing target a card out there at all.
  --
  -- ChooseDungeon\'s shape and for its reasons. OutsideCard rather than a bare
  -- PrintingId because which zone the card leaves decides which main-game
  -- abilities trigger (CR 729.4a), so the pool and the main game are not
  -- interchangeable sources.
  --
  -- Raised only for two or more, one matching card leaving nothing to ask (CR
  -- 103.2a\'s sideboard is a multiset, so two copies of one printing are one
  -- offer). Whether to reveal AT ALL is the clause\'s printed may and is asked
  -- before this.
  ChooseFromOutsideTheGame :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty OutsideCard.OutsideCard -> Prompt OutsideCard.OutsideCard
  -- | CR 309.5a \/ 701.49b: which arrow a venturing player follows. The
  -- ObjectId is the dungeon card their marker is on; the NonEmpty is the rooms
  -- the arrows out of their current room lead to. Choose, not target. Raised
  -- only for two or more arrows, which is CR 309.5a's own condition.
  ChooseRoom :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty RoomIndex.RoomIndex -> Prompt RoomIndex.RoomIndex
  -- | CR 709.5f \/ 709.5g: WHICH HALF of a permanent an effect locks or
  -- unlocks. The ObjectId is the permanent; the NonEmpty is the halves the
  -- instruction admits, in printed order. Answered with a CardName because rule
  -- 709.4a gives each half its own name, which is how Object.unlockedHalves
  -- keys CR 709.5c's designations. Choose, not target. Raised only for two or
  -- more, and not at all for an instruction naming every admitted half
  -- (Pawl.Types.SetHalfLocked's `every`).
  --
  -- HALVES and not doors: CR 709.5j makes "door" what a card calls a half, and
  -- rules 709.5f\/g word both instructions over any permanent with halves.
  ChooseHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty CardName.CardName -> Prompt CardName.CardName
  -- | CR 704.5j: which of two or more same-named legendary permanents its
  -- controller keeps. One prompt per name, not per player. The answer is what
  -- is KEPT, as the rule is worded. Never elided: the group is always two or
  -- more, and the permanents may differ in ways the shared name cannot see.
  ChooseLegend :: Decider.Decider -> PlayerId.PlayerId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 508.1a. The [ObjectId] is the legal attackers; the answer is which of
  -- them attack. WHAT each attacks is ChooseAttackTarget's separate question,
  -- CR 508.1b being a separate step.
  DeclareAttackers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [ObjectId.ObjectId]
  -- | CR 508.1b. The ObjectId is the creature being announced; the NonEmpty is
  -- what it may attack, the defending player first. One prompt PER CREATURE, as
  -- the rule asks. CR 508.4's put-onto-the-battlefield-attacking reaches the
  -- same prompt, being the same question with the same chooser (CR 506.3b).
  -- Elided at one candidate, which is CR 508.1b's own condition read backwards.
  ChooseAttackTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty AttackTarget.AttackTarget -> Prompt AttackTarget.AttackTarget
  -- | CR 508.1g / 701.43d: whether one chosen attacker with exert is exerted.
  -- One prompt PER CREATURE, ChooseAttackTarget's shape, since exerting one
  -- attacker and not another is an observably different declaration. Never
  -- elided while the creature has the keyword: CR 701.43b makes a permanent
  -- exertable even when already exerted, so no board makes the answer moot.
  ChooseExert :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 509.1. The legal blockers, then the attackers they may block; the
  -- answer maps each blocking creature to the attackers it blocks, and a
  -- blocker that blocks nothing is absent.
  --
  -- A SET per blocker, never a single attacker: CR 509.1a's one attacker can be
  -- raised by a permission (Foriysian Brigade), and fixed arity is the
  -- recurring root cause (design doc §2.11). Which attackers each blocker may
  -- take is not offered per blocker, the restrictions being pairwise (CR
  -- 509.1b); Pawl.Engine.Combat.legalBlockDeclaration judges the answer.
  DeclareBlockers :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> [ObjectId.ObjectId] -> Prompt (Map.Map ObjectId.ObjectId (Set.Set ObjectId.ObjectId))
  -- | CR 510.1 / 702.19b: a combatant divides its power among the legal
  -- recipients. The ObjectId is the assigning creature -- an attacker under CR
  -- 510.1c or a blocker under CR 510.1d. The Map is recipient -> lethal
  -- threshold (the defender -> 0), every threshold being 0 on the blocking
  -- side, CR 510.1d imposing no minimum.
  --
  -- Clearing a threshold is a question about the whole combat damage step (CR
  -- 702.19b, CR 702.19c), so an answer under one can still be legal. Validation
  -- is Damage.wellFormedAssignment, then Damage.tiersCleared over every
  -- attacker's answer at once.
  AssignCombatDamage :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map Recipient.Recipient Natural.Natural -> Natural.Natural -> Prompt (Map.Map Recipient.Recipient Natural.Natural)
  -- | CR 601.2c. Per named slot of the spell being cast (the ObjectId): how
  -- many targets it takes, and the legal recipients. The answer fills every
  -- slot with exactly the stated number. Slots agree by NAME, never by
  -- position. The slots offered are the ones that WILL be filled, at the counts
  -- AnnounceTargets settled, so a declined CR 115.6 slot is absent.
  ChooseTargets :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map SlotName.SlotName (Natural.Natural, Set.Set Recipient.Recipient) -> Prompt (Map.Map SlotName.SlotName (Set.Set Recipient.Recipient))
  -- | CR 601.2c's other announcement, made BEFORE ChooseTargets: how many
  -- targets a variable slot will take. The Map holds only the variable slots,
  -- each with the range it may be answered within, already narrowed to what the
  -- board can supply. The candidate sets ride along because an unbounded count
  -- is bounded by them and nothing else (Pawl.Types.TargetCount.ceilingOn).
  AnnounceTargets :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Map.Map SlotName.SlotName (TargetCount.TargetCount, Set.Set Recipient.Recipient) -> Prompt (Map.Map SlotName.SlotName Natural.Natural)
  -- | CR 612: choose the two basic land types for a text-changing spell's slot.
  -- The ObjectId is the resolving spell; the Set is the words the NEW type may
  -- not be, from the text-changer's own card text, and is empty for a card that
  -- restricts nothing. Asked as the effect is APPLIED rather than as the spell
  -- is cast, which is where CR 608.2d places a choice that is none of CR
  -- 601.2b-d's announcements.
  ChooseLandTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 612: ChooseLandTypeSwap's sibling for the other family CR 612.2 names,
  -- a creature type word used as a creature type. Same moment and same reason.
  -- No candidate list, unlike ChooseLandTypeSwap's implied five: CR 205.3m's
  -- creature types run to hundreds, so the family is named rather than
  -- enumerated.
  ChooseCreatureTypeSwap :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> SlotName.SlotName -> Set.Set Subtype.Subtype -> Prompt (Subtype.Subtype, Subtype.Subtype)
  -- | CR 614.1c: as an object enters, its controller chooses ONE basic land
  -- type (Convincing Mirage). The ObjectId is the entering object. No candidate
  -- list, CR 305.6 fixing the five types, and no SlotName -- the answer is
  -- written to Object.chosenSubtype rather than bound into a slot.
  ChooseBasicLandType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Subtype.Subtype
  -- | The printed "and/or" of a multi-zone search: which of the zones the card
  -- names this searcher actually looks through (Delivery Moogle, Dark
  -- Supplicant). The Set is the zones OFFERED, all owned by one player, and the
  -- answer is a NONEMPTY subset intersected back with it.
  --
  -- No rule of the CR spells "and/or"; it is the card's own English for "one,
  -- the other, or both". Boonweaver Giant is why nonempty: it prints "you MAY
  -- search your graveyard, hand, and/or library", and that "may" would be
  -- redundant if the and/or already permitted searching nothing. An empty
  -- answer would dodge the forced find of a public zone (CR 400.2, CR 701.23b)
  -- and the card's own "if you search your library this way, shuffle".
  --
  -- Raised only where two or more zones make it a real choice. Asked once per
  -- searcher, ahead of CR 601.3's offer and of the search itself.
  ChooseSearchZones :: Decider.Decider -> PlayerId.PlayerId -> Set.Set Zone.Zone -> Prompt (Set.Set Zone.Zone)
  -- | CR 701.23 / 701.23b. The [ObjectId] is the MATCHING cards
  -- (engine-pre-filtered) across every zone this searcher is looking through --
  -- the ones ChooseSearchZones settled, where the card printed "and/or" -- and
  -- the PlayerId is the player SEARCHING, who need not own those zones. The
  -- Natural is how many the search may find; a search that states no count
  -- (Mana Severance) arrives here as the number of MATCHING cards, which is the
  -- bound CR 701.23a gives it.
  --
  -- ONE prompt over the union rather than one per zone, and no zone in the
  -- payload: the card prints one instruction with one count over the zones it
  -- names. A LIST rather than a repeated prompt, CR 701.23a's find being one
  -- look at the whole zone.
  --
  -- Answering with fewer is legal for a search of a HIDDEN zone (CR 400.2)
  -- stating a quality (CR 701.23b's "some or all"), for one printing "up to"
  -- (Search.upTo) and for one stating no count; the empty answer is "fail to
  -- find". A bare quantity with none of the three is CR 701.23d, and a public
  -- zone is under none of the three rules at all, so Pawl.Engine.Resolve
  -- completes a short answer from those zones instead.
  Search :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 608.2g: the re-entrant cast opportunity during a library search
  -- (Panglacial Wurm), following CR 601.2a-i except that no player receives
  -- priority afterwards. Nothing declines. Offered in a loop before the search
  -- finds, so multiple copies may be cast; CR 605.3a permits mana activation.
  -- One entry per castable HALF, paired with the name CR 709.3 needs.
  CastWhileSearching :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, CardName.CardName)] -> Prompt (Maybe (ObjectId.ObjectId, CardName.CardName))
  -- | CR 601.2b: choose the value of X while casting a spell, or through CR
  -- 602.2b while activating an ability. The ObjectId is whichever object is on
  -- the stack. The Natural is the greatest value this player could LEGALLY
  -- ANNOUNCE now, climbed by Cast.affordableX / Activate.affordableX over the
  -- cost that will really be paid and stopped at CR 101.1's card-stated ceiling
  -- where the face prints any (Cost.maximumX); the payable half counts LIFE as
  -- well as mana, Cost.canPay measuring CR 601.2b's nonhybrid resolutions.
  --
  -- ADVISORY, and the answer is filtered against it nowhere -- but the two
  -- halves of the bound are refused by DIFFERENT rules once it is answered. CR
  -- 601.2b lets the player announce past what they can PAY, and CR 601.2 /
  -- 602.2's reversal is what answers that (#741); announcing past what the CARD
  -- permits is CR 101.1 and CR 101.2, refused by Cast's own gate.
  --
  -- Prompted before targets, and only when the cost declares an X -- a
  -- ManaSymbol.Variable, a CostComponent.PayLifeX (Hatred), a
  -- CostComponent.BlightX (Soul Immolation), or a CostComponent.PayEnergyX
  -- (Sphinx of the Revelation); CR 107.3a is the general statement.
  ChooseX :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt Natural.Natural
  -- | CR 702.42a: whether this player entwines the modal spell they are
  -- casting. The Cost is what entwining adds on top of the candidate cost then
  -- announced (CR 601.2f). Asked BEFORE ChooseModes, which then has nothing to
  -- ask.
  ChooseEntwine :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Cost.Cost Keyword.Keyword -> Prompt EntwineDecision.EntwineDecision
  -- | CR 702.33a: how many times this player pays ONE of the spell's kicker
  -- costs. The Cost is what each payment adds on top of the candidate cost then
  -- announced (CR 601.2f), and is what tells a spell's two kicker costs (CR
  -- 702.33b) apart. Asked after ChooseModes and before ChooseCost, CR 601.2b's
  -- own order.
  --
  -- The Natural is the LIMIT: one for rule 702.33a's kicker, and Nothing for
  -- 702.33c's multikicker. An answer past a stated limit is text the card does
  -- not have, so Pawl.Engine.Cast rejects the cast rather than clamping it.
  ChooseKicker :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Cost.Cost Keyword.Keyword -> Maybe Natural.Natural -> Prompt KickerDecision.KickerDecision
  -- | CR 903.9a: this player's commander is in a graveyard or exile, having
  -- arrived since the last state-based action check. The rule is a "may", so
  -- the object stays put if they decline and is not asked again until it moves
  -- there afresh. The OWNER is asked, never the controller.
  ReturnCommander :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt CommandZoneDecision.CommandZoneDecision
  -- | CR 401.2: which END of a library a card arrives at, when the effect
  -- leaves the choice open (Aetherspouts). The ObjectId is the card being
  -- placed, read PRE-MOVE, so two cards put distinguishable questions on the
  -- wire. The OWNER is asked, per CR 400.3, not CR 608.2f's resolving
  -- controller. Never elided; a STATED end
  -- (Pawl.Types.LibraryPlacement.Stated) is not asked at all. Asked in APNAP
  -- order (CR 101.4, CR 608.2f).
  ChooseLibraryEnd :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt LibraryPosition.LibraryPosition
  -- | CR 401.4: the owner arranges two or more cards arriving at one end of a
  -- library at once. The LibraryPosition is that end, on the wire because the
  -- answer's meaning depends on it; the [ObjectId] is the batch in the engine's
  -- canonical order; the answer is a permutation of the INDICES, read as the
  -- order the cards end up in from that end inward.
  --
  -- A DIFFERENT decider from CR 608.2f's secondary sentence: rule 401.4 takes
  -- relative order back for a library destination and gives it to the cards'
  -- owner. Asked only for two or more, which is CR 401.4's own wording; still
  -- asked for copies of one printing, they being distinct objects.
  ArrangeLibraryArrivals :: Decider.Decider -> PlayerId.PlayerId -> LibraryPosition.LibraryPosition -> [ObjectId.ObjectId] -> Prompt [Natural.Natural]
  -- | CR 601.2b / 700.2a: choose the mode(s) while casting. The Set ModeIndex
  -- is the LEGAL modes, pre-filtered to ones whose targets are all fillable
  -- (CR 700.2a); the ModeSelection is the printed instruction being satisfied,
  -- including whether CR 700.2d lets one mode be chosen twice. The answer is a
  -- Seq and not a Set because of that exception; the engine rejects a repeat
  -- under the ordinary instruction and sorts either way (CR 608.2c).
  --
  -- Prompted before X and targets, and only when there is a real choice: a
  -- forced selection is not asked, which is every entwined cast.
  ChooseModes :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Set.Set ModeIndex.ModeIndex -> ModeSelection.ModeSelection -> Prompt (Seq.Seq ModeIndex.ModeIndex)
  -- | CR 707.5 / 614.1c / 614.12a: as an object enters AS A COPY, its
  -- controller chooses which permanent to copy. Nothing is the card's own "may"
  -- decline, after which it enters as itself. Answered inside the zone change
  -- before the enters event is recorded. The legal set is the CARD's printed
  -- noun phrase over the battlefield, minus anything entering in the same batch.
  --
  -- Asked whenever one or more are eligible, the "may" making a single
  -- candidate a real fork; not asked with none, where declining is forced.
  ChooseCopyTarget :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 208.2b / 614.1c: as an object enters, its controller chooses among the
  -- shapes an "as this creature enters, it becomes your choice of ..." ability
  -- offers (Primal Plasma). The answer is an index into the offered list, and
  -- the chosen shape is written into the object's COPIABLE snapshot (CR 707.2).
  -- Asked only for two or more options.
  ChooseEntryOption :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [EntryOption.EntryOption] -> Prompt Natural.Natural
  -- | CR 702.136a / 614.1c: whether a permanent with riot enters with an
  -- additional +1/+1 counter instead of haste. Exercises is the counter,
  -- Declines the haste, which is the rule's own reading order. Never elided.
  ChooseRiot :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 702.98a / 614.1c: whether a permanent with unleash enters with an
  -- additional +1/+1 counter. Exercises takes the counter. Distinct from
  -- ChooseRiot so a transcript cannot answer one as-enters "may" as another.
  -- Never elided: the counter costs the creature its ability to block.
  ChooseUnleash :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 614.1c with CR 119.4: "As this permanent enters, you may pay N life.
  -- If you don't, it enters tapped" (Razorgrass Field). The Natural is the
  -- amount, which is card text rather than a rule's. Exercises pays and enters
  -- untapped. Not asked below N life, where CR 119.4 leaves declining as the
  -- only legal answer; CR 119.4b makes a zero amount payable at any total, so
  -- that case is still asked.
  ChoosePayLifeOnEntry :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Natural.Natural -> Prompt OptionalDecision.OptionalDecision
  -- | CR 614.1c with CR 701.20a: "As this permanent enters, you may reveal a
  -- [matching] card from your hand. If you don't, it enters tapped" (Rustic
  -- Clachan). The NonEmpty is the hand cards the printed filter admits, and the
  -- engine honours an answer only if it is in that list; Nothing is the
  -- decline.
  --
  -- Names a CARD rather than ChoosePayLifeOnEntry's OptionalDecision because
  -- which card was shown is public (CR 701.20a). Raised at one candidate too --
  -- revealing and declining leave different boards.
  ChooseRevealOnEntry :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt (Maybe ObjectId.ObjectId)
  -- | CR 614.1c: as an object enters, its controller chooses a colour
  -- (Painter's Servant). No candidate list, CR 105.1 fixing the five colours.
  ChooseColor :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt Color.Color
  -- | CR 105.4 with CR 106.3: which mana a resolving spell or ability adds when
  -- the type it names is not settled (Quirion Sentinel). The PlayerId is the
  -- player instructed to add it, whose pool CR 106.4 holds it. The offer is
  -- Pawl.Engine.Mana.producedTypes' answer for THIS production, and the answer
  -- is a ManaType, so it IS the unit that lands in the pool. Asked only for two
  -- or more candidates.
  ChooseManaType :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaType.ManaType -> Prompt ManaType.ManaType
  -- | CR 201.4 / 614.1c: as an object enters, a player chooses a card name
  -- (Null Chamber). The PlayerId is the CHOOSER -- the entering object's
  -- controller for one of Null Chamber's two asks and an opponent for the other
  -- -- and Pawl.Engine.Replacement asks in APNAP order (CR 101.4). The Filter
  -- is CR 201.4a's restriction, read off the card that asks.
  --
  -- Not implemented: no candidate list and no validation of the answer against
  -- the Filter, CR 201.4's Oracle card reference not being a set the engine
  -- holds (#663).
  ChooseCardName :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Filter.Filter Keyword.Keyword -> Prompt CardName.CardName
  -- | Which opponent a card's text names (Null Chamber). The ObjectId is the
  -- object whose text names the opponent. Two moments reach it, being the same
  -- question: as a permanent enters, where CR 614.12a puts the ask first, and
  -- at resolution, where CR 701.29a's fateseal picks the library looked at
  -- (Spin into Myth).
  --
  -- WHO is asked is pawl's reading at the entry moment -- Null Chamber leaves
  -- "an opponent" open and no rule assigns the pick, so it goes to CR 109.5's
  -- "you"; at the fateseal moment it is rule 701.29a's own actor. Asked only
  -- for two or more, CR 102.2 leaving a two-player game exactly one opponent.
  ChooseOpponent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 310.9a: which player protects a battle. The chooser is the battle's
  -- controller, which the rule assigns; the NonEmpty is
  -- Pawl.Engine.Battle.protectorCandidates. Asked only for two or more
  -- candidates.
  --
  -- Carries the battle because two places ask: CR 310.9a as it enters, and CR
  -- 310.11 as a state-based action (CR 704.5x, CR 704.5y) when the designation
  -- has become illegal. Not ChooseOpponent reused: a battle with no battle
  -- types takes its own controller (CR 310.9a), whom that prompt never offers.
  ChooseProtector :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 614.1c with CR 614.12a: which player this permanent's controller
  -- chooses as it enters (Stuffy Doll). The chooser is CR 109.5's "you"; the
  -- NonEmpty is every player still in the game (CR 102.1), which CR 800.4 has
  -- already emptied of anyone who left. Asked only for two or more.
  ChoosePlayer :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty PlayerId.PlayerId -> Prompt PlayerId.PlayerId
  -- | CR 603.3b: each player, in APNAP order, puts the triggered abilities they
  -- control on the stack in any order. The [TriggerEntry] is that player's
  -- pending triggers in the engine's canonical order; the answer is a
  -- permutation of the INDICES, so the last named resolves first. A
  -- TriggerSource rather than an ObjectId inside the entry, because the
  -- inherent abilities (CR 725.2's monarch pair, CR 702.179d's speed increase)
  -- have no source.
  --
  -- Positional, but not positional BY NECESSITY: equal entries can still be
  -- distinguishable, since CR 117.3b makes which of two same-ability triggers
  -- resolved first visible whenever the payload reads one (Aether Flash on two
  -- entrants). So the elision is two or more AND the order observable;
  -- Pawl.Engine.Engine.orderInert is the other half.
  --
  -- Asked once per PASS of CR 603.3b's two-part process, not once per batch, so
  -- the entries offered are always all of one class.
  OrderTriggers :: Decider.Decider -> PlayerId.PlayerId -> [TriggerEntry.TriggerEntry] -> Prompt [Natural.Natural]
  -- | CR 615.7: with damage from two or more applicable sources at once, the
  -- shielded player or the permanent's controller chooses which the shield
  -- prevents. The answer is a permutation of the events' INDICES, giving the
  -- order the shields are offered them, and the whole DamageEvent is on the
  -- wire because the amounts are what the answer turns on.
  --
  -- An ORDER rather than a pick: within one event a shield covers as much as it
  -- can and then moves on. Asked only when the order is observable -- two or
  -- more events one shield admits, and a remaining amount neither 0 nor enough
  -- to cover all of them.
  OrderDamage :: Decider.Decider -> PlayerId.PlayerId -> [DamageEvent.DamageEvent] -> Prompt [Natural.Natural]
  -- | CR 616.1: with two or more applicable effects in the highest non-empty
  -- bucket, the affected object's controller (or owner, or the affected player)
  -- chooses which to apply NEXT; CR 616.1f then re-collects, so this is asked
  -- once per iteration. The answer is an index into the candidates, which are
  -- in the engine's canonical order (battlefield ascending, then the floating
  -- store). A PICK and not a permutation, unlike OrderTriggers, because the
  -- rest of the order is never settled here.
  --
  -- Asked only when the bucket holds two or more candidates that are not all
  -- indistinguishable -- equal in the EFFECT, plus, where application reads the
  -- applying candidate (Replacement.readsApplier), equal in CR 109.5's "you".
  -- Each still gets its own CR 614.5 opportunity.
  ChooseReplacement :: Decider.Decider -> PlayerId.PlayerId -> [ReplacementEntry.ReplacementEntry] -> Prompt Natural.Natural
  -- | CR 603.7c: which of several minted tokens a Create's slot binds -- the
  -- "it" a delayed triggered ability armed in the same resolution will name.
  -- The ObjectId is the effect's source; the NonEmpty is the tokens minted, in
  -- creation order. Asked only for two or more tokens.
  --
  -- Reachable only through a replacement: a Create whose printed quantity is
  -- anything but one binds every token instead (Resolve.namesEveryToken), so
  -- the singular "it" starts with a single candidate and CR 614.16 scales the
  -- count at runtime. CR 707.10e is the codified analogue that settles this as
  -- a choice rather than something the engine may decide.
  ChooseBoundToken :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 701.21a: which permanents to sacrifice to pay a cost. The ObjectId is
  -- the spell being cast or the permanent whose ability is activated; the
  -- [ObjectId] is the payer's matching permanents, engine-pre-filtered; the
  -- Natural is how many. A Set, since one permanent cannot be sacrificed twice
  -- for one payment. Asked only when there are more candidates than the count.
  --
  -- Deliberately NOT ChooseTargets or the TargetSlot machinery: CR 115.1 makes
  -- a target only what the word "target" names, and conflating them would let
  -- shroud, hexproof and "becomes the target" triggers observe a sacrifice.
  ChooseSacrifices :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 406.2: which cards to exile from the paying player's own graveyard to
  -- pay a cost (Headless Skaab). ChooseSacrifices' payload, posture and
  -- elision, over cards in a graveyard rather than permanents on the
  -- battlefield.
  ChooseExilesFromGraveyard :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 614.1c with CR 614.13a: sacrifice ANY NUMBER of the candidates as this
  -- permanent enters (Shimatsu the Bloodcloaked). No count field, the offer
  -- being the candidate list and every subset admissible including the empty
  -- one. Asked even at one candidate, unlike ChooseSacrifices: a free choice of
  -- subset still leaves two answers. At zero the caller skips it. Answers as
  -- Response.ChoseSacrifices.
  ChooseAnyNumberToSacrifice :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 608.2d: which of the permanents an effect offers to act on the player
  -- picks, where the effect states no count (Tovolar, Dire Overlord;
  -- Pawl.Types.ObjectRef's AnyNumberMatching). The ObjectId is the effect's
  -- source; the [ObjectId] is the matching permanents, engine-pre-filtered
  -- against that ref's Filter. ChooseAnyNumberToSacrifice's shape and posture,
  -- a separate arm because that one is CR 614.1c's as-enters replacement and
  -- this one is a resolving effect's own choice.
  --
  -- NOT ChooseTargets: CR 115.1 makes a target only what the word "target"
  -- names, and conflating them would let shroud, hexproof and "becomes the
  -- target" triggers observe this choice (CR 115.10a).
  ChooseAnyNumberOfPermanents :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 608.2d: WHICH ONE of the permanents an effect offers it acts on, where
  -- the effect names exactly one (Hanweir Battlements' Garrison;
  -- Pawl.Types.ObjectRef.ChosenPermanent). The ObjectId is the effect's source;
  -- the NonEmpty is the matching permanents, engine-pre-filtered against that
  -- ref's Filter.
  --
  -- The arm above's singular, so this is asked only at TWO or more candidates.
  -- At one the answer is forced and the caller takes it without asking; at none
  -- the instruction is impossible, which CR 101.3 ignores and CR 609.3 leaves
  -- the rest of the effect to work around, so the caller never builds the
  -- NonEmpty.
  --
  -- NOT ChooseTargets: CR 115.1 makes a target only what the word "target"
  -- names, and conflating them would let shroud, hexproof and "becomes the
  -- target" triggers observe this choice (CR 115.10a).
  ChoosePermanent :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 702.122a: which permanents to TAP to pay a cost measured by their
  -- TOTAL POWER. The ObjectId is the Vehicle; the [ObjectId] is the payer's
  -- matching untapped permanents, engine-pre-filtered; the Natural is a
  -- THRESHOLD the chosen set's summed power must reach, not a size, and
  -- Pawl.Engine.Cost validates against a sum. A Set, for ChooseSacrifices'
  -- reason. Answers as Response.ChoseTaps.
  --
  -- Asked whenever the cost is paid, with no elision: whether the answer is
  -- forced is a question about SUBSETS -- crew 6 with a 6 and a 7 has two legal
  -- answers, with a 4 and a 3 one -- and deciding it means enumerating them.
  ChooseTapsForTotalPower :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | Which permanents to TAP to pay a cost that names HOW MANY of them (CR
  -- 601.2f's "tapping permanents"; Springleaf Drum). ChooseSacrifices' shape
  -- and elision -- asked only when there are more candidates than the count --
  -- over ChooseTapsForTotalPower's action, whose Natural is a threshold instead
  -- and so admits subsets of any size. Answers as Response.ChoseTaps, shared
  -- with that arm.
  ChooseTaps :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | Which permanents to RETURN to their owners\' hands to pay a cost that
  -- names how many of them (CR 118.1; Meloku the Clouded Mirror). ChooseTaps\'
  -- payload, posture and elision over a different action. Answers as
  -- Response.ChoseReturns, its own constructor rather than ChoseTaps: a
  -- transcript of one must not satisfy the other, or a replay would tap what it
  -- should have returned.
  ChooseReturns :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt (Set.Set ObjectId.ObjectId)
  -- | CR 701.3a: where an effect that moves an already-attached permanent puts
  -- it. The first ObjectId is the permanent being moved (Crown of the Ages'
  -- targeted Aura); the NonEmpty is the destinations its card text admits, the
  -- current host never among them (CR 701.3b).
  --
  -- Choose, not target: such an effect targets the Aura and not either
  -- creature. The offer is the card's TEXT and nothing more -- "another
  -- creature" gets every creature, including ones CR 303.4j will refuse to move
  -- the Aura onto, since narrowing further would answer that rule for the
  -- player; "another permanent it can enchant" gets only the legal ones
  -- (Filter.CanHostSubject). Elided at one candidate, the effect being
  -- mandatory.
  --
  -- The PlayerId is the RESOLVING controller for an effect that names one
  -- destination (CR 608.2d), and the SUBJECT's controller for one that names
  -- several -- CR 303.4d for an Aura, CR 301.5c for an Equipment.
  ChooseAttachment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ObjectId.ObjectId -> Prompt ObjectId.ObjectId
  -- | CR 303.4k with CR 614.1e: whether an Aura being turned face up exercises
  -- its printed "you MAY attach it" (Gift of Doom). CR 303.4k names the player.
  -- Its own prompt rather than a decline arm on ChooseAttachment, the "may"
  -- being the CARD's where WHICH host is rule 303.4k's own.
  --
  -- Never elided where it is asked at all: declining leaves the Aura
  -- unattached, and CR 704.5m then bins it.
  ChooseTurnUpAttachment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> Prompt OptionalDecision.OptionalDecision
  -- | CR 601.2b: the player announces their intention to pay alternative or
  -- additional costs, after the modes and before X and targets. The [Cost] is
  -- the PAYABLE candidates, pre-filtered through Cost.costsFor, total and
  -- canPay at CR 601.2b's X=0 floor. CR 118.9b makes an alternative cost
  -- optional, so a player who can afford both is genuinely choosing; asked only
  -- for two or more payable candidates.
  ChooseCost :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [Cost.Cost Keyword.Keyword] -> Prompt (Cost.Cost Keyword.Keyword)
  -- | CR 601.2h: the non-mana parts of a total cost are paid "in any order".
  -- The [CostComponent] is that cost's components in PRINTED order; the answer
  -- is a permutation of their indices, first-named paid first.
  --
  -- Asked once for the whole cost rather than once per part, losing nothing:
  -- each component's own choices are still asked when it is paid. Once per
  -- PASS: CR 601.2h pays the parts that move no card out of a library into a
  -- public zone first and the rest after, and each pass is ordered on its own
  -- (Pawl.Engine.Cost.paidInSecondPass). Asked only where the order is
  -- observable within a pass (Pawl.Engine.Cost.orderObservable).
  OrderCostComponents :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [CostComponent.CostComponent Keyword.Keyword] -> Prompt [Natural.Natural]
  -- | CR 508.1j / 509.1f: the costs of a combat toll are paid "in any order",
  -- and the order is the payer's. The [ObjectId] is the taxing permanents in
  -- the order the charges were gathered (Pawl.Engine.AttackCost.totalCost,
  -- Pawl.Engine.BlockCost.totalCost), one entry per charge; the answer is a
  -- permutation of their indices, first-named paid first.
  --
  -- The tags and not the components, unlike OrderCostComponents above: each
  -- charge is the price one permanent's text states. Asked only where the order
  -- is observable (Pawl.Engine.Cost.tollOrderObservable).
  OrderCombatTolls :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Prompt [Natural.Natural]
  -- | CR 712.21a: the owner arranges the two cards a melded permanent becomes
  -- when it is put into their graveyard or library. The Zone is that
  -- destination and the [PrintingId] the components in the order they melded
  -- in; the answer is a permutation of their indices, first-named PUT IN first
  -- -- so at a graveyard (CR 404.1) and at a library's top the last-named ends
  -- up on top. The printings and not the arrivals, which have no ids until this
  -- has settled where they go.
  --
  -- CR 730.3a says the sentence again for a merged permanent (#874), so the
  -- name is the leaving permanent's components rather than meld's.
  OrderComponentCards :: Decider.Decider -> PlayerId.PlayerId -> Zone.Zone -> [PrintingId.PrintingId] -> Prompt [Natural.Natural]
  -- | The relative order of a per-object batch over objects ONE player
  -- controls. The [Recipient] is one such group in the engine's canonical
  -- order; the answer is a permutation of its indices.
  --
  -- WHO is asked is the rule's, and the two rules that reach this disagree: CR
  -- 608.2f's secondary sentence gives it to the controller of the RESOLVING
  -- spell or ability, generally NOT the player whose permanents these are,
  -- which is the whole difference from OrderTriggers; CR 701.44d's simultaneous
  -- explores give it to that seat itself. The PlayerId field is whichever.
  --
  -- Asked once per GROUP rather than once per loop, the primary determination
  -- being APNAP (CR 101.4) and nobody's choice; the groups are asked in APNAP
  -- order, which CR 101.4c leaves open. Raised for two or more members, with no
  -- further elision: distinct objects are distinguishable by construction.
  OrderForEach :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> [Recipient.Recipient] -> Prompt [Natural.Natural]
  -- | CR 103.5: whether this player takes a mulligan. The MulliganOffer carries
  -- both halves of what a player at a table can see -- mulligans already taken,
  -- and how many cards another would bottom -- which diverge under CR 103.5c's
  -- free first mulligan. Asked in turn order, once per round, only while the
  -- hand is non-empty and only until the player keeps.
  DeclareMulligan :: Decider.Decider -> PlayerId.PlayerId -> MulliganOffer.MulliganOffer -> Prompt MulliganDecision.MulliganDecision
  -- | CR 103.5: after redrawing, put `count` cards from `hand` on the bottom of
  -- the library. The [ObjectId] is the redrawn hand; the answer is an ordered
  -- list of exactly `count` of those ids, first-listed ending up higher.
  -- Ordered and never a Set: bottom order IS future draw order, so it is a real
  -- choice even when the subset is forced. Asked only at two or more cards.
  Bottom :: Decider.Decider -> PlayerId.PlayerId -> [ObjectId.ObjectId] -> Natural.Natural -> Prompt [ObjectId.ObjectId]
  -- | CR 103.5b: an action a card lets a player take "any time they could
  -- mulligan". Each entry is the card and WHICH of its actions, since a face
  -- may print more than one; Nothing declines. Offered immediately BEFORE each
  -- DeclareMulligan and again after each action taken, in every round, and only
  -- to a player who has not yet kept. Performing the action is not taking a
  -- mulligan, so it feeds neither the bottom count nor CR 103.5c.
  MulliganAction :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, HandActionIndex.HandActionIndex)] -> Prompt (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  -- | CR 103.6 / 103.6a / 103.6b: an action an opening-hand card lets a player
  -- take once the mulligan process is complete. MulliganAction's entries,
  -- offered in turn order and again to the same player until they decline or
  -- nothing is left -- and a card acted on is not offered again, CR 103.6b
  -- capping this window where CR 103.5b caps nothing
  -- (Pawl.Types.HandWindowCap).
  --
  -- The candidate list is also narrowed by each action's OWN clause
  -- (Pawl.Types.HandAction.condition): Gemstone Caverns' "you're not the
  -- starting player" is answered before the offer.
  OpeningHandAction :: Decider.Decider -> PlayerId.PlayerId -> [(ObjectId.ObjectId, HandActionIndex.HandActionIndex)] -> Prompt (Maybe (ObjectId.ObjectId, HandActionIndex.HandActionIndex))
  -- | CR 603.5 / 608.2d: whether the player the PlayerId names exercises a
  -- printed "may". Usually the resolving controller, and not always: Jungle
  -- Wayfinder's "each player may search their library" asks the whole table,
  -- one question each, in CR 101.4's APNAP order and all before the clause's
  -- effects run (CR 608.2e). Pawl.Types.Optionality.Optional carries who.
  --
  -- The ObjectId is the object RESOLVING, not its source, since two triggers
  -- off one source resolve as two stack objects. The ModeIndex and ClauseIndex
  -- say which question this is: the "may" covers the CLAUSE it is printed on
  -- (CR 608.2e), not the whole mode. Resolution-time and not cast-time, CR
  -- 603.5 putting an optional ability on the stack regardless.
  --
  -- Never elided for what the clause's effects would ACCOMPLISH. The one
  -- exception is a clause whose every effect reads a slot and every slot it
  -- reads is illegal or unfilled (CR 608.2b): both answers are then the same
  -- board. Pawl.Engine.Resolve.clauseIsInert is the test, off the effects'
  -- CLASSIFICATION rather than their identity.
  ChooseOptional :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> Prompt OptionalDecision.OptionalDecision
  -- | CR 608.2d's "or": which of a resolving mode's mutually exclusive clauses
  -- happens, announced "while applying the effect" (Twiddle). The NonEmpty is
  -- the branches in printed order (CR 608.2c), and the answer is filtered back
  -- through it, so one naming no offered branch takes the first. The ObjectId
  -- is the object RESOLVING and the ModeIndex says which instance (CR 700.2d),
  -- both for ChooseOptional's reasons.
  --
  -- Asked ONCE per exclusive group, at whichever branch the resolution reaches
  -- first, and put to each player Pawl.Types.OrElse.chooser names, in CR 101.4's
  -- order: Twiddle's is the resolving controller alone, and Worms of the Earth's
  -- "any player may sacrifice two lands of their choice or have this enchantment
  -- deal 5 damage to that player" is the whole table, one announcement each.
  -- Asked BEFORE ChooseOptional, so a card printing one "may" over an either-or
  -- raises one "may" and not one per branch, and the "may" goes only to the
  -- seats that announced that branch. Never elided.
  --
  -- NOT ChooseModes. CR 700.2 makes a spell modal only for two or more options
  -- "in a bulleted list preceded by instructions for a player to choose", and
  -- CR 601.2b fixes those as the spell is cast; this is announced on
  -- resolution.
  ChooseClause :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> NonEmpty.NonEmpty ClauseIndex.ClauseIndex -> Prompt ClauseIndex.ClauseIndex
  -- | CR 608.2g: a cast a RESOLVING effect specifically allows (CR 310.12b is
  -- the producer). The ObjectId is the CARD being offered -- the exiled
  -- incarnation CR 400.7 minted, not the ability resolving -- and the CardName
  -- is the half CR 712.11a puts on the stack. No facing, an OfferCast opcode
  -- offering no face-down cast.
  --
  -- Raised only for CR 608.2g's "allows"; that rule's "instructs" (Wild
  -- Evocation) is not a decision and casts without asking. Never elided. Not
  -- offered when the card is no longer where the effect left it (CR 608.2h) or
  -- Cast.castableWhenOffered refuses the cast.
  OfferedCast :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> CardName.CardName -> Prompt OptionalDecision.OptionalDecision
  -- | CR 709.3 / 712.11b / 715.3: WHICH HALF of the multi-faced object an
  -- OfferedCast is casting. The NonEmpty names the halves already gated by
  -- Cast.castableWhenOffered, CR 709.3a and CR 712.11c both evaluating only the
  -- chosen half.
  --
  -- Asked BEFORE OfferedCast's "may", and asked under Optionality.Mandatory
  -- too: CR 118.8c's excuse is a property of the cost of the spell being cast,
  -- and which spell that is is what this settles. Raised only where two or more
  -- halves survive the gate.
  ChooseOfferedCastFace :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty CardName.CardName -> Prompt CardName.CardName
  -- | CR 702.94a / CR 121.9: whether this player reveals the card they are
  -- drawing as they draw it -- miracle's static half. The CardName rides along
  -- for OfferedCast's reason, and naming it in the question is what satisfies
  -- CR 121.9's look.
  --
  -- Distinct from OfferedCast, which is the LINKED ability's own "may" one step
  -- later: rule 702.94a gives the reveal to the static ability and the cast to
  -- the triggered ability the reveal put on the stack, so a player who reveals
  -- may still decline. Never elided; not asked when there is no miracle or the
  -- draw was not the turn's first.
  OfferedMiracleReveal :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> CardName.CardName -> Prompt OptionalDecision.OptionalDecision
  -- | CR 118.12 / 118.12a: whether this player pays a cost a RESOLVING spell or
  -- ability offers them (Mana Leak). The ObjectId is the object resolving, the
  -- ModeIndex/ClauseIndex pair is which clause of which chosen mode asks, and
  -- the Cost is what is offered, with CR 107.3a's X already resolved
  -- (Pawl.Engine.Resolve.announcedXOn). The "unless" scopes to its clause
  -- exactly as a "may" does (Stymied Hopes' scry).
  --
  -- The PlayerId is emphatically NOT the resolving controller: CR 118.12's
  -- clause names a player, and for this family that is the player the effects
  -- are aimed at. Still routed through Decide.deciderFor, so CR 723.1 control
  -- applies. CR 118.12a's rewriting is per player, so Rishadan Cutpurse raises
  -- this prompt once per opponent, in CR 101.4's APNAP order; one offer per
  -- PAYMENT and not per clause, a clause naming another's offer
  -- (Pawl.Types.PayGate.offeredAt) reusing the recorded answers.
  --
  -- Asked when the spell or ability RESOLVES, so a countered Mana Leak never
  -- asks. Never elided for a payable optional cost. Two cases leave nothing to
  -- ask: CR 118.3 leaves declining as the only answer, which CR 118.12's clause
  -- covers in as many words ("does, doesn't, or CAN'T"), and CR 118.12's
  -- MANDATORY limb (Standstill) reads whether the payer "started to pay" and
  -- gives them no choice (Pawl.Types.PayObligation). Pawl.ResolveSpec's PayGate
  -- group fails if this prompt is raised there.
  ChooseToPay :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ModeIndex.ModeIndex -> ClauseIndex.ClauseIndex -> Cost.Cost Keyword.Keyword -> Prompt PaymentDecision.PaymentDecision
  -- | CR 601.2b: whether 2 life or a coloured mana pays each Phyrexian symbol.
  -- CR 118.13a puts the choice here rather than at payment for a cast and an
  -- activation; CR 118.13b puts it immediately before a resolution-time cost
  -- is paid (Pawl.Engine.Resolve.payGatePaidBy) and CR 118.13c before a
  -- special action's; a combat toll asks at the same moment
  -- (Pawl.Engine.Cost.announceToll). The ManaSymbol is the
  -- symbol itself, so two symbols of different colours are distinguishable and
  -- CR 107.4f's hybrid Phyrexian symbol is sayable here at all.
  --
  -- BOTH of rule 107.4f's shapes come through this prompt, and the hybrid one
  -- asks a SECOND question afterwards: this constructor settles mana against
  -- life, and AnnounceHybridHalf then settles which colour.
  --
  -- One prompt per symbol, in printed order, and the NonEmpty is the routes
  -- payable given the announcements already made, measured at CR 601.2f's TOTAL
  -- so the promise survives a cost adjustment. Two symbols of the same colour
  -- ask identical questions, which is sound because both demand the same mana
  -- and the same 2 life. Elided when only one route is payable.
  AnnouncePhyrexianPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty PhyrexianPayment.PhyrexianPayment -> Prompt PhyrexianPayment.PhyrexianPayment
  -- | CR 601.2b: the nonhybrid equivalent cost for CR 107.4e's MONOCOLORED
  -- hybrid ({2/R}), whose two ways spend a different NUMBER of mana. All three
  -- of rule 118.13's moments put the choice here rather than at payment, and so
  -- does a combat toll (Pawl.Engine.Cost.announceToll). The ManaType is
  -- the symbol's own stated type, so a {2/R} and a {2/G} are distinguishable.
  -- The colour/colour hybrid is AnnounceHybridHalf, whose two ways are two mana
  -- types rather than a type against generic mana.
  --
  -- AnnouncePhyrexianPayment's per-symbol contract, elision and
  -- interchangeability argument throughout.
  AnnounceHybridPayment :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaType.ManaType -> NonEmpty.NonEmpty HybridPayment.HybridPayment -> Prompt HybridPayment.HybridPayment
  -- | CR 601.2b's same sentence for CR 107.4e's COLOUR/COLOUR hybrid ({G/U}):
  -- which of its two mana types the one mana will be. The ManaSymbol is the
  -- symbol itself, so a {G/U} and a {R/W} are distinguishable. Answered with a
  -- ManaType, so the answer IS the nonhybrid equivalent.
  --
  -- Both ways spend one mana, so unlike AnnounceHybridPayment this never
  -- changes CR 601.2f's total. What it changes is WHICH unit of an oversupplied
  -- pool is consumed, and so what floats (Pawl.ManaSpec's Gyre Engineer case).
  --
  -- AnnouncePhyrexianPayment's per-symbol contract and interchangeability
  -- argument. Elided when only one half is payable, and for the degenerate
  -- `Hybrid t t`.
  AnnounceHybridHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty ManaType.ManaType -> Prompt ManaType.ManaType
  -- | CR 118.7e: which half of a hybrid symbol a COST REDUCTION is applied as,
  -- chosen at CR 601.2f. A different question from AnnounceHybridPayment, which
  -- is about a symbol in the cost being PAID and constrains nothing here.
  --
  -- The ManaSymbol payload is the symbol being reduced BY; the NonEmpty is its
  -- two halves written as CR 118.7e's own outcomes -- an OfType, or a Generic
  -- for "an amount of generic mana equal to that half's number". Answering with
  -- the resulting SYMBOL is what lets both of CR 107.4e's shapes share one
  -- prompt. One prompt per symbol, in the order the reductions are read; two
  -- identical reductions are interchangeable.
  --
  -- NOT filtered by payability, unlike the two announcements above: CR 118.7e
  -- puts no such condition on the choice. Elided only for the degenerate
  -- `Hybrid t t`.
  ChooseReductionHalf :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> ManaSymbol.ManaSymbol -> NonEmpty.NonEmpty ManaSymbol.ManaSymbol -> Prompt ManaSymbol.ManaSymbol
  -- | CR 601.2f: "If multiple cost reductions apply, the player may apply them
  -- in any order." Asked of the payer as CR 601.2f's totalling reaches the
  -- reductions, once for the whole cost rather than once per reduction. The
  -- offer is the distinct TOTALS those orders reach, cheapest first, and the
  -- answer is the one the payer picked -- an order's only observable is the
  -- total it leaves (Pawl.Engine.Cost.applyAdjustments).
  --
  -- Raised only where two totals differ, which takes a FLOORED reduction beside
  -- an UNFLOORED one on one cost (Pawl.Types.CostAdjustments' pairing);
  -- reductions that state one floor commute, and every spell-cost reduction
  -- states none.
  --
  -- NOT filtered by payability, ChooseReductionHalf's posture and reason: CR
  -- 601.2f puts no such condition on the order, so the payer may take the
  -- costlier one and CR 601.2h reverses a payment they cannot make.
  ChooseReducedCost :: Decider.Decider -> PlayerId.PlayerId -> ObjectId.ObjectId -> NonEmpty.NonEmpty ManaCost.ManaCost -> Prompt ManaCost.ManaCost
