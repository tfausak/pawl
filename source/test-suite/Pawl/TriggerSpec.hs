-- Covers M4.5 P4 in full. The turn-scoped event log (Pawl.Types.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff -- `logSpec`. The CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanSpec`. The `Sacrifice` opcode and its
-- reserved trigger-source slot, CR 701.21 -- `sacrificeSpec`, and CR 603.10a's
-- second look-back family, a trigger on the sacrifice AS a sacrifice, with
-- Mayhem Devil -- `mayhemDevilSpec`. CR 603.6a's
-- OTHER written form, "whenever a [type] enters", with Soul Warden --
-- `permanentEntersSpec`. CR 603.8 state
-- triggers -- `stateTriggerSpec`, and CR 612.1's basic-land-type word swap
-- reaching one of those, with Magical Hack aimed at Barbarian Outcast --
-- `textChangedTriggerSpec`. CR 608.2i turn history (Khabál Ghoul's
-- "died this turn") -- `historySpec`. CR 603.7 delayed triggered abilities
-- -- `delayedSpec`, and the plural of its object binding -- a Create whose slot
-- holds every token it minted, so a card can say "those tokens", with Thatcher
-- Revolt -- `tokenSetSpec`, and that group read back through ObjectRef.InSlot by
-- a second opcode of the same resolution, with Salt Road Skirmish --
-- `tokenGroupReadSpec`, and that group's SINGULAR -- one token bound into the
-- target field and read back by a later effect of the same TRIGGERED ability,
-- with Harried Dronesmith -- `singleTokenSlotReadSpec`. The CR 603.3b ordering prompt -- `orderingSpec`, and its
-- CR 725.2 sourceless case (the monarch's inherent triggers ordered WITH the
-- batch) -- `monarchOrderingSpec`. The CR 603.4 / 608.2a intervening "if" --
-- `interveningSpec`. Also Pawl.Engine.Keyword: CR
-- 702.70 poisonous, the first keyword whose rule text IS a triggered ability,
-- and the
-- reserved "that player" slot the scan stamps for it -- `poisonousSpec`. CR
-- 702.115 ingest, the same event and the same slot over a payload that moves a
-- card out of a library nobody targeted, with Culling Drone at two seats and
-- three -- `ingestSpec`. CR
-- 702.86 annihilator, whose "defending player" (CR
-- 508.5) rides the declaration event through the same reserved slot, at three
-- seats so that player is not the attacker's only opponent --
-- `annihilatorSpec`. CR
-- 702.91 battle cry, and with it the CR 603.3b
-- ordering payload's ability discriminator -- two DISTINCT abilities of one
-- source, ordered both ways with different boards, and two triggers of the SAME
-- ability staying indistinguishable, with Hero of Bladehold -- `battleCrySpec`.
-- CR 702.108 prowess, the first such keyword whose event is not
-- its bearer's combat -- CR 601.2i's cast, watched through the same
-- TriggerCondition.SpellCast a card writes -- with Monastery Swiftspear --
-- `prowessSpec`. CR 509.3a's blocking-side declaration trigger, the mirror of
-- CR 508.3a's, with Pride Guardian -- `selfBlocksSpec`. CR 509.3c's attacking
-- side of that same declaration, with Sacred Prey -- `selfBecomesBlockedSpec`.
-- CR 702.45 bushido, the one such keyword to name both of those events, with
-- Inner-Chamber Guard --
-- `bushidoSpec`. CR 509.3d's once-per-blocker form of the same declaration,
-- which names the blocker, and CR 702.25 flanking, with
-- Benalish Cavalry -- `flankingSpec`. CR 702.130 afflict, which
-- puts CR 509.3c's event and CR 508.5's defending player in one sentence, with
-- Khenra Eternal at three seats -- `afflictSpec`. CR 702.83 exalted,
-- whose ability watches a permanent that is not its bearer and pumps a third
-- one, with Aven Squire -- `exaltedSpec`. CR 702.121 melee, the
-- first keyword whose payload is a number read off game state -- CR 508.3b's
-- record of who was declared attacked -- with Wings of the Guard at three seats
-- -- `meleeSpec`. CR 702.105 dethrone, the first whose whole content is its
-- CONDITION -- a comparison of every player's life total, not a fact about the
-- declaration -- with Enraged Revolutionary at three seats -- `dethroneSpec`.
-- CR 702.134 mentor, the first keyword whose
-- minted ability TARGETS -- a slot chosen under CR 603.3d and narrowed by a power
-- comparison against its own source -- with Blade Instructor -- `mentorSpec`.
-- CR 702.134c, the same rule's other half -- an ability that
-- watches a mentor ability resolve -- and the first condition read through its
-- bearer's ATTACHMENT rather than off the bearer or a Filter, with Aegis of the
-- Legion -- `mentorsTriggerSpec`.
-- CR 702.23 rampage, whose bonus multiplies a printed N by a number
-- read off the declaration, with Wolverine Pack and Horrible Hordes --
-- `rampageSpec`. CR 702.149 training, whose CONDITION reads the rest
-- of the declaration for a bigger companion, with Apprentice Sharpshooter --
-- `trainingSpec`. CR 702.39 provoke, the first whose payload
-- creates a CR 509.1c blocking requirement, with Goblin Grappler --
-- `provokeSpec`. CR 603.2's "that player" narrowing a TARGET SPEC rather than an
-- effect's operand -- Filter.ControlledByBound, baked to the player the event
-- named -- with Trygon Predator at three seats -- `trygonPredatorSpec`. CR
-- 702.112 renown, the first minted
-- ability with CR 603.4's intervening "if", with Rhox Maulers, plus CR 702.112b's
-- designation watched from outside, with Valeron Wardens -- `renownSpec`. CR
-- 701.37b's designation watched the same way -- the shared
-- TriggerCondition.PermanentBecomesDesignated with the other member of
-- Pawl.Types.Designation in it -- with Arbor Colossus, plus that designation read
-- back by CR 701.37a's own clause condition on a permanent Rune-Brand Juggler has
-- also suspected -- `arborColossusSpec`. CR
-- 702.63 vanishing, the first keyword whose rule text spans
-- BOTH mints -- one CR 614.1c entry replacement and two triggers, one of them
-- watching the counter removal the other performs -- with Waning Wurm --
-- `vanishingSpec`. CR 702.43 modular, whose rule text spans both mints too and
-- whose trigger PAYLOAD counts the dead permanent's own +1/+1 counters out of
-- CR 608.2h last known information, with Arcbound Hybrid and Arcbound Worker --
-- `modularSpec`. CR
-- 510.2's combat damage watched by a bystander rather than by the creature that
-- dealt it, with Tovolar, Dire Overlord -- `tovolarSpec`. The same condition's
-- damager slot, read by a payload that aims at it, with Aragorn, Hornburg Hero --
-- `aragornSpec`. CR 509.3b's blocking-side form
-- that names the ATTACKER, with Loyal Sentry -- `selfBlocksCreatureSpec`.
-- CR 509.3e's form that counts them, with Lairwatch Giant --
-- `selfBlocksAtLeastSpec`, and the same rule's FILTERED form on both sides of
-- the declaration at once, with Serra Inquisitors -- `selfBlocksOneOrMoreSpec`.
-- CR
-- 113.6k's non-battlefield scan -- the graveyard, with Tome Scour milling
-- Narcomoeba -- `graveyardTriggerSpec`, and CR 113.6m's reading of the same
-- zone off a triggered ability's EFFECT rather than its condition, with Squee,
-- Goblin Nabob against a Bitterblossom in the same graveyard --
-- `graveyardEffectZoneTriggerSpec`. CR 400.7e's OTHER reference inside a
-- look-back trigger, the card it became in the first zone it went to, with
-- Endless Cockroaches -- `becameSlotSpec`, which also pins
-- Event.eventBindingSlots (the per-condition slot set the card lint asks)
-- against the keys eventBindings actually stamps, over every event each
-- condition admits. The same slot under a BYSTANDER's dies trigger, where the
-- bearer is a third object entirely, with Promise of Tomorrow --
-- `promiseOfTomorrowSpec`. CR 603.4's intervening "if"
-- read against a source that no longer exists (CR 608.2h), with Deathknell Berserker
-- -- `lookBackInterveningSpec`, and the same clause asking about a COUNTER rather
-- than a characteristic -- the one thing CR 613 folds away before the snapshot
-- is taken -- with Promising Duskmage -- `counterLookBackSpec`. CR 702.93
-- undying and CR 702.79 persist, the pair of keywords that return their bearer
-- with a counter on it, with Young Wolf and Putrid Goblin -- `undyingSpec`.
-- CR 702.135 afterlife, the first minted keyword ability that CREATES a token,
-- with Ministrant of Obligation -- `afterlifeSpec`.
-- CR 702.123 fabricate, the first minted keyword ability whose resolution offers
-- a COST -- CR 118.12a's "unless", over a cost that puts counters -- with
-- Glint-Sleeve Artisan and Weaponcraft Enthusiast -- `fabricateSpec`.
-- CR 702.46 soulshift, the first minted keyword ability that TARGETS a card in a
-- graveyard, with Kami of Empty Graves -- `soulshiftSpec`.
-- CR 603.10's first sentence for a BYSTANDER -- a
-- permanent that was on the battlefield when some OTHER event in the same batch
-- happened and is gone by the CR 117.5 boundary -- with Lightning Skelemental
-- and Khabál Ghoul -- `bystanderSpec`, and CR 113.6m read off that same
-- bystander, so a graveyard-functioning trigger does not fire from the
-- battlefield it just left, with Squee, Goblin Nabob against a Bitterblossom
-- leaving beside it -- `bystanderZoneSpec`. The same CR 400.7e slot read from the ENTRY
-- direction, where the entrant is a different card from the bearer, with Aether
-- Flash -- `aetherFlashSpec`. CR 308's kindred card type, whose one observable
-- consequence (CR 308.2: a noncreature card carrying creature types) is read
-- through the ordinary Pool + Filter target machinery, with Bitterblossom --
-- `kindredSpec`. CR 701.9a's discard trigger, and CR 702.29d's
-- "only once when a card is cycled", with Megrim -- `discardTriggerSpec`.
-- CR 603.3a's controller read AT THE TRIGGER MOMENT rather than at the CR 117.5
-- scan, with a Megrim stolen until end of turn by Zealous Conscripts and handed
-- back by CR 514.2 before CR 514.3a places the trigger --
-- `controllerAtTriggerSpec`.
-- CR 701.6a's countering trigger, and the CR 113.6g gate that keeps it silent,
-- with Baral, Chief of Compliance -- `counterTriggerSpec`. CR 603.6c's OTHER
-- written form, "leaves the battlefield" for any destination, and the CR 400.7e
-- public-zone proviso a hidden destination makes real, with Thragtusk bounced by
-- Unsummon -- `leavesBattlefieldSpec`. That rule's SECOND written form read by a
-- BYSTANDER -- "whenever another creature you control dies", where the bearer
-- watches a permanent other than itself leave the battlefield -- with Meren of
-- Clan Nel Toth, which is also the pool's producer for CR 122.1's experience
-- counters -- `permanentDiesSpec`, and the pool's first READER of that same
-- counter kind, whose two triggers are CR 603.6a's entry form narrowed by a
-- power floor and a CR 603.2b step trigger whose Quantity counts a player's
-- counters, with Ezuri, Claw of Progress -- `ezuriExperienceSpec`. CR 119.9's life-gain trigger, from both
-- producers (CR 119.3's instructed gain and CR 120.3f's lifelink) with the
-- controls that keep it from being a "life total moved" trigger, with Ajani's
-- Pridemate -- `lifeGainTriggerSpec`. That event read for its NUMBER, which CR
-- 603.2 makes part of it and Sanguine Bond's "that much" is the pool's first
-- reader of -- `lifeGainAmountSpec`. That event's mirror, a player LOSING life,
-- from all three recording sites (CR 119.3's instructed loss, CR 119.2's damage
-- and CR 119.4's paid life) with the controls that keep it from being a "life
-- total moved" trigger, with Exquisite Blood -- `lifeLossTriggerSpec`. That
-- event read for BOTH the halves CR 603.2 makes part of it -- the amount and the
-- player who lost it -- on a three-seat board where "that player" and "an
-- opponent" come apart, with Mindcrank -- `mindcrankSpec`. CR 601.2i's cast
-- trigger again, this time with a payload that aims at a TARGET PLAYER and the
-- keyword half of the spell filter (CR 702.90 infect), which is the pool's first
-- poison counter given to someone chosen rather than derived, with Hand of the
-- Praetors -- `handOfThePraetorsSpec`. CR 725.1's crowning trigger, matched
-- against the EVENT so an entry's crown, a targeted crown and CR 725.2's stolen
-- crown all fire it alike, read through CR 109.5's "you" on a three-seat board
-- where "you" and "an opponent" come apart, with Custodi Lich --
-- `monarchTriggerSpec`.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.TriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Saga as Saga
-- Aliased Condition.Type, not Condition, per the project-wide convention
-- (CardSpec's note): the evaluator module Pawl.Engine.Condition may later be imported
-- and must not collide.

import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentDecision as PaymentDecision
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.TurnWindow as TurnWindow
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Barbarian Outcast on N Swamps, the two loaded printings each test case
-- supplies.
outcastBoard :: Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
outcastBoard barbarianOutcast swamp swamps =
  S.addCreature barbarianOutcast S.alice (S.landsInPlay swamp swamps)

-- alice casts Tidal Wave off three Islands and lets it resolve.
castWave :: Printing.Printing -> Printing.Printing -> GameState.GameState
castWave tidalWave island =
  let resolveAll g = snd (Engine.runGamePure S.identityAnswer g Engine.priorityLoop)
      (gs, oid) = S.handOne tidalWave (S.landsInPlay island 3)
   in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid)))

-- CR 608.2i: the log records; it is never emptied by a reader.
logSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
logSpec s registry =
  Spec.describe s "EventLog" $ do
    -- CR 400.7 / 603.2g: a zone change appends a Moved event carrying the
    -- RESOLVED destination. Pawl.EventSpec's "CR 603.2g: the emitted event
    -- records the RESOLVED destination (exile)" covers this same accessor
    -- (S.zoneChangesOf / ZoneChange.to) more strongly, through a Rest in
    -- Peace redirect -- no separate case needed here.
    -- CR 608.2h: the snapshot is the object as it last existed in the zone it
    -- LEFT. Re-deriving from the printed card would be wrong for an animated
    -- land and impossible for a token (CR 111.1).
    Spec.it s "CR 608.2h a Moved event snapshots the object it moved" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          expected = Projection.project piker gs
          after = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
      case S.eventsOf after of
        GameEvent.Moved _ snapshot : _ -> Spec.assertEqWith s "snapshot from the origin zone" snapshot expected
        _ -> Spec.assertFailure s "expected exactly one Moved event"
    -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
    -- bumping a watermark, and the record survives.
    Spec.it s "CR 704 the SBA check advances the damage watermark but keeps the record" $ do
      typhoidRats <- S.printingOf s registry "Typhoid Rats"
      ogreSentry <- S.printingOf s registry "Ogre Sentry"
      let (gs, _, _) = S.combatBoardOf [typhoidRats] [ogreSentry]
          fought = S.fightWith S.aggressiveAnswer gs
          after = S.settleSba fought
      Spec.assertEqWith s "nothing left unscanned for damage" (Event.unscannedDamage after) []
      Spec.assertBool s (not (null (S.damageEventsOf after))) "the damage events are still recorded"
    Spec.it s "CR 117.5 the trigger scan advances its watermark but keeps the record" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, gs) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (pikerId, gs1) = S.addCreature piker S.bob gs
          moved = S.runPure S.identityAnswer gs1 (Event.changeZone pikerId Zone.Hand)
          scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
      Spec.assertEqWith s "nothing left unscanned" (Event.unscannedEvents scanned) []
      Spec.assertBool s (not (null (S.zoneChangesOf scanned))) "the zone change is still recorded"
    -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
    -- wrong: cleanup is still part of THIS turn.
    Spec.it s "the log and both watermarks are cleared at turn handoff" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          moved = S.runPure S.identityAnswer gs (Event.changeZone piker Zone.Graveyard)
          after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
      Spec.assertEqWith s "log empty" (GameState.events after) Seq.empty
      Spec.assertEqWith s "scan watermark reset" (GameState.scannedThrough after) 0
      Spec.assertEqWith s "damage watermark reset" (GameState.damageScannedThrough after) 0
    -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
    -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
    Spec.it s "advance settles before handing off, so no unscanned event is discarded" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
          ending = gs1 {GameState.remaining = Seq.empty}
          after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
          isTrigger oid = case Game.lookupObject oid after of
            Just obj -> case Object.source obj of
              Source.OfTrigger _ _ -> True
              _ -> False
            Nothing -> False
      Spec.assertBool s (any isTrigger (GameState.stack after)) "the pending trigger reached the stack"
      Spec.assertEqWith s "the log was cleared afterwards" (GameState.events after) Seq.empty

-- CR 603.2b / 603.6a: a step begins, and EVERY permanent is checked.
scanSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
scanSpec s registry =
  Spec.describe s "Scan" $ do
    Spec.it s "CR 603.2b running a step records that it began, on the active player's turn" $ do
      let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.Ending EndingStep.EndStep, GameState.activePlayer = S.alice}
          after = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
          began ev = case ev of
            GameEvent.StepBegan p pid -> Just (p, pid)
            _ -> Nothing
      Spec.assertEqWith s "the end step's beginning is recorded exactly once" (Maybe.mapMaybe began (S.eventsOf after)) [(Phase.Ending EndingStep.EndStep, S.alice)]
    Spec.it s "CR 603.2b StepBegins matches its own step and no other" $ do
      let bearer = ObjectId.MkObjectId 1
          cond = TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)) "the end step matches"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice))) "the upkeep does not"
    -- CR 603.3a / 109.5: "your upkeep" is the ABILITY CONTROLLER's (603.3a
    -- controls the ability; 109.5 makes "your" mean that controller), so the
    -- scope is read against the bearer's controller, not the card.
    Spec.it s "CR 603.3a ControllersTurn matches only the bearer's controller's turn" $ do
      let bearer = ObjectId.MkObjectId 1
          cond = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.alice)) "alice's upkeep matches for alice"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice cond (GameEvent.StepBegan (Phase.Beginning BeginningStep.Upkeep) S.bob))) "bob's upkeep does not"
    -- The widening falsifier: the scan now visits every battlefield permanent,
    -- so SelfEnters must ask whether the event is about THIS permanent. Rest in
    -- Peace is on the battlefield and a DIFFERENT object entered.
    Spec.it s "CR 603.6a a SelfEnters trigger does not fire on another object's entry" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (pikerId, gs1) = S.addCreature piker S.bob gs0
          entered = ZoneChange.MkZoneChange pikerId pikerId Zone.Stack Zone.Battlefield
          gs2 = S.withEvents [GameEvent.Moved entered (Projection.project pikerId gs1)] gs1
      Spec.assertEqWith s "no trigger" (length (fst (Event.gatherTriggers (Event.unscannedGrouped gs2) gs2))) 0
    Spec.it s "CR 603.6a a SelfEnters trigger still fires on its own entry" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
      case fst (Event.gatherTriggers (Event.unscannedGrouped gs1) gs1) of
        [pt] -> do
          Spec.assertEqWith s "source is RiP" (PendingTrigger.source pt) (TriggerSource.OfObject ripId)
          Spec.assertEqWith s "controller is alice" (PendingTrigger.controller pt) S.alice
        other -> Spec.assertFailure s ("expected exactly one pending trigger, got " <> show (length other))
    Spec.it s "a graveyard-bound event yields no enters trigger" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          toGrave = ZoneChange.MkZoneChange ripId ripId Zone.Battlefield Zone.Graveyard
          gs1 = S.withEvents [GameEvent.Moved toGrave (Projection.project ripId gs0)] gs0
      Spec.assertEqWith s "no triggers" (length (fst (Event.gatherTriggers (Event.unscannedGrouped gs1) gs1))) 0
    Spec.it s "SelfEnters matches only a battlefield destination" $ do
      let bearer = ObjectId.MkObjectId 1
          movedTo zone = GameEvent.Moved (ZoneChange.MkZoneChange bearer bearer Zone.Stack zone) S.emptyCharacteristics
      Spec.assertBool s (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Battlefield)) "enters battlefield matches"
      Spec.assertBool s (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfEnters (movedTo Zone.Graveyard))) "enters graveyard does not"
    -- CR 508.3a plus Aurelia, the Warleader's "for the first time each turn".
    -- The declaration being matched is already in the log when the scan runs,
    -- so "the first time" is "this is the only one so far".
    Spec.it s "SelfAttacks FirstTimeEachTurn matches only the first declaration" $ do
      let bearer = ObjectId.MkObjectId 1
          declared = GameEvent.AttackerDeclared bearer S.bob 1
          gsWith events = S.withEvents events (Setup.emptyGame S.bothPlayers)
          matches frequency events =
            Event.matchesTrigger (gsWith events) bearer S.alice (TriggerCondition.SelfAttacks frequency) declared
      Spec.assertBool s (matches TriggerFrequency.FirstTimeEachTurn [declared]) "the first declaration matches"
      Spec.assertBool s (not (matches TriggerFrequency.FirstTimeEachTurn [declared, declared])) "a second declaration this turn does not"
      -- Hanweir Garrison's shape is untouched by the narrowing.
      Spec.assertBool s (matches TriggerFrequency.EveryTime [declared]) "EveryTime matches the first"
      Spec.assertBool s (matches TriggerFrequency.EveryTime [declared, declared]) "EveryTime matches the second too"
      -- The count is per bearer, not per turn: two creatures declared
      -- together are each attacking for the first time.
      Spec.assertBool s (matches TriggerFrequency.FirstTimeEachTurn [GameEvent.AttackerDeclared (ObjectId.MkObjectId 2) S.bob 1, declared]) "another creature's declaration does not spend this one's first time"
      -- CR 508.3a's last sentence, unchanged by the frequency: a
      -- non-declaration event never matches.
      Spec.assertBool s (not (Event.matchesTrigger (gsWith [declared]) bearer S.alice (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn) (GameEvent.StepBegan (Phase.Combat CombatStep.DeclareAttackers) S.alice))) "a step beginning is not an attack"
    -- Pins the canonical emission order this module's `eventTriggers` comment
    -- documents ("events outer, permanents inner, ascending by id"), which a
    -- later task's CR 603.3b ordering prompt indexes into. Two RiP bearers
    -- enter via two separate events recorded in the same order their ids
    -- were assigned; the resulting PendingTrigger.source list must follow
    -- that same ascending order.
    Spec.it s "CR 603.6a two SelfEnters triggers emit in ascending ObjectId order" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (rip1, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          (rip2, gs1) = S.addCreature restInPeace S.alice gs0
          entered1 = ZoneChange.MkZoneChange rip1 rip1 Zone.Stack Zone.Battlefield
          entered2 = ZoneChange.MkZoneChange rip2 rip2 Zone.Stack Zone.Battlefield
          gs2 =
            S.withEvents
              [ GameEvent.Moved entered1 (Projection.project rip1 gs1),
                GameEvent.Moved entered2 (Projection.project rip2 gs1)
              ]
              gs1
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped gs2) gs2)
      Spec.assertBool s (rip1 < rip2) "rip1 has the lower id"
      Spec.assertEqWith s "both triggers fired" (length triggers) 2
      Spec.assertEqWith s "sources in ascending ObjectId order" (fmap PendingTrigger.source triggers) (fmap TriggerSource.OfObject [rip1, rip2])
    -- The PERMANENTS-INNER half of that same order guarantee. Every SelfEnters
    -- test above has exactly one bearer matching each event, so inner order
    -- can never affect the output -- SelfEnters alone cannot discriminate
    -- events-outer-permanents-inner from any other traversal. A StepBegins
    -- bearer can: ONE StepBegan event matches MANY permanents at once. Two
    -- Khabál Ghouls (CR 603.2b, "at the beginning of each end step") on the
    -- battlefield, one end-step event -- the PendingTrigger.source list must
    -- come out in ascending ObjectId order.
    Spec.it s "CR 603.2b two StepBegins triggers from one event emit in ascending ObjectId order" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul1, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          (ghoul2, gs1) = S.addCreature khabalGhoul S.alice gs0
          event = GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice
          triggers = fst (Event.gatherTriggers [(EventGroup.first, event)] gs1)
      Spec.assertBool s (ghoul1 < ghoul2) "ghoul1 has the lower id"
      Spec.assertEqWith s "both triggers fired" (length triggers) 2
      Spec.assertEqWith s "sources in ascending ObjectId order" (fmap PendingTrigger.source triggers) (fmap TriggerSource.OfObject [ghoul1, ghoul2])
    -- CR 603.10, FIRST sentence -- the normal rule, not the "looks back in
    -- time" exception list that follows it: "objects that exist immediately
    -- after an event are checked to see if the event matched any trigger
    -- conditions". Ravenous Rats existed immediately after the event that put
    -- it onto the battlefield, so its CR 603.6a entry trigger fired -- even
    -- though CR 704.5f then buried it as a 0/0 before the CR 117.5 boundary's
    -- trigger scan ran.
    --
    -- bob holds TWO cards, so "discarded once" is distinguishable from
    -- "discarded twice"; the companion test below is the no-double-fire half.
    Spec.it s "CR 603.10 whole cards: under Night of Souls' Betrayal, Ravenous Rats dies as it enters and STILL makes bob discard" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      ravenousRats <- S.printingOf s registry "Ravenous Rats"
      night <- S.printingOf s registry "Night of Souls' Betrayal"
      let (_, base1) = S.addCreature night S.alice (S.landsInPlay swamp 2)
          (_, base2) = S.addHandCard piker S.bob base1
          (_, base3) = S.addHandCard piker S.bob base2
          (gs, spellId) = S.handOne ravenousRats base3
          bobBefore = S.handSize S.bob gs
          cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
          settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
      Spec.assertEqWith s "CR 704.5f buried the 0/0" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 0
      Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 1
      Spec.assertEqWith s "and bob still discarded, exactly once" (S.handSize S.bob settled) (bobBefore - 1)
    -- The no-double-fire half. Same board minus the -1/-1, so the Rats is on
    -- the battlefield at the CR 117.5 boundary AND named by an unscanned
    -- entry event -- the two candidate sources the scan draws from. A count,
    -- not a boolean: a Rats counted twice discards two of bob's two cards.
    Spec.it s "CR 603.6a whole cards: a Ravenous Rats that survives its entry triggers exactly once" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      ravenousRats <- S.printingOf s registry "Ravenous Rats"
      let (_, base1) = S.addHandCard piker S.bob (S.landsInPlay swamp 2)
          (_, base2) = S.addHandCard piker S.bob base1
          (gs, spellId) = S.handOne ravenousRats base2
          bobBefore = S.handSize S.bob gs
          cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
          settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
      Spec.assertEqWith s "the Rats survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Ravenous Rats") S.alice settled) 1
      Spec.assertEqWith s "nothing in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 0
      Spec.assertEqWith s "bob discarded exactly one" (S.handSize S.bob settled) (bobBefore - 1)

-- CR 701.21: sacrificing is its own keyword action -- NOT a destruction.
sacrificeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sacrificeSpec s registry =
  Spec.describe s "Sacrifice" $ do
    Spec.it s "CR 701.21a a sacrificed permanent goes to its owner's graveyard" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "off the battlefield" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "in bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
    -- The test above sacrifices a permanent the same player owns and
    -- controls, so it never exercises owner-relativity: CR 701.21a says
    -- "its CONTROLLER moves it... to its OWNER's graveyard." Here bob owns
    -- and alice controls (S.giveControl installs the layer-2 SetController
    -- effect), so the result must land in bob's graveyard, not alice's.
    Spec.it s "CR 701.21a a sacrifice lands in the OWNER's graveyard even when a different player controls it" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          gs = S.giveControl piker S.alice gs0
          -- ALICE is the sacrificing player, because she controls it (CR
          -- 701.21a); bob merely owns it, which is what the assertions below
          -- separate.
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
      Spec.assertEqWith s "off bob's battlefield" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "in bob's (owner's) graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 1
      Spec.assertEqWith s "not in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0
    -- CR 701.21a: "sacrificing a permanent doesn't destroy it", so neither CR
    -- 702.12b's indestructible gate nor CR 701.19a's shield applies.
    Spec.it s "CR 701.21a an indestructible permanent can still be sacrificed" $ do
      darksteelMyr <- S.printingOf s registry "Darksteel Myr"
      let (myr, gs) = S.addCreature darksteelMyr S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob myr)
      Spec.assertEqWith s "gone from the battlefield" (S.creaturesInPlay S.bob after) 0
    Spec.it s "CR 701.21a sacrificing neither consults nor consumes a regeneration shield" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs0) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          gs = S.addRegenShield piker gs0
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "still sacrificed" (S.creaturesInPlay S.bob after) 0
      Spec.assertEqWith s "the shield's source is untouched" (fmap ActiveReplacement.source (GameState.replacements after)) [piker]
    Spec.it s "only a battlefield permanent can be sacrificed (CR 701.21a)" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (card, gs) = S.addLibraryCard piker S.bob (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs (Event.sacrifice S.bob card)
      Spec.assertEqWith s "the library card is untouched" after gs
    -- CR 701.21a's second clause, which had no enforcement before #44: "A player
    -- can't sacrifice ... a permanent they don't control." Bob controls it;
    -- alice asking is refused outright rather than quietly honoured.
    Spec.it s "CR 701.21a a player cannot sacrifice a permanent they do not control" $ do
      pikerPrinting <- S.printingOf s registry "Goblin Piker"
      let (piker, gs) = S.addCreature pikerPrinting S.bob (Setup.emptyGame S.bothPlayers)
          byAlice = S.runPure S.identityAnswer gs (Event.sacrifice S.alice piker)
          byBob = S.runPure S.identityAnswer gs (Event.sacrifice S.bob piker)
      Spec.assertEqWith s "alice's attempt changes nothing at all" byAlice gs
      -- The discriminating half: the same call from the controller works, so the
      -- refusal above is the guard and not an unrelated no-op.
      Spec.assertEqWith s "bob's own sacrifice goes through" (S.creaturesInPlay S.bob byBob) 0
    -- CR 113.7: "this creature" is a slot read, filled at placement.
    Spec.it s "CR 113.7 a placed trigger binds its source into the reserved self slot" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
          placed = snd (Engine.runGamePure S.identityAnswer gs1 Engine.placePendingTriggers)
          bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid placed)
          selfOf oid = Map.lookup Binding.triggerSource (Binding.targetsOf (bindingsOn oid))
      Spec.assertEqWith s "the trigger names its source" (fmap selfOf (GameState.stack placed)) [Just (Set.singleton (Recipient.ToObject ripId))]

-- CR 603.10a's sacrifice family, end to end. Mayhem Devil {1}{B}{R} Creature --
-- Devil 3/3: "Whenever a player sacrifices a permanent, this creature deals 1
-- damage to any target."
--
-- THE PAIR is the point, and neither half means anything alone. CR 700.4 makes
-- "dies" mean "is put into a graveyard from the battlefield", so every sacrifice
-- IS a death and writes the same battlefield-to-graveyard Moved event a
-- destruction writes. An implementation that emitted the sacrifice event from the
-- generic zone-change funnel would pass the firing case and fail only the silent
-- one -- which is exactly the confusion #386 describes.
--
-- THREE SEATS. The Devil fires on ANY player's sacrifice, its own controller's
-- included, so alice has to be the sacrificing player -- a two-handed board where
-- bob sacrificed would leave a wrong "opponent only" reading passing. carol is
-- the third seat, and she absorbs the Fire-Eater's damage so that the Devil's 1
-- lands alone on bob: pointing the two sources at the SAME player would let a
-- single total agree for the wrong reason.
--
-- Ghitu Fire-Eater ({T}, Sacrifice this creature: it deals damage equal to its
-- power to any target) is the sacrificing card, chosen because its cost
-- sacrifices ITSELF -- so nothing is prompted about WHICH permanent, and the
-- fixture stays free of an answerer choice that could drift.
--
-- ONE TUPLE of all three totals, per assertion. bob's exact 19 is also the
-- no-double-fire pin: an event recorded both before and after the move, or a
-- condition that matched the Moved event alongside its own, would take him to 18.
mayhemDevilSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mayhemDevilSpec s registry =
  let -- Answers every target slot with `who`. Split across the two runs below:
      -- the Fire-Eater's own target is chosen as the ability is ACTIVATED, and
      -- the Devil's trigger's when the priority loop places it, so two answerers
      -- aim the two damage sources at two different players.
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      board devil extra =
        let (_, withDevil) = S.addCreature devil S.alice S.threePlayerGame
            (extraId, withExtra) = S.addCreature extra S.alice withDevil
         in ( extraId,
              withExtra
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "PermanentSacrificed" $ do
        Spec.it s "CR 603.10a Mayhem Devil fires on a sacrifice, including its own controller's" $ do
          mayhemDevil <- S.printingOf s registry "Mayhem Devil"
          ghituFireEater <- S.printingOf s registry "Ghitu Fire-Eater"
          case Face.activatedAbilities (S.combinedFace ghituFireEater) of
            [] -> Spec.assertFailure s "Ghitu Fire-Eater should declare one activated ability"
            ability : _ -> do
              let (fireEater, gs) = board mayhemDevil ghituFireEater
                  -- The Fire-Eater's own damage goes to carol; the cost paid here
                  -- is the CR 701.21a sacrifice under test.
                  activated = S.runPure (aimAt S.carol) gs (Activate.activateAbility S.alice fireEater ability)
                  -- The Devil's trigger is gathered and placed by the loop, so its
                  -- target is answered here -- bob, alone.
                  after = S.runPure (aimAt S.bob) activated Engine.priorityLoop
              Spec.assertEqWith s "everyone starts at 20" (lives gs) (Just 20, Just 20, Just 20)
              Spec.assertBool s (not (Set.member fireEater (GameState.battlefield activated))) "the cost really sacrificed the Fire-Eater"
              Spec.assertEqWith s "CR 701.21a: bob takes the Devil's 1, carol the Fire-Eater's 2" (lives after) (Just 20, Just 19, Just 18)
        -- The half that makes the half above mean something. Same board, same
        -- Devil, a permanent that DIES without being sacrificed: CR 700.4 makes
        -- the zone change identical, so only an engine that records the sacrifice
        -- as a sacrifice can stay silent here.
        Spec.it s "CR 700.4 a plain death is not a sacrifice, and fires nothing" $ do
          mayhemDevil <- S.printingOf s registry "Mayhem Devil"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = board mayhemDevil piker
              killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [pikerId])
              after = S.runPure (aimAt S.bob) killed Engine.priorityLoop
          -- Positive control: the Piker really left, so the silence below is the
          -- condition's answer rather than a fixture that destroyed nothing.
          Spec.assertEqWith s "the Piker really died" (S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Goblin Piker")) S.alice after) 0
          Spec.assertEqWith s "and it landed in a graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "CR 700.4: nobody's life total moved" (lives after) (Just 20, Just 20, Just 20)

-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2:
-- "When you control no Swamps, sacrifice this creature." CR 603.8's own example
-- shape ("a player controlling no permanents of a particular card type"), chosen
-- by the rulebook to illustrate the rule.
stateTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
stateTriggerSpec s registry =
  let triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ _ -> True
          _ -> False
        Nothing -> False
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
   in Spec.describe s "StateTrigger" $ do
        -- THE flooding falsifier. CR 603.8's second sentence exists to prevent
        -- exactly this: a state trigger that re-fires at every boundary.
        --
        -- What this test can and can't observe: it can only ever see 0 or 1
        -- here, never 2+. If suppression were absent, placePendingTriggers
        -- would keep reporting the same source as newly-triggered on every
        -- iteration, and Engine.settleForPriority (which loops until
        -- nothing new triggers) would recurse without terminating -- a
        -- broken suppression HANGS the test suite rather than failing this
        -- assertion with some higher count. This test still earns its keep
        -- by discriminating 0 (no trigger at all) from 1 (correctly armed
        -- once); it does not, and cannot, discriminate "one instance" from
        -- "flooding". The two-source test below is a separate discriminator,
        -- pinning that suppression is scoped per SOURCE rather than per
        -- ability (a bug that would wrongly suppress a second source, not
        -- flood -- so it fails this same 0/1 shape rather than hanging).
        Spec.it s "CR 603.8 a true state condition puts EXACTLY ONE instance on the stack" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
          Spec.assertEqWith s "one trigger, not one per boundary" (length (triggerIds settled)) 1
        Spec.it s "CR 603.8 re-settling while the instance is on the stack adds no second copy" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              twice = settle (settle gs)
          Spec.assertEqWith s "still exactly one" (length (triggerIds twice)) 1
        Spec.it s "CR 603.8 the condition being FALSE means no trigger at all" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 1
              settled = settle gs
          Spec.assertEqWith s "no trigger while a Swamp is out" (length (triggerIds settled)) 0
        Spec.it s "CR 603.8 losing the last Swamp makes the condition true and fires it" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 1
              quiet = settle gs
              swampOid = case Game.zoneMembers Zone.Battlefield S.alice quiet of
                ids -> case filter (\oid -> Set.member Subtype.Swamp (Projection.subtypesOf oid quiet)) ids of
                  swampId : _ -> swampId
                  [] -> ObjectId.MkObjectId 999
              gone = settle (S.runPure S.identityAnswer quiet (Event.destroy Regenerability.Regenerable [swampOid]))
          Spec.assertEqWith s "the Swamp's death arms it" (length (triggerIds gone)) 1
        -- CR 603.8: "doesn't trigger again until the ability has resolved, has
        -- been countered, or has otherwise left the stack" -- all three are
        -- "no longer on the stack", which is why armedness is derived.
        Spec.it s "CR 603.8 an instance leaving the stack re-arms the trigger" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
              removed = case triggerIds settled of
                abilId : _ -> Game.cease abilId settled
                [] -> settled
              again = settle removed
          Spec.assertEqWith s "a fresh instance" (length (triggerIds again)) 1
        -- IMPORTANT-2 (review): Event.stateTriggers' instancesOnStack count
        -- keys on BOTH the source object's id and the ability (`Source.OfTrigger
        -- srcId ab`). Every test above uses exactly one
        -- Barbarian Outcast, so all of them would still pass a weaker
        -- implementation that compared only the TriggeredAbility and ignored
        -- srcId -- and that weaker version would wrongly suppress a second,
        -- otherwise-independent source. This is the one that catches it: put
        -- one Outcast's instance on the stack first, THEN let a second,
        -- identical Outcast (same controller, same 0-Swamp board) get its own
        -- chance to trigger. Under a source-scoped comparison the second fires;
        -- under an ability-only comparison it is (wrongly) suppressed by the
        -- first's presence on the stack.
        Spec.it s "CR 603.8 a second identical source still triggers -- suppression is per-source, not per-ability" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs0) = outcastBoard barbarianOutcast swamp 0
              settledFirst = settle gs0
              (_, gs1) = S.addCreature barbarianOutcast S.alice settledFirst
              settledBoth = settle gs1
          Spec.assertEqWith s "two instances, one per source" (length (triggerIds settledBoth)) 2
        -- M-4 (review): the state trigger's Condition.holds reads the PROJECTION
        -- -- CR 613 layer 4 for a subtype -- not Face.typeLine. Pin it with no real Swamp card
        -- anywhere: alice controls only a Mountain, so the Outcast triggers;
        -- adding an AddLandSubtype Swamp modification (the Urborg shape) to
        -- that same Mountain must turn the trigger off.
        Spec.it s "CR 613 layer 4: an added Swamp subtype (no real Swamp card) suppresses the trigger" $ do
          mountain <- S.printingOf s registry "Mountain"
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = S.landsInPlay mountain 1
              mountainId = case Game.zoneMembers Zone.Battlefield S.alice gs0 of
                i : _ -> i
                [] -> ObjectId.MkObjectId 999
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              before = settle gs1
              withUrborg = S.withEffect mountainId (Modification.AddLandSubtype Subtype.Swamp) gs1
              after = settle withUrborg
          Spec.assertEqWith s "no real Swamp yet: triggers" (length (triggerIds before)) 1
          Spec.assertEqWith s "projected Swamp subtype (still a Mountain card): stops triggering" (length (triggerIds after)) 0
        -- M-4 (review): the state trigger's Condition.holds reads projected
        -- CONTROL -- CR 613 layer 2 -- not Object.owner. Pin it: bob owns and controls the only Swamp, so
        -- alice's Outcast triggers; giving alice control of bob's Swamp (a
        -- layer-2 SetController effect, S.giveControl) must turn it off even
        -- though bob still OWNS that Swamp.
        Spec.it s "CR 110.2/613 layer 2: gaining control of the opponent's Swamp suppresses the trigger" $ do
          swamp <- S.printingOf s registry "Swamp"
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.bothPlayers
              (swampId, gs1) = S.addCreature swamp S.bob gs0
              (_, gs2) = S.addCreature barbarianOutcast S.alice gs1
              before = settle gs2
              gs3 = S.giveControl swampId S.alice gs2
              after = settle gs3
          Spec.assertEqWith s "alice controls no Swamps yet: triggers" (length (triggerIds before)) 1
          Spec.assertEqWith s "alice now controls the Swamp: stops triggering" (length (triggerIds after)) 0
        -- The whole card, at gameplay level: the trigger resolves and the
        -- Outcast sacrifices itself (CR 701.21a, through Event.sacrifice).
        Spec.it s "CR 701.21 the resolved trigger sacrifices the Outcast into its owner's graveyard" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (outcast, gs) = outcastBoard barbarianOutcast swamp 0
              settled = settle gs
              resolved = snd (Engine.runGamePure S.identityAnswer settled Stack.resolveTop)
          Spec.assertBool s (not (Set.member outcast (GameState.battlefield resolved))) "the Outcast is off the battlefield"
          Spec.assertEqWith s "and in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1
        -- CR 603.2 / 603.8: ONE object carrying TWO identical state-triggered
        -- abilities. Each ability is its own ability, so each triggers, and CR
        -- 603.8's suppression ("a state-triggered ability doesn't trigger again
        -- until the ability has resolved...") is scoped to *the* ability -- it
        -- says nothing about a different ability that happens to read the same.
        --
        -- Synthetic Twofold Outcast (synthetic-twofold-outcast.json) is
        -- Barbarian Outcast with its one line printed twice and a life payment
        -- bolted on: "When you control no Swamps, you lose 1 life. Sacrifice
        -- this creature." x2. No printed card has two identical trigger lines
        -- (checked against Scryfall's whole oracle-cards bulk export, funny and
        -- alchemy sets included: zero hits), and nothing in CR 603 forbids one.
        --
        -- The life payment is what makes this test DISCRIMINATE. Counting stack
        -- objects alone would be weak; two identical abilities produce two
        -- indistinguishable trigger objects, so the effect COUNT is the real
        -- evidence. Alice is at 20; both instances resolving costs 2, one
        -- instance costs 1. The sacrifice is what makes it terminate: with the
        -- source off the battlefield the condition can never re-arm, so this is
        -- not the CR 603.8 loop a bare "lose 1 life" would be.
        Spec.it s "CR 603.2/603.8 two identical state triggers on ONE permanent each trigger" $ do
          twofoldOutcast <- S.printingOf s registry "Synthetic Twofold Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard twofoldOutcast swamp 0
              settled = settle gs
              resolveTop g = snd (Engine.runGamePure S.identityAnswer g Stack.resolveTop)
              both = resolveTop (resolveTop settled)
          Spec.assertEqWith s "two instances, one per ability" (length (triggerIds settled)) 2
          Spec.assertEqWith s "both resolved, so two life paid" (S.lifeOf S.alice both) (Just 18)
          Spec.assertEqWith s "and the stack is empty again" (length (triggerIds both)) 0
        -- CR 603.8's second half, on the same two-ability source: "A
        -- state-triggered ability doesn't trigger again until THE ABILITY has
        -- resolved, has been countered, or has otherwise left the stack." The
        -- rule is about the ability whose instance left -- an instance of the
        -- OTHER, identical ability is not that ability's instance and must not
        -- hold it back.
        --
        -- Game.cease is the "otherwise left the stack" path the sibling
        -- single-Outcast test above uses, and it is the only one that reaches
        -- this: resolving an instance sacrifices the source, which takes it off
        -- the battlefield and out of Event.stateTriggers' scan entirely.
        Spec.it s "CR 603.8 one instance leaving re-arms ITS ability, not held back by the twin's instance" $ do
          twofoldOutcast <- S.printingOf s registry "Synthetic Twofold Outcast"
          swamp <- S.printingOf s registry "Swamp"
          let (_, gs) = outcastBoard twofoldOutcast swamp 0
              settled = settle gs
              removed = case triggerIds settled of
                abilId : _ -> Game.cease abilId settled
                [] -> settled
              again = settle removed
          Spec.assertEqWith s "one of the two is gone" (length (triggerIds removed)) 1
          Spec.assertEqWith s "and the freed ability triggers again, alongside the survivor" (length (triggerIds again)) 2

-- Answers the Hack: it targets `oid` and swaps `from` for `to`. Everything else
-- falls through to the identity answer.
answerHackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
answerHackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  Prompt.ChooseLandTypeSwap {} -> (from, to)
  _ -> S.identityAnswer p

-- alice controls a Swamp and a Barbarian Outcast; BOB holds the Magical Hack and
-- the Island that pays for it. Whose Island it is decides the whole test: one on
-- ALICE's side would satisfy the swapped condition all by itself and the fired
-- trigger would prove nothing. bob casting it is legal for the same reason a
-- Hack can be aimed at any permanent -- CR 612.1's swap is a property of the
-- OBJECT, not of who changed its text.
--
-- With `hacked`, bob casts the Hack at the Outcast (Swamp -> Island) and it
-- resolves before the board is settled. Returns the Outcast's id and the state
-- at the CR 117.5 boundary, triggers placed and unresolved.
outcastHackBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (ObjectId.ObjectId, GameState.GameState)
outcastHackBoard s registry hacked = do
  barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
  swamp <- S.printingOf s registry "Swamp"
  island <- S.printingOf s registry "Island"
  magicalHack <- S.printingOf s registry "Magical Hack"
  let gs0 = Setup.emptyGame S.bothPlayers
      (_, gs1) = S.addCreature swamp S.alice gs0
      (_, gs2) = S.addCreature island S.bob gs1
      (outcastId, gs3) = S.addCreature barbarianOutcast S.alice gs2
      (hackId, gs4) = S.addHandCard magicalHack S.bob gs3
      gs5 = gs4 {GameState.priority = Just S.bob}
      hackIt g = S.runPure (answerHackAt outcastId Subtype.Swamp Subtype.Island) g (do S.cast S.bob hackId; Stack.resolveTop)
      settle g = snd (Engine.runGamePure S.identityAnswer g Engine.settleForPriority)
  pure (outcastId, settle (if hacked then hackIt gs5 else gs5))

-- CR 612.1 reaching a TRIGGERED ability, end to end through the real engine.
--
-- Barbarian Outcast {1}{R} Creature -- Human Barbarian Beast 2/2, "When you
-- control no Swamps, sacrifice this creature." (checked against Scryfall) and
-- Magical Hack are the whole board -- no card had to be added for this.
--
-- CR 612.1: a text-changing effect "can apply to any words or symbols printed on
-- that object, but generally affects only that object's rules text (which
-- appears in its text box)". A triggered ability is printed in that text box
-- exactly as an activated ability is, so a hacked Outcast asks about ISLANDS.
--
-- The reader this proves out is Pawl.Engine.Event.stateTriggers, which takes the
-- ability from Projection.triggeredAbilitiesOf (the projection's post-layer
-- list) and hands its CR 603.8 condition to Condition.holds -- so the swap has
-- to land in the projection, at CR 613.1c layer 3, to be seen here.
textChangedTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
textChangedTriggerSpec s registry =
  let isTriggerObject gs oid = case Game.lookupObject oid gs of
        Just obj -> case Object.source obj of
          Source.OfTrigger _ _ -> True
          _ -> False
        Nothing -> False
      triggerIds gs = filter (isTriggerObject gs) (GameState.stack gs)
      resolveTop gs = snd (Engine.runGamePure S.identityAnswer gs Stack.resolveTop)
   in Spec.describe s "TextChangedTriggeredAbility" $ do
        -- The control, and what keeps the case below from passing vacuously:
        -- unhacked, the printed word stands, alice's Swamp answers it, and the
        -- Outcast is never in any danger.
        Spec.it s "CR 603.8 whole card: an unhacked Outcast asks about SWAMPS and stays" $ do
          (outcastId, board) <- outcastHackBoard s registry False
          Spec.assertEqWith s "alice controls a Swamp: no trigger" (length (triggerIds board)) 0
          Spec.assertBool s (Set.member outcastId (GameState.battlefield (resolveTop board))) "the Outcast is still on the battlefield"
        -- The swap, at gameplay level. alice's board did not move -- one Swamp,
        -- no Islands -- but the Outcast's own text now reads "no Islands", which
        -- is true, so it fires and sacrifices itself.
        Spec.it s "CR 612.1 whole card: a hacked Outcast asks about ISLANDS, fires and sacrifices itself" $ do
          (outcastId, board) <- outcastHackBoard s registry True
          Spec.assertEqWith s "alice controls no Islands: it triggers" (length (triggerIds board)) 1
          let resolved = resolveTop board
          Spec.assertBool s (not (Set.member outcastId (GameState.battlefield resolved))) "the Outcast is off the battlefield"
          Spec.assertEqWith s "and in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 1

-- Khabál Ghoul {2}{B} Creature -- Zombie 1/1: "At the beginning of each end step,
-- put a +1/+1 counter on this creature for each creature that died this turn."
-- Scryfall's only ruling on the card is the design in one sentence: the count
-- "includes creature tokens ... as well as creatures put into a graveyard before
-- Khabál Ghoul entered the battlefield."
historySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
historySpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs =
        Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
   in Spec.describe s "TurnHistory" $ do
        -- The drained-queue falsifier: the deaths are SCANNED past before the end
        -- step's trigger ever exists, and must still be counted.
        Spec.it s "CR 608.2i deaths the trigger scan already passed are still counted" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              (p2, gs2) = S.addCreature piker S.bob gs1
              dead = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (Event.destroy Regenerability.Regenerable [p1])) (Event.destroy Regenerability.Regenerable [p2])
              scanned = settle dead
              atEnd = resolveAll (settle (beginEndStep scanned))
          Spec.assertEqWith s "two +1/+1 counters" (countersOn ghoul atEnd) 2
          Spec.assertEqWith s "a 3/3" (Projection.powerOf ghoul atEnd) (Just 3)
        -- CR 111.1 / 608.2h: a token has NO printed card, so an implementation
        -- that re-derived card types from print instead of from the event's
        -- snapshot would read zero here.
        Spec.it s "CR 111.1 a token creature that died counts, though it has no printed card" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (tok, gs1) = S.addToken (Printing.card piker) S.bob gs0
              dead = S.settleSba (S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [tok]))
              atEnd = resolveAll (settle (beginEndStep dead))
          Spec.assertEqWith s "the token is counted" (countersOn ghoul atEnd) 1
        Spec.it s "a creature that left the battlefield for HAND did not die (CR 700.4)" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              bounced = S.runPure S.identityAnswer gs1 (Event.changeZone p1 Zone.Hand)
              atEnd = resolveAll (settle (beginEndStep bounced))
          Spec.assertEqWith s "a bounce is not a death" (countersOn ghoul atEnd) 0
        -- CR 608.2i: "look back in time" effects don't require the counted
        -- objects to currently exist, or the counting object to have existed
        -- at the time. Scryfall's ruling says this explicitly: the count
        -- "includes ... creatures put into a graveyard before Khabál Ghoul
        -- entered the battlefield." This test cannot fail against today's
        -- `Pawl.Engine.Quantity.countOf`, which takes no `ObjectId` at all and so
        -- has no way to scope the fold to the Ghoul's own lifetime -- it is
        -- a regression gate on the ruling, pinned ahead of that signature
        -- ever gaining one.
        Spec.it s "CR 608.2i a creature that died before the Ghoul entered is still counted" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (p1, gs0) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
              dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [p1])
              (ghoul, gs1) = S.addCreature khabalGhoul S.alice (settle dead)
              atEnd = resolveAll (settle (beginEndStep gs1))
          Spec.assertEqWith s "one +1/+1 counter" (countersOn ghoul atEnd) 1
        -- CR 608.2i: the history's scope is ONE turn.
        Spec.it s "the count resets at turn handoff, not at the trigger scan" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              (p1, gs1) = S.addCreature piker S.bob gs0
              dead = S.runPure S.identityAnswer gs1 (Event.destroy Regenerability.Regenerable [p1])
              nextTurn = snd (Engine.runGamePure S.identityAnswer dead Engine.handoffTurn)
              atEnd = resolveAll (settle (beginEndStep nextTurn))
          Spec.assertEqWith s "last turn's death does not count" (countersOn ghoul atEnd) 0
        -- CR 603.2b: the step trigger belongs to a permanent with nothing to do
        -- with the event -- Task 2's widened scan, at gameplay level.
        Spec.it s "CR 603.2b the end step's beginning is what fires it" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              quiet = settle gs0
              fired = settle (beginEndStep quiet)
              isTrigger oid = case Game.lookupObject oid fired of
                Just obj -> case Object.source obj of
                  Source.OfTrigger _ _ -> True
                  _ -> False
                Nothing -> False
          Spec.assertEqWith s "nothing before the step began" (GameState.stack quiet) []
          Spec.assertEqWith s "one trigger once it did" (length (filter isTrigger (GameState.stack fired))) 1

-- Tidal Wave {2}{U} Instant: "Create a 5/5 blue Wall creature token with defender.
-- Sacrifice it at the beginning of the next end step." CR 603.7c's object-bound
-- delayed ability -- "it" must survive the resolution that armed it. Synthetic
-- Deferred Rally joins it for the one shape Tidal Wave cannot reach: a delayed
-- ability with an intervening "if".
delayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
delayedSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      walls gs = filter (\oid -> Set.member Subtype.Wall (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Answers Prompt.ChooseBoundToken with the LAST token minted, recording
      -- every candidate list so a test can assert whether the prompt was issued
      -- at all. Naming the last is what makes the assertion discriminating:
      -- binding the FIRST is exactly what the engine used to do silently.
      chooseLastToken :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      chooseLastToken p = case p of
        Prompt.ChooseBoundToken _ _ _ candidates -> do
          State.modify' (<> [NonEmpty.toList candidates])
          pure (NonEmpty.last candidates)
        _ -> pure (S.identityAnswer p)
      -- Answers Prompt.ChooseBoundToken with an object that was never minted, so
      -- the engine's filter is what decides the binding. Id 999 names nothing --
      -- the same posture S.noSource takes.
      -- Stamp an expiry onto every armed delayed ability, so the CR 603.7b
      -- stated-duration mechanism can be exercised on a real armed entry.
      withExpiry expiry gs =
        gs
          { GameState.delayedTriggers =
              fmap (\entry -> entry {DelayedTrigger.expiry = expiry}) (GameState.delayedTriggers gs)
          }
      chooseUnmintedToken :: Prompt.Prompt r -> r
      chooseUnmintedToken p = case p of
        Prompt.ChooseBoundToken {} -> ObjectId.MkObjectId 999
        _ -> S.identityAnswer p
      -- alice casts the spell in hand and resolves it under chooseLastToken,
      -- handing back the board alongside the candidate lists it was asked about.
      castUnderChoice gs oid =
        State.runState
          ( Engine.runGame chooseLastToken gs $ do
              S.cast S.alice oid
              Engine.priorityLoop
          )
          []
   in Spec.describe s "DelayedTrigger" $ do
        Spec.it s "CR 111.3 the spell mints a 5/5 Wall with defender and arms one delayed ability" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let after = castWave tidalWave island
          case walls after of
            [wall] -> do
              Spec.assertEqWith s "5 power" (Projection.powerOf wall after) (Just 5)
              Spec.assertEqWith s "5 toughness" (Projection.toughnessOf wall after) (Just 5)
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Defender wall after) "defender"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers after)) 1
            other -> Spec.assertFailure s ("expected exactly one Wall token, got " <> show (length other))
        -- CR 603.7b: "only once, the next time its trigger event occurs".
        Spec.it s "CR 603.7 the token is sacrificed at the beginning of the next end step" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let after = resolveAll (settle (beginEndStep (castWave tidalWave island)))
          Spec.assertEqWith s "no Wall left" (walls after) []
          Spec.assertEqWith s "the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
        -- CR 603.7b's other half: "unless it has a stated duration, such as
        -- 'this turn.'" Tidal Wave's entry is reused with an expiry stamped on
        -- it, so the mechanism is tested without inventing a card; Full
        -- Throttle is the card that actually prints one.
        Spec.it s "CR 603.7b a stated-duration delayed ability stays armed after firing" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              stated = withExpiry (Just Expiry.Type.AtCleanup) armed
              began = [GameEvent.StepBegan endStep S.alice]
              (firedOnce, survivors) = Event.delayedPending began stated
              (firedAgain, _) = Event.delayedPending began stated {GameState.delayedTriggers = survivors}
          Spec.assertEqWith s "it fired" (length firedOnce) 1
          Spec.assertEqWith s "and stayed armed" (Seq.length survivors) 1
          Spec.assertEqWith s "so the next end step fires it again" (length firedAgain) 1
        Spec.it s "CR 603.7b without a stated duration firing still spends it" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              (fired, survivors) = Event.delayedPending [GameEvent.StepBegan endStep S.alice] armed
          Spec.assertEqWith s "it fired" (length fired) 1
          Spec.assertEqWith s "and was evicted" (Seq.length survivors) 0
        -- Synthetic Deferred Rally {W} Instant: "At the beginning of the next
        -- end step, if you control a creature, you gain 2 life." A LABELED
        -- CRUTCH (#851): every printed delayed ability with an intervening "if"
        -- asks whether a named card was cast, played or is still in some zone,
        -- and neither Quantity nor Filter can name a card. CR 603.4 and
        -- CR 603.7b meeting on one ability: the first end step's event matches,
        -- but the intervening "if" is false, so the ability does not TRIGGER --
        -- and CR 603.7b bounds how many times it triggers, not how many events
        -- it watches, so nothing was spent and it is still waiting for the next
        -- end step.
        --
        -- The STORE is where the difference shows, and the two assertions on it
        -- are the discriminating pair: still armed after the false occurrence,
        -- spent after the true one. The life total alone would not discriminate
        -- in the first half -- an entry that wrongly triggered at the first end
        -- step still gains nothing, because Pawl.Engine.Stack's CR 608.2a
        -- re-check removes it from the stack for the same false condition, so
        -- the one shot would be spent invisibly.
        Spec.it s "CR 603.4 a false intervening \"if\" leaves the delayed ability armed for the next end step" $ do
          rally <- S.printingOf s registry "Synthetic Deferred Rally"
          plains <- S.printingOf s registry "Plains"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, oid) = S.handOne rally (S.landsInPlay plains 1)
              armed = resolveAll (snd (Engine.runGamePure S.identityAnswer gs0 (S.cast S.alice oid)))
              firstEnd = resolveAll (settle (beginEndStep armed))
              withCreature = snd (S.addCreature piker S.alice firstEnd)
              secondEnd = resolveAll (settle (beginEndStep withCreature))
          Spec.assertEqWith s "the resolution armed it" (Seq.length (GameState.delayedTriggers armed)) 1
          Spec.assertEqWith s "no life gained while the condition is false" (S.lifeOf S.alice firstEnd) (Just 20)
          Spec.assertEqWith s "and the entry is still armed" (Seq.length (GameState.delayedTriggers firstEnd)) 1
          Spec.assertEqWith s "the next end step fires it" (S.lifeOf S.alice secondEnd) (Just 22)
          Spec.assertEqWith s "and that spends it" (Seq.length (GameState.delayedTriggers secondEnd)) 0
        -- CR 514.2: "all 'until end of turn' and 'this turn' effects end"
        -- during the cleanup step -- which is what ends the stated duration,
        -- and the reason an armed entry cannot outlive the turn that made it.
        Spec.it s "CR 514.2 cleanup drops a stated-duration delayed ability" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              swept expiry = GameState.delayedTriggers (Expiry.dropAtCleanup (withExpiry expiry armed))
          Spec.assertEqWith s "an 'this turn' entry is gone" (Seq.length (swept (Just Expiry.Type.AtCleanup))) 0
          Spec.assertEqWith s "an end-of-game entry stays" (Seq.length (swept (Just Expiry.Type.Never))) 1
          -- CR 603.7b's one shot is spent by FIRING, not by time, so an entry
          -- on no duration at all must survive every sweep.
          Spec.assertEqWith s "and a one-shot entry stays" (Seq.length (swept Nothing)) 1
        Spec.it s "CR 603.7b a second end step does not re-fire it" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let once = resolveAll (settle (beginEndStep (castWave tidalWave island)))
              again = settle (beginEndStep once)
          Spec.assertEqWith s "nothing on the stack" (GameState.stack again) []
        -- CR 603.7a: a delayed ability does not trigger on an event that
        -- happened BEFORE it was created. Falls out of the watermark for free.
        Spec.it s "CR 603.7a armed during an end step, it waits for the NEXT one" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (gs0, oid) = S.handOne tidalWave (S.landsInPlay island 3)
              inEndStep = settle (beginEndStep gs0)
              cast = resolveAll (snd (Engine.runGamePure S.identityAnswer inEndStep (S.cast S.alice oid)))
              sameStep = settle cast
              nextStep = resolveAll (settle (beginEndStep sameStep))
          Spec.assertEqWith s "still alive during the step it was armed in" (length (walls sameStep)) 1
          Spec.assertEqWith s "sacrificed at the next end step" (walls nextStep) []
        -- CR 603.7c: the ability still triggers and is still consumed even when
        -- the object it remembers is gone.
        Spec.it s "CR 603.7c with the token already gone the ability does nothing and is consumed" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let armed = castWave tidalWave island
              killed = case walls armed of
                wall : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [wall]))
                [] -> armed
              after = resolveAll (settle (beginEndStep killed))
          Spec.assertEqWith s "no Wall" (walls after) []
          Spec.assertEqWith s "the store is still emptied" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "nothing stuck on the stack" (GameState.stack after) []
        -- IMPORTANT-1 (fix pass 1): Engine.placeOne merges a delayed ability's
        -- OWN placement-time bindings (its chosen modes/targets, chosen just now)
        -- with the environment CAPTURED when the ability was armed, under
        -- Map.union -- left-biased, so the argument ORDER decides which side
        -- wins a collision on a reserved slot such as Binding.chosenModes. The
        -- two DO collide in practice: Pawl.Engine.Cast builds an arming spell's
        -- bindings through the same Binding.fromChoices that stamps chosenModes
        -- whenever the spell chooses a mode, so a modal arming spell's captured
        -- environment carries a "modes" entry that belongs to the SPELL, not to
        -- the delayed ability being placed.
        --
        -- Tidal Wave cannot exercise this: both it and its one delayed ability
        -- have exactly one mode, so both always choose mode 0 and the two
        -- possible union orders are indistinguishable through it (the earlier
        -- Tidal Wave tests above pass under EITHER order). This test instead
        -- calls Engine.placeOne directly with a hand-built PendingTrigger whose
        -- CAPTURED bindings carry a chosenModes entry for a mode index (7) the
        -- ability being placed does not even have -- standing in for "whatever
        -- mode a modal arming spell happened to choose". The ability itself has
        -- one legal mode (index 0, forced/unprompted), so a correct placement
        -- can only ever stamp {0} for ITS OWN choice. Under the pre-fix
        -- direction (`Map.union captured placementTime`) the captured {7} would
        -- win instead, and this assertion would fail.
        Spec.it s "CR 603.7c placement-time's own chosen mode wins a collision with the captured environment" $ do
          let onlyMode = Mode.MkMode {Mode.clauses = Seq.empty, Mode.targetSpecs = Map.empty}
              ability =
                TriggeredAbility.MkTriggeredAbility
                  { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                    TriggeredAbility.modal = Modal.MkModal {Modal.modes = Seq.singleton onlyMode, Modal.selection = ModeSelection.ChooseExactly 1},
                    TriggeredAbility.intervening = Nothing
                  }
              -- Stands in for a modal arming spell's own captured chosenModes --
              -- built with the SAME Binding.fromChoices Cast.castSpell uses, so
              -- the collision is the real production shape, not a fabricated one.
              captured = Binding.fromChoices Map.empty Nothing (Seq.singleton (ModeIndex.MkModeIndex 7))
              pending = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject (ObjectId.MkObjectId 0)) S.alice ability captured
              after = snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Engine.placeOne pending))
              placedModes = case GameState.stack after of
                placedId : _ -> case Game.lookupObject placedId after of
                  Just obj -> Binding.modesOf (Object.bindings obj)
                  Nothing -> Seq.empty
                [] -> Seq.empty
          Spec.assertEqWith s "the ability's own mode (0), not the captured spell's mode (7)" placedModes (Seq.singleton (ModeIndex.MkModeIndex 0))
        -- CR 800.4d: "If a triggered ability that would be controlled by a
        -- player who has left the game would be put onto the stack, it isn't
        -- put on the stack." CR 800.4d's own example is a delayed ability, and
        -- so is this: Tidal Wave's "Sacrifice it at the beginning of the next
        -- end step", armed by bob, whose controller is baked in at arming
        -- (CR 603.7d) and so survives him. CR 603.7b still spends its one shot:
        -- the example's Hypnotic Specter "never returns to the battlefield."
        --
        -- Three seats, because at two the departure ends the game before an end
        -- step can arrive.
        Spec.it s "CR 800.4d a departed player's delayed ability triggers, is consumed, and is not put on the stack" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (_, l1) = S.addCreature island S.bob S.threePlayerGame
              (_, l2) = S.addCreature island S.bob l1
              (_, l3) = S.addCreature island S.bob l2
              (waveId, l4) = S.addHandCard tidalWave S.bob l3
              ready = l4 {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
              cast = S.runPure S.identityAnswer ready (S.cast S.bob waveId)
              armed = S.runPure S.identityAnswer cast Engine.priorityLoop
              gone = Departure.depart Departure.Type.Conceded S.bob armed
              began = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gone {GameState.phase = endStep})
              (placedAny, placed) = S.runPureWith S.identityAnswer began Engine.placePendingTriggers
              (controlAny, control) = S.runPureWith S.identityAnswer (Event.recordEvent (GameEvent.StepBegan endStep S.alice) (armed {GameState.phase = endStep})) Engine.placePendingTriggers
          Spec.assertEqWith s "the fixture really armed one delayed ability" (Seq.length (GameState.delayedTriggers armed)) 1
          Spec.assertEqWith s "bob's ability is not put on the stack" (GameState.stack placed) []
          Spec.assertEqWith s "CR 603.7b: it still triggered, so its one shot is spent" (Seq.length (GameState.delayedTriggers placed)) 0
          Spec.assertEqWith s "with bob still in the game the SAME ability IS placed -- the filter is what did it" (length (GameState.stack control)) 1
          Spec.assertEqWith s "nothing reached the stack, so placePendingTriggers honestly reports it placed nothing" placedAny False
          Spec.assertEqWith s "with bob still in the game, something genuinely got placed" controlAny True
        -- CR 614.16 meets CR 603.7c. Doubling Season ("If an effect would
        -- create one or more tokens under your control, it creates twice that
        -- many of those tokens instead") scales Tidal Wave's Create at
        -- RESOLUTION, so two Walls are minted where CR 603.7c's "it" names one
        -- particular object. CR 707.10e is the codified analogue and settles
        -- that the leftover is a CHOICE, not something the engine may decide:
        -- where a replacement causes a copy to target more than one object,
        -- "the copy's controller chooses one of them to be the new target",
        -- and its Frontline Heroism / Anointed Procession example is this exact
        -- shape -- two tokens created, "the copy targets one of those tokens of
        -- your choice."
        --
        -- Discriminating: the answerer names the LAST minted token, and the
        -- unfixed engine bound the first, so the wrong Wall would be the one
        -- that died and the surviving assertion would fail too.
        Spec.it s "CR 614.16/603.7c a doubled Create asks which minted token \"it\" names" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (gs, waveId) = S.handOne tidalWave base
              ((_, armed), asked) = castUnderChoice gs waveId
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "the replacement really doubled the Create" (length (walls armed)) 2
          case asked of
            [[unchosen, chosen]] -> do
              Spec.assertEqWith s "the token named by \"it\" was sacrificed, and only it" (walls after) [unchosen]
              Spec.assertBool s (Set.notMember chosen (GameState.battlefield after)) "the chosen Wall is off the battlefield"
            _ -> Spec.assertFailure s ("expected one prompt offering two tokens, got " <> show asked)
        -- The companion, and the elision this pairs with: without the
        -- replacement the Create mints exactly one token, so "it" has only one
        -- possible referent and there is nothing to ask. Where the rules leave
        -- nothing to ask, don't prompt.
        Spec.it s "CR 603.7c one minted token is no choice, so nothing is asked" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          let (gs, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
              ((_, armed), asked) = castUnderChoice gs waveId
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "one Wall minted" (length (walls armed)) 1
          Spec.assertEqWith s "no binding prompt was issued" asked []
          Spec.assertEqWith s "and it is still sacrificed at the end step" (walls after) []
        -- FILTERED, NOT TRUSTED, the posture Sba.chooseLegendVictims takes for
        -- CR 704.5j: an answer naming something that was never minted would
        -- otherwise leave CR 603.7c's "it" pointing at nothing and the delayed
        -- ability would sacrifice neither Wall. The slot is bound either way,
        -- deterministically to the first token.
        Spec.it s "CR 603.7c an answer naming an unminted object falls back to the first token" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (gs, waveId) = S.handOne tidalWave base
              cast = S.runPure chooseUnmintedToken gs (S.cast S.alice waveId)
              armed = S.runPure chooseUnmintedToken cast Engine.priorityLoop
              after = resolveAll (settle (beginEndStep armed))
          case walls armed of
            [firstWall, secondWall] -> do
              Spec.assertEqWith s "only the second minted Wall is left" (walls after) [secondWall]
              Spec.assertBool s (Set.notMember firstWall (GameState.battlefield after)) "the first minted Wall was bound, and it is gone"
            other -> Spec.assertFailure s ("expected two Wall tokens, got " <> show (length other))

-- Thatcher Revolt {2}{R} Sorcery: "Create three 1/1 red Human creature tokens
-- with haste. Sacrifice those tokens at the beginning of the next end step."
-- delayedSpec's plural sibling: the card refers back to EVERY token it made at
-- once, so the Create's slot holds a GROUP rather than one object, and the
-- delayed ability sacrifices all of it.
tokenSetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenSetSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      humans gs = filter (\oid -> Set.member Subtype.Human (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Records every Prompt.ChooseBoundToken the resolution issues, so a test
      -- can assert the plural binding asks NOTHING -- "them" names every minted
      -- token, and where the rules leave nothing to ask, don't prompt.
      recordPrompts :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      recordPrompts p = case p of
        Prompt.ChooseBoundToken _ _ _ candidates -> do
          State.modify' (<> [NonEmpty.toList candidates])
          pure (NonEmpty.head candidates)
        _ -> pure (S.identityAnswer p)
      castUnderPrompts gs oid =
        State.runState
          ( Engine.runGame recordPrompts gs $ do
              S.cast S.alice oid
              Engine.priorityLoop
          )
          []
      -- alice casts Thatcher Revolt from hand onto the given board and lets it
      -- resolve, handing back the board alongside any prompts it issued.
      castRevolt revolt base = uncurry castUnderPrompts (S.handOne revolt base)
      boardOf mountain = S.landsInPlay mountain 3
   in Spec.describe s "DelayedTrigger token set" $ do
        Spec.it s "CR 111.3 the spell mints three 1/1 Humans with haste and arms one delayed ability" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), asked) = castRevolt revolt (boardOf mountain)
          case humans armed of
            tokens@[_, _, _] -> do
              Spec.assertEqWith s "each is 1/1" (fmap (`Projection.powerOf` armed) tokens) [Just 1, Just 1, Just 1]
              Spec.assertBool s (all (\oid -> Projection.hasKeyword Keyword.Type.Haste oid armed) tokens) "each has haste"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers armed)) 1
            other -> Spec.assertFailure s ("expected exactly three Human tokens, got " <> show (length other))
          -- The second invariant: a group binding names every token, so unlike
          -- CR 603.7c's singular "it" there is no candidate to choose between.
          Spec.assertEqWith s "no binding prompt was issued" asked []
        -- Binding one of the three would be the engine choosing; binding all
        -- three is what "those tokens" says.
        Spec.it s "CR 111.1 \"those tokens\" names every minted token, so all three are sacrificed" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), _) = castRevolt revolt (boardOf mountain)
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "three were minted" (length (humans armed)) 3
          Spec.assertEqWith s "and none is left" (humans after) []
          Spec.assertEqWith s "the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
        -- CR 614.16 meets the plural binding, and this is where it differs from
        -- CR 603.7c's singular "it": a replacement that multiplies the count just
        -- makes the set bigger. "Those tokens" still names all of them, so there
        -- is nothing to ask and nothing survives -- where the singular case must
        -- prompt (see delayedSpec's doubled Tidal Wave).
        Spec.it s "CR 614.16 a doubled Create binds all six, unprompted" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (boardOf mountain)
              ((_, armed), asked) = castRevolt revolt base
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "the replacement really doubled the Create" (length (humans armed)) 6
          Spec.assertEqWith s "still nothing to ask" asked []
          Spec.assertEqWith s "and all six are sacrificed" (humans after) []
        -- CR 603.7c's "no longer in the zone it's expected to be in": one token
        -- already gone does not spare the others, and the ability is still spent.
        Spec.it s "CR 603.7c one token already gone leaves the rest sacrificed" $ do
          revolt <- S.printingOf s registry "Thatcher Revolt"
          mountain <- S.printingOf s registry "Mountain"
          let ((_, armed), _) = castRevolt revolt (boardOf mountain)
              killed = case humans armed of
                token : _ -> S.settleSba (S.runPure S.identityAnswer armed (Event.destroy Regenerability.Regenerable [token]))
                [] -> armed
              after = resolveAll (settle (beginEndStep killed))
          Spec.assertEqWith s "two were left to sacrifice" (length (humans killed)) 2
          Spec.assertEqWith s "and both are gone" (humans after) []
          Spec.assertEqWith s "the store is still emptied" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "nothing stuck on the stack" (GameState.stack after) []
        -- The positive control for the three "nothing to ask" assertions above.
        -- They are claims about the ANSWERER as much as about the engine: a
        -- recordPrompts that matched the wrong constructor or dropped its
        -- State.modify' would report an empty list for every case in this group
        -- and every one of them would pass. So the SAME answerer is pointed at
        -- Tidal Wave under Doubling Season, the singular case that must prompt.
        Spec.it s "the recorder does see a prompt when the singular case asks for one" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          doublingSeason <- S.printingOf s registry "Doubling Season"
          let (_, base) = S.addCreature doublingSeason S.alice (S.landsInPlay island 3)
              (_, asked) = uncurry castUnderPrompts (S.handOne tidalWave base)
          Spec.assertEqWith s "one prompt, offering the two minted Walls" (fmap length asked) [2]

-- Salt Road Skirmish {3}{B} Sorcery: "Destroy target creature. Create two 1/1 red
-- Warrior creature tokens. They gain haste until end of turn. Sacrifice them at
-- the beginning of the next end step."
--
-- tokenSetSpec's other half. There the group binding was WRITTEN by a Create and
-- read by the one opcode that understood it; here a THIRD sentence reads the same
-- group through ObjectRef.InSlot, which is what makes the binding a general
-- reference rather than Sacrifice's private channel. "They" and "them" are the
-- same two tokens, named by two different opcodes in one resolution.
tokenGroupReadSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tokenGroupReadSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      warriors gs = filter (\oid -> Set.member Subtype.Warrior (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- alice casts the Skirmish off four Swamps with bob's creature the only
      -- one on the battlefield, so S.identityAnswer's lowest-legal-recipient
      -- pick is that creature and nothing else can be targeted by accident.
      board swamp victim = S.addCreature victim S.bob (S.landsInPlay swamp 4)
      castSkirmish skirmish base =
        let (gs, oid) = S.handOne skirmish base
         in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid)))
   in Spec.describe s "Group read through InSlot" $ do
        -- THE PROVING CASE. Before ObjectRef.InSlot could see a group binding,
        -- "they gain haste" resolved against an empty slot and granted haste to
        -- nobody, while the two tokens still appeared and were still sacrificed
        -- -- so only the keyword assertion discriminates.
        Spec.it s "CR 613.1f \"they gain haste\" reaches BOTH minted tokens" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (_, base) = board swamp rats
              after = castSkirmish skirmish base
          case warriors after of
            tokens@[_, _] ->
              Spec.assertEqWith
                s
                "each of the two has haste"
                (fmap (\oid -> Projection.hasKeyword Keyword.Type.Haste oid after) tokens)
                [True, True]
            other -> Spec.assertFailure s ("expected exactly two Warrior tokens, got " <> show (length other))
        Spec.it s "CR 701.8 the same resolution still destroys its target" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (victim, base) = board swamp rats
              after = castSkirmish skirmish base
          Spec.assertBool s (Set.notMember victim (GameState.battlefield after)) "the targeted creature is gone"
          Spec.assertEqWith s "and two Warriors stand" (length (warriors after)) 2
        -- The group is read by TWO opcodes in one resolution, and the second
        -- must still find it: ModifyTarget consuming the slot would leave the
        -- delayed ability with nothing to sacrifice.
        Spec.it s "CR 603.7c and the delayed ability still sacrifices both" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (_, base) = board swamp rats
              armed = castSkirmish skirmish base
              after = resolveAll (settle (beginEndStep armed))
          Spec.assertEqWith s "two before the end step" (length (warriors armed)) 2
          Spec.assertEqWith s "none after it" (warriors after) []
        -- CR 608.2b: "if all its targets ... are now illegal, the spell doesn't
        -- resolve". The card's own Gatherer ruling spells out what that costs
        -- here -- "it won't resolve and none of its effects will happen" -- so
        -- the tokens the LATER sentences would have made are never minted and
        -- the group is never bound. Also the negative control for the group
        -- read: no group, and the haste sentence still has to be harmless.
        Spec.it s "CR 608.2b an illegal target takes the whole spell, tokens and all" $ do
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          swamp <- S.printingOf s registry "Swamp"
          rats <- S.printingOf s registry "Typhoid Rats"
          let (victim, base) = board swamp rats
              (gs, oid) = S.handOne skirmish base
              onStack = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice oid))
              -- The target leaves before the Skirmish resolves, which is the only
              -- way to make it illegal without a second card.
              gone = S.settleSba (S.runPure S.identityAnswer onStack (Event.destroy Regenerability.Regenerable [victim]))
              after = resolveAll gone
          Spec.assertBool s (Set.notMember victim (GameState.battlefield after)) "the target really left"
          Spec.assertEqWith s "no Warriors were minted" (warriors after) []
          Spec.assertEqWith s "and nothing was armed" (Seq.length (GameState.delayedTriggers after)) 0
          -- Otherwise "no Warriors" would also be true of a spell still sitting
          -- on the stack, which is a different bug wearing the same result.
          Spec.assertEqWith s "the spell left the stack rather than stalling on it" (GameState.stack after) []

-- Harried Dronesmith {3}{R} Creature -- Human Artificer 2/3: "At the beginning of
-- combat on your turn, create a 1/1 colorless Thopter artifact creature token with
-- flying. It gains haste until end of turn. Sacrifice it at the beginning of your
-- next end step."
--
-- tokenGroupReadSpec's SINGULAR, and the whole reason it is a separate group: a
-- Create binds one token into the binding's target field and several into its
-- objects group (Pawl.Engine.Resolve.bindSlot and bindObjectsSlot), and the two
-- are read back by different routes. Salt Road Skirmish's "they" reads the group,
-- which has always been live; this card's "it" reads the single target, which the
-- ability path once fixed before its effect fold began -- so CR 608.2c's "in the
-- order written" was violated for the second sentence only, and only when the
-- sentence before it made exactly one token.
--
-- ATTACKING is the discriminating observable, not the keyword. CR 302.6 keeps a
-- creature that has not been controlled continuously since the turn began from
-- attacking, and CR 702.10b is the only thing that lifts it; the token entered
-- during this very combat phase, so it is on the attackers menu if and only if
-- the haste grant landed on it. Asserting Projection.hasKeyword alone would be a
-- claim about a stored effect rather than about the game.
--
-- The token is authored with FLYING only. Printing haste on its face would make
-- every assertion below pass with the grant deleted entirely.
singleTokenSlotReadSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
singleTokenSlotReadSpec s registry =
  let thopters gs = filter (\oid -> Set.member Subtype.Thopter (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- alice, active, sitting in her own beginning of combat step with the
      -- Dronesmith and a Goblin Piker both SETTLED (S.addCreature stamps
      -- Sickness.Settled), so neither of the two creatures she already had is
      -- summoning-sick and a bug that kept EVERY creature off the menu could not
      -- hide inside assertion 2. The rest of the turn is scheduled so Engine.runStep
      -- can walk it.
      board dronesmith piker =
        let (dronesmithId, gs1) = S.addCreature dronesmith S.alice (Setup.emptyGame S.bothPlayers)
            (pikerId, gs2) = S.addCreature piker S.alice gs1
         in ( dronesmithId,
              pikerId,
              gs2
                { GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice,
                  GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
                  GameState.remaining =
                    Seq.fromList
                      [ Phase.Combat CombatStep.DeclareAttackers,
                        Phase.Combat CombatStep.DeclareBlockers,
                        Phase.Combat CombatStep.CombatDamage,
                        Phase.Combat CombatStep.EndOfCombat,
                        Phase.PostcombatMain,
                        Phase.Ending EndingStep.EndStep,
                        Phase.Ending EndingStep.Cleanup
                      ]
                }
            )
      -- The real turn loop, one whole step at a time: Engine.runStep is what
      -- writes the CR 603.2b StepBegan record this trigger matches, runs the
      -- priority round that resolves it, and advances the schedule.
      step gs = snd (Engine.runGamePure S.identityAnswer gs Engine.runStep)
      -- Run steps until the cleanup step is scheduled, which is one step PAST the
      -- end step -- so the delayed ability has fired and resolved. Bounded, so a
      -- schedule bug cannot loop forever.
      throughEndStep =
        let go n gs =
              if n <= (0 :: Int) || GameState.phase gs == Phase.Ending EndingStep.Cleanup
                then gs
                else go (n - 1) (step gs)
         in go 10
      dronesmithBoard = do
        dronesmith <- S.printingOf s registry "Harried Dronesmith"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (board dronesmith piker)
   in Spec.describe s "Single-token slot read through InSlot" $ do
        -- CR 603.2b then CR 111.1: the step began, the ability triggered, and one
        -- token stands. The characteristics are all here because the token is
        -- authored in this card's own text and nothing else asserts them.
        Spec.it s "CR 111.1 the combat trigger mints one 1/1 colorless flying Thopter artifact creature" $ do
          (_, _, gs) <- dronesmithBoard
          let after = step gs
          case thopters after of
            [token] -> do
              Spec.assertEqWith s "1/1" (S.powerToughnessOf token after) (Just (1, 1))
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying token after) "flying"
              Spec.assertEqWith s "colorless" (Projection.colorsOf token after) Set.empty
              Spec.assertEqWith
                s
                "an artifact creature"
                (Projection.cardTypesOf token after)
                (Set.fromList [CardType.Artifact, CardType.Creature])
              Spec.assertEqWith s "under alice's control" (Projection.controllerOf token after) (Just S.alice)
              Spec.assertEqWith s "and untapped" (fmap Object.tapped (Game.lookupObject token after)) (Just TapState.Untapped)
              Spec.assertEqWith s "the same resolution armed the delayed ability" (Seq.length (GameState.delayedTriggers after)) 1
            other -> Spec.assertFailure s ("expected exactly one Thopter token, got " <> show (length other))
        -- THE PROVING CASE. "It gains haste until end of turn" is the
        -- sentence after the one that made the token, on a TRIGGERED ability, and
        -- the token's slot is read through ObjectRef.InSlot. With the ability
        -- path's pre-fold snapshot, that read comes back empty, ModifyTarget
        -- stores nothing, and CR 302.6 keeps the token home.
        --
        -- The Piker is asserted onto the same menu deliberately: it makes an
        -- answer of "every creature alice controls" and an answer of "none" both
        -- fail, so the Thopter's membership is the only thing that can carry
        -- this.
        Spec.it s "CR 702.10b \"it gains haste\" reaches the one token, so it can attack the turn it entered" $ do
          (_, pikerId, gs) <- dronesmithBoard
          let after = step gs
              menu = Combat.legalAttackers S.alice after
          Spec.assertEqWith s "the beginning of combat step is over" (GameState.phase after) (Phase.Combat CombatStep.DeclareAttackers)
          case thopters after of
            [token] -> do
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Haste token after) "the grant landed on the token"
              Spec.assertBool s (List.elem token menu) "so CR 302.6 does not keep it out of the attackers menu"
              Spec.assertBool s (List.elem pikerId menu) "and the Piker that was already there is on the same menu"
            other -> Spec.assertFailure s ("expected exactly one Thopter token, got " <> show (length other))
        -- The regression guard on the group reader's LIVE read, which the fix must
        -- not disturb: the third sentence's delayed ability reads the same slot at
        -- alice's own end step (CR 603.7 / 603.7b), and the onset is immediate, so
        -- "your next end step" is this turn's.
        Spec.it s "CR 603.7b the delayed ability sacrifices the token at alice's end step" $ do
          (_, _, gs) <- dronesmithBoard
          let armed = step gs
              after = throughEndStep armed
          Spec.assertEqWith s "one Thopter before the end step" (length (thopters armed)) 1
          Spec.assertEqWith s "none after it" (thopters after) []
          Spec.assertEqWith s "and the store is empty" (Seq.length (GameState.delayedTriggers after)) 0
          Spec.assertEqWith s "with nothing stuck on the stack" (GameState.stack after) []

-- CR 603.3b: "puts each triggered ability they control ... on the stack in any
-- order they choose". The centerpiece: two triggers, one controller, and an
-- order that changes the answer.
orderingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
orderingSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      countersOn oid gs = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice has Khabál Ghoul out and casts Tidal Wave, so both a delayed
      -- sacrifice and a step trigger are pending at the same end step.
      boardOf tidalWave khabalGhoul island =
        let (gs0, waveId) = S.handOne tidalWave (S.landsInPlay island 3)
            (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
            cast = resolveAll (snd (Engine.runGamePure S.identityAnswer gs1 (S.cast S.alice waveId)))
         in (ghoul, beginEndStep cast)
      -- The source of the OTHER pending trigger: Tidal Wave's delayed ability,
      -- whose source is the resolved spell's id rather than any permanent.
      otherThan ghoul gs =
        let sources = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
         in case filter (/= TriggerSource.OfObject ghoul) sources of
              src : _ -> src
              [] -> TriggerSource.OfObject ghoul
      -- An answerer that puts a chosen source LAST on the stack, so it resolves
      -- FIRST (CR 603.3b's answer is the order they are PUT on the stack). The
      -- two triggers here come from two different sources, so the entry's SOURCE
      -- is enough to name one of them; battleCrySpec is where the ability half
      -- of the entry (#61) is what does the naming.
      orderLast :: TriggerSource.TriggerSource -> Prompt.Prompt r -> r
      orderLast wanted p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              pick keep = fmap fst (filter (\entry -> (TriggerEntry.source (snd entry) == wanted) == keep) indexed)
           in pick False <> pick True
        _ -> S.identityAnswer p
      -- Counts how many times the ordering prompt was asked, answering canonically.
      countingAnswer :: Prompt.Prompt r -> State.State Int r
      countingAnswer p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (+ 1)
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.identityAnswer p)
      -- alice has a permanent that watches creatures enter, plus lands and a
      -- token-maker in hand. Casting and resolving the maker is one event, so CR
      -- 603.6a fires the watcher once per entrant.
      watcherBoard watcher land n maker =
        let (gs0, makerId) = S.handOne maker (S.landsInPlay land n)
            (_, gs1) = S.addCreature watcher S.alice gs0
         in (gs1, makerId)
      -- Cast it, run priority out, and report how many times CR 603.3b's order
      -- was asked for along the way.
      castCounting gs makerId =
        let ((_, after), asked) = State.runState (Engine.runGame countingAnswer gs (S.cast S.alice makerId >> Engine.priorityLoop)) 0
         in (after, asked)
   in Spec.describe s "TriggerOrdering" $ do
        Spec.it s "CR 603.3b two triggers under one controller ask for an order, exactly once" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, gs) = boardOf tidalWave khabalGhoul island
              (_, asked) = State.runState (Engine.runGame countingAnswer gs Engine.settleForPriority) 0
          Spec.assertEqWith s "asked once" asked 1
        -- THE ELISION, and its negative, one permanent apart. Dragon Fodder's
        -- single Create of two tokens is one event (CR 603.6a), so each board's
        -- watcher contributes two EQUAL entries under one controller.
        --
        -- Soul Warden's payload reads no slot, so both orders gain the same 2
        -- life and there is nothing to decide. The life assertion is what stops
        -- `asked == 0` passing because the triggers never fired.
        Spec.it s "CR 603.3b a batch of interchangeable triggers is not asked about (Soul Warden)" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          mountain <- S.printingOf s registry "Mountain"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (gs, fodderId) = watcherBoard soulWarden mountain 2 dragonFodder
              (after, asked) = castCounting gs fodderId
          Spec.assertEqWith s "not asked" asked 0
          Spec.assertEqWith s "and both triggers resolved: 1 life each" (S.lifeOf S.alice after) (fmap (+ 2) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "with both goblins still standing" (length (S.tokensOf after)) 2
        -- Aether Flash is Warstorm Surge's shape and the reason entry equality
        -- alone is not the test: the two entries are equal, their bindings name
        -- different creatures, and CR 117.3b hands priority back between the two
        -- resolutions -- so which goblin was shot first is observable.
        Spec.it s "CR 117.3b the same-shaped batch IS asked about when the payload reads the entrant (Aether Flash)" $ do
          aetherFlash <- S.printingOf s registry "Aether Flash"
          mountain <- S.printingOf s registry "Mountain"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (gs, fodderId) = watcherBoard aetherFlash mountain 2 dragonFodder
              (after, asked) = castCounting gs fodderId
          Spec.assertEqWith s "asked once" asked 1
          Spec.assertEqWith s "and both triggers resolved: 2 damage kills a 1/1 (CR 704.5g)" (S.tokensOf after) []
        -- The control: one entrant, so one trigger, which the older `length mine
        -- < 2` guard already elided. Without it, "Soul Warden asks nothing"
        -- cannot be told apart from "the batch was never two".
        Spec.it s "CR 603.3b one trigger is elided by count alone (Soul Warden, one token)" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          island <- S.printingOf s registry "Island"
          tidalWave <- S.printingOf s registry "Tidal Wave"
          let (gs, waveId) = watcherBoard soulWarden island 3 tidalWave
              (after, asked) = castCounting gs waveId
          Spec.assertEqWith s "not asked" asked 0
          Spec.assertEqWith s "and the one trigger resolved" (S.lifeOf S.alice after) (fmap (+ 1) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "off one token" (length (S.tokensOf after)) 1
        -- Sacrifice resolves FIRST: the Wall token dies, and CR 608.2h has the
        -- Ghoul count it when its own effect is applied. The token has NO printed
        -- card (CR 111.1) and its death happened at a boundary the scan already
        -- passed -- so a re-derived type line or a drained queue both read zero.
        --
        -- orderLast's argument is the source PUT LAST on the stack, i.e. the one
        -- that RESOLVES FIRST (CR 603.3b, see orderLast's own comment above): for
        -- the sacrifice to resolve first, the OTHER (non-Ghoul) trigger is the one
        -- named -- not the Ghoul itself.
        Spec.it s "CR 608.2h sacrificing first makes the Ghoul count the token" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (ghoul, gs) = boardOf tidalWave khabalGhoul island
              after = snd (Engine.runGamePure (orderLast (otherThan ghoul gs)) gs Engine.priorityLoop)
          Spec.assertEqWith s "the token was counted" (countersOn ghoul after) 1
        -- The Ghoul resolves FIRST: the token is still alive, so it is not
        -- counted. Same board, same cards, opposite answer -- which is what makes
        -- the ordering a genuine choice rather than a formality.
        Spec.it s "CR 608.2h counting first means the token is still alive and is not counted" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (ghoul, gs) = boardOf tidalWave khabalGhoul island
              after = snd (Engine.runGamePure (orderLast (TriggerSource.OfObject ghoul)) gs Engine.priorityLoop)
          Spec.assertEqWith s "nothing counted" (countersOn ghoul after) 0
        -- M-1 (review): permute's reject-not-repair guard, pinned directly. The
        -- centerpiece above only ever answers with a valid permutation (via
        -- orderLast/countingAnswer), and the canonical-answer tests elsewhere use
        -- the identity -- so nothing exercises the fallback branch. "Rejected"
        -- means the input list comes back verbatim: nothing dropped, nothing
        -- duplicated.
        Spec.it s "permute applies a genuine permutation" $ do
          Spec.assertEqWith s "reordered" (Game.permute "abc" [2, 1, 0]) "cba"
        Spec.it s "permute rejects a short answer, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [1, 0]) "abc"
        Spec.it s "permute rejects a duplicate index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [0, 0, 1]) "abc"
        Spec.it s "permute rejects an out-of-range index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Game.permute "abc" [0, 1, 5]) "abc"
        -- M-2 (review): apnapPlayers rotates the turn order to start at the active
        -- player and filters to controllers with a pending trigger -- genuinely new
        -- behaviour versus M3f's apnapOrder, which never consulted turn order at
        -- all, and untested where two DIFFERENT players each control a trigger.
        -- Barbarian Outcast's state trigger (CR 603.8) needs no event, so one
        -- Outcast under EACH player, both controlling no Swamps, gives two
        -- controllers with one trigger apiece -- fewer than two each, so no
        -- ordering prompt is asked and the test isolates the cross-controller walk.
        Spec.it s "CR 101.4/603.3b the active player's trigger is placed first (bottom of stack)" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.bothPlayers
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              (_, gs2) = S.addCreature barbarianOutcast S.bob gs1
              placed = snd (Engine.runGamePure S.identityAnswer gs2 Engine.placePendingTriggers)
              controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
              stack = GameState.stack placed
          case stack of
            [top, bottom] -> do
              Spec.assertEqWith s "the OTHER player's trigger is on top -- placed second" (controllerOf top) (Just S.bob)
              Spec.assertEqWith s "the active player's (alice's) trigger is at the bottom -- placed first" (controllerOf bottom) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- The same walk with a third seat and a departure. Barbarian Outcast's
        -- state trigger (CR 603.8) needs no event, so one Outcast under each of
        -- alice, bob and carol -- none controlling a Swamp -- gives three
        -- controllers with one trigger apiece. The two ordering assertions are
        -- the APNAP rotation itself: it starts at the active player and takes the
        -- seats still in the game, so carol's trigger is placed after alice's.
        -- Unobservable at two players, where a departure ends the game before any
        -- trigger is gathered.
        --
        -- Bob's trigger is absent for a different reason than it once was: CR
        -- 800.4a's first clause removes his Outcast with him, so the trigger
        -- never exists to be filtered. Engine.apnapPlayers still filters his seat
        -- out of the rotation -- see the still-playing filter there -- and the
        -- assertion on his Outcast below is what keeps this case honest about
        -- which rule did what.
        Spec.it s "CR 101.4/603.3b APNAP orders the two remaining players' triggers starting at the active player, and a departed seat's permanent is gone with it" $ do
          barbarianOutcast <- S.printingOf s registry "Barbarian Outcast"
          let gs0 = Setup.emptyGame S.threePlayers
              (_, gs1) = S.addCreature barbarianOutcast S.alice gs0
              (bobsOutcast, gs2) = S.addCreature barbarianOutcast S.bob gs1
              (_, gs3) = S.addCreature barbarianOutcast S.carol gs2
              gone = Departure.depart Departure.Type.Conceded S.bob gs3
              placed = snd (Engine.runGamePure S.identityAnswer gone Engine.placePendingTriggers)
              controllerOf oid = fmap Object.owner (Game.lookupObject oid placed)
          Spec.assertBool s (Maybe.isJust (Game.lookupObject bobsOutcast gs3)) "the fixture really gave bob one"
          Spec.assertEqWith s "CR 800.4a: bob's Outcast left the game with him, so it has no trigger to place" (Game.lookupObject bobsOutcast gone) Nothing
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "carol's trigger is on top -- placed second" (controllerOf top) (Just S.carol)
              Spec.assertEqWith s "the active player's (alice's) is at the bottom -- placed first" (controllerOf bottom) (Just S.alice)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))

-- CR 725.2: the monarch's two inherent abilities "have no source and are
-- controlled by the player who was the monarch at the time the abilities
-- triggered" -- triggered abilities in every other respect, so CR 603.3b's
-- own-order choice covers them exactly as it covers an object's trigger. The
-- collision is reachable from the pool: Palace Jailer crowns its controller, and
-- Khabál Ghoul triggers "at the beginning of each end step", so one player's end
-- step fires her Ghoul's trigger and the monarch's inherent draw in one batch.
monarchOrderingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchOrderingSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settleWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      settleWith answer gs = S.runPure answer gs Engine.settleForPriority
      -- alice's Palace Jailer enters and crowns her (its first entry trigger,
      -- BecomeMonarch TheController) on top of whatever board the caller built;
      -- the state is then wound to the beginning of her end step. bob's Goblin
      -- Piker is the Jailer's SECOND entry trigger's exile victim -- without one
      -- that mode is unfillable and CR 603.3c would take the trigger back off the
      -- stack -- and alice's library holds the card the monarch's draw takes.
      crownAndEndStep palaceJailer piker base =
        let (_, gs1) = S.addCreature piker S.bob base
            (_, gs2) = S.addLibraryCard piker S.alice gs1
            (jailer, gs3) = S.addCreature palaceJailer S.alice gs2
            entered = ZoneChange.MkZoneChange jailer jailer Zone.Stack Zone.Battlefield
         in beginEndStep (resolveAll (S.withEvents [GameEvent.Moved entered (Projection.project jailer gs3)] gs3))
      -- Records every ordering payload's SOURCES, in order, answering
      -- canonically. The sources are what this group is about (CR 725.2's
      -- absent one beside a borne one); battleCrySpec asserts on the ability
      -- half of the same entries.
      recordPayloads :: Prompt.Prompt r -> State.State [[TriggerSource.TriggerSource]] r
      recordPayloads p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [fmap TriggerEntry.source entries])
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.identityAnswer p)
      -- Puts the SOURCELESS entry -- CR 725.2's inherent draw, named by what it
      -- is rather than by where it sits -- at the front of the permutation, so it
      -- goes on the stack FIRST and, the stack being LIFO, resolves LAST. That is
      -- the direction the old two-pass placement could not express: it appended
      -- the inherent trigger after the ordered batch, i.e. always on top, always
      -- resolving first.
      sourcelessFirst :: Prompt.Prompt r -> r
      sourcelessFirst p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              pick keep = fmap fst (filter (\entry -> (TriggerEntry.source (snd entry) == TriggerSource.Sourceless) == keep) indexed)
           in pick True <> pick False
        _ -> S.identityAnswer p
      inherentController placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfInherentTrigger pid _) -> Just pid
        _ -> Nothing
      triggerSourceOf placed oid = case fmap Object.source (Game.lookupObject oid placed) of
        Just (Source.OfTrigger src _) -> Just src
        _ -> Nothing
   in Spec.describe s "MonarchTriggerOrdering" $ do
        -- The collision itself: both triggers reach ONE CR 603.3b choice.
        Spec.it s "CR 603.3b/725.2 the inherent end-step draw is offered in the same ordering choice as the Ghoul's trigger" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
          Spec.assertEqWith s "alice really holds the crown" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "one ordering choice, offering the Ghoul's trigger and the sourceless inherent draw together" asked [[TriggerSource.OfObject ghoul, TriggerSource.Sourceless]]
        -- The order is HONOURED, in the direction the old two-pass placement
        -- could not reach: the inherent draw goes on the stack first, so it sits
        -- at the BOTTOM and resolves last.
        Spec.it s "CR 603.3b/725.2 putting the inherent draw on the stack first leaves it at the bottom, resolving last" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              placed = settleWith sourcelessFirst gs
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "the inherent draw is at the bottom -- placed first, resolves last" (inherentController placed bottom) (Just S.alice)
              Spec.assertEqWith s "the Ghoul's trigger is on top -- placed second, resolves first" (triggerSourceOf placed top) (Just ghoul)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- Same board, opposite answer: the inherent draw goes on the stack
        -- last, so it is on top and resolves first. This is what the old engine
        -- forced unconditionally; here it is a choice.
        Spec.it s "CR 603.3b/725.2 putting the inherent draw on the stack last leaves it on top, resolving first" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (ghoul, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              placed = settleWith S.identityAnswer gs
          case GameState.stack placed of
            [top, bottom] -> do
              Spec.assertEqWith s "the inherent draw is on top -- placed second, resolves first" (inherentController placed top) (Just S.alice)
              Spec.assertEqWith s "the Ghoul's trigger is at the bottom -- placed first, resolves last" (triggerSourceOf placed bottom) (Just ghoul)
            other -> Spec.assertFailure s ("expected exactly two triggers on the stack, got " <> show (length other))
        -- Both still resolve, whichever order was chosen: the merge must not
        -- lose the inherent trigger's placement, only relocate it.
        Spec.it s "CR 725.2 the monarch still draws when her own trigger is ordered last" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = crownAndEndStep palaceJailer piker base
              after = snd (Engine.runGamePure sourcelessFirst gs Engine.priorityLoop)
          Spec.assertEqWith s "alice drew the one card in her library" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
        -- The companion elision: with only the inherent trigger in the batch
        -- there is nothing to order, and where the rules leave nothing to ask,
        -- don't prompt.
        Spec.it s "CR 603.3b the inherent draw alone is one trigger, so nothing is asked" $ do
          palaceJailer <- S.printingOf s registry "Palace Jailer"
          piker <- S.printingOf s registry "Goblin Piker"
          let gs = crownAndEndStep palaceJailer piker (Setup.emptyGame S.bothPlayers)
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
              after = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
          Spec.assertEqWith s "alice really holds the crown" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "no ordering choice was offered" asked []
          Spec.assertEqWith s "and she still drew" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
        -- And the mirror: the Ghoul's trigger alone, with no monarch at all, is
        -- also one trigger and also asks nothing.
        Spec.it s "CR 603.3b the Ghoul's trigger alone, with no monarch, asks nothing" $ do
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, base) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
              gs = beginEndStep base
              (_, asked) = State.runState (Engine.runGame recordPayloads gs Engine.settleForPriority) []
          Spec.assertEqWith s "no monarch, so no inherent trigger exists" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "no ordering choice was offered" asked []

-- Sarcomancy {B} Enchantment: "When this enchantment enters, create a 2/2 black
-- Zombie creature token. At the beginning of your upkeep, if there are no Zombies
-- on the battlefield, this enchantment deals 1 damage to you."
interveningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
interveningSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      zombies gs = filter (\oid -> Set.member Subtype.Zombie (Projection.subtypesOf oid gs)) (Set.toList (GameState.battlefield gs))
      -- Sarcomancy enters and its ETB resolves, so a Zombie token is out.
      withZombie sarcomancy =
        let (sarcId, gs0) = S.addCreature sarcomancy S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange sarcId sarcId Zone.Stack Zone.Battlefield
            gs1 = S.withEvents [GameEvent.Moved entered (Projection.project sarcId gs0)] gs0
         in (sarcId, resolveAll (settle gs1))
   in Spec.describe s "InterveningIf" $ do
        Spec.it s "CR 603.6a the enters trigger makes a 2/2 black Zombie token" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, after) = withZombie sarcomancy
          case zombies after of
            [tok] -> do
              Spec.assertEqWith s "2 power" (Projection.powerOf tok after) (Just 2)
              Spec.assertEqWith s "black" (Projection.colorsOf tok after) (Set.singleton Color.Black)
            other -> Spec.assertFailure s ("expected exactly one Zombie token, got " <> show (length other))
        -- CR 603.4: with the condition FALSE, the ability does not trigger AT ALL
        -- -- nothing reaches the stack.
        Spec.it s "CR 603.4 with a Zombie out, the upkeep ability does not trigger" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, board) = withZombie sarcomancy
              atUpkeep = settle (beginUpkeep board)
          Spec.assertEqWith s "nothing on the stack" (GameState.stack atUpkeep) []
          Spec.assertEqWith s "no life lost" (S.lifeOf S.alice atUpkeep) (Just 20)
        Spec.it s "CR 603.4 with no Zombie, it triggers and deals 1 to its controller" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          let (_, board) = withZombie sarcomancy
              killed = case zombies board of
                tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
                [] -> board
              after = resolveAll (settle (beginUpkeep killed))
          Spec.assertEqWith s "alice took 1" (S.lifeOf S.alice after) (Just 19)
        -- CR 608.2a: the case that distinguishes an intervening "if" from a plain
        -- condition. The ability triggered legitimately; a Zombie appearing in
        -- RESPONSE makes it do nothing on resolution.
        Spec.it s "CR 608.2a a Zombie made in response makes the trigger resolve doing nothing" $ do
          sarcomancy <- S.printingOf s registry "Sarcomancy"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, board) = withZombie sarcomancy
              killed = case zombies board of
                tok : _ -> S.settleSba (S.runPure S.identityAnswer board (Event.destroy Regenerability.Regenerable [tok]))
                [] -> board
              onStack = settle (beginUpkeep killed)
              -- The Zombie arrives under BOB's control, which is exactly the
              -- point: CR 603.4's clause is "no Zombies on the battlefield",
              -- not "no Zombies you control".
              responded = snd (S.addToken (zombieTokenOf sarcomancy piker) S.bob onStack)
              after = resolveAll responded
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "no damage on resolution" (S.lifeOf S.alice after) (Just 20)

-- The 2/2 black Zombie Sarcomancy's own ETB mints, read back out of the card data
-- so the "in response" fixture makes the same object the card would.
zombieTokenOf :: Printing.Printing -> Printing.Printing -> Card.Type.Card
zombieTokenOf sarcomancy pikerFallback =
  let created effect = case effect of
        Effect.Create _ card _ _ -> Just card
        _ -> Nothing
      abilityEffects = concatMap (Modal.allEffects . TriggeredAbility.modal) (Face.triggeredAbilities (S.combinedFace sarcomancy))
   in case Maybe.mapMaybe created abilityEffects of
        card : _ -> card
        [] -> Printing.card pikerFallback

-- CR 702.70: poisonous -- the first keyword whose rule text IS a triggered
-- ability, so it is minted by Pawl.Engine.Keyword and gathered by the same
-- Pawl.Engine.Event.eventTriggers scan a printed trigger goes through, with the damaged
-- player carried across in the reserved Binding.triggerPlayer slot.
poisonousSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
poisonousSpec s registry =
  let -- Hang `n` Auras off `host`, each owned by alice. Attached directly rather
      -- than cast: the cast path is proved once, by the whole-card test below.
      hang printing n host gs =
        foldl
          (\g _ -> let (aura, g1) = S.addCreature printing S.alice g in S.attach aura host g1)
          gs
          (replicate n ())
      -- alice attacks with one `attacking` creature wearing `n` copies of the
      -- `aura`; bob defends with one creature per printing in `theirs`.
      board attacking aura n theirs = case S.combatBoardOf [attacking] theirs of
        (gs, attacker : _, blockers) -> Just (hang aura n attacker gs, attacker, blockers)
        _ -> Nothing
   in Spec.describe s "Poisonous" $ do
        -- CR 702.70b: "If a creature has multiple instances of poisonous, each
        -- triggers separately." So the count is a MULTIPLICITY, not a sum --
        -- the opposite of CR 702.164b's toxic, which sums its N values into one
        -- rider. The falsifier is a mint that collapses the count to one
        -- ability.
        Spec.it s "CR 702.70b each instance of poisonous is its own ability" $ do
          Spec.assertEqWith s "poisonous 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 1) 2)) [Keyword.poisonous 1, Keyword.poisonous 1]
          Spec.assertEqWith s "and poisonous 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Poisonous 3) 1)) [Keyword.poisonous 3]
        -- Rule 702.70 is the only keyword in the pool that mints an ability;
        -- every other one is read where it matters (Projection.hasKeyword, the
        -- infect/toxic damage riders), so it must mint nothing here.
        Spec.it s "CR 702.164 toxic mints no triggered ability" $ do
          Spec.assertEqWith s "toxic is a damage rider, not a trigger" (Keyword.triggeredAbilitiesOf (Map.fromList [(Keyword.Type.Toxic 2, 1), (Keyword.Type.Flying, 1), (Keyword.Type.Infect, 1)])) []
        -- CR 702.70a's "that player": the trigger's own event names them, and
        -- the scan stamps them under the reserved slot as it gathers. The
        -- falsifier is an implementation that hands the poison to the ability's
        -- controller (Binding.you) instead.
        Spec.it s "CR 603.2 the damaged player rides the trigger in the reserved slot" $ do
          let ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 7) (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
              bindings = Event.eventBindings TriggerCondition.SelfDealsCombatDamageToPlayer ev
          Spec.assertEqWith s "bob is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.bob)))
        -- The proving test. CR 702.70a: "Whenever this creature deals combat
        -- damage to a player, that player gets N poison counters." bob is dealt
        -- the Piker's two damage AND gets three poison -- poisonous is not
        -- infect (CR 702.90b), so the life still goes.
        Spec.it s "CR 702.70a Snake Cult Initiation gives the damaged player three poison" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, attacker, _) -> do
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs) "the enchanted creature has poisonous 3"
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
              Spec.assertEqWith s "and lost the two life as well" (S.lifeOf S.bob after) (Just 18)
              Spec.assertEqWith s "alice, who controls the ability, gets none" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
        -- What separates poisonous from infect and toxic: it is a TRIGGERED
        -- ability, so the poison arrives when the ability resolves, not as the
        -- damage is dealt. `fightWith` deals combat damage without ever reaching
        -- a priority boundary, so nothing has been gathered yet.
        Spec.it s "CR 702.70a the poison rides the stack, not the damage" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let fought = S.fightWith S.aggressiveAnswer gs
              Spec.assertEqWith s "damage is dealt" (S.lifeOf S.bob fought) (Just 18)
              Spec.assertEqWith s "but no poison until the trigger resolves" (S.playerCounterOf PlayerCounterKind.Poison S.bob fought) 0
        -- CR 702.70b at the board level: two Auras are two poisonous 3
        -- abilities, so two triggers and six counters. The falsifier is a
        -- projection that keeps keywords in a set -- the second grant collapses
        -- into the first and bob takes three.
        Spec.it s "CR 702.70b two Snake Cult Initiations trigger separately for six poison" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 2 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has six poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 6
        -- CR 702.70a is scoped to combat damage dealt TO A PLAYER: a blocked
        -- creature deals its damage to the blocker, so the ability never
        -- triggers and the blocker (not being a player) gets nothing either.
        Spec.it s "CR 702.70a a blocked creature poisons nobody" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case board piker initiation 1 [piker] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob has no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
              Spec.assertEqWith s "and lost no life" (S.lifeOf S.bob after) (Just 20)
        -- CR 613.1f / 613 layer 6: the ability is derived from the POST-LAYER
        -- keywords, so Humility's LoseAllAbilities (a later timestamp, so it
        -- applies after the Aura's grant) takes it away with no arm of its own.
        -- The falsifier is a mint that reads the PRINTED keywords or the Aura's
        -- own static ability instead of the projection.
        Spec.it s "CR 613 Humility strips poisonous along with everything else" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          humility <- S.printingOf s registry "Humility"
          case board piker initiation 1 [] of
            Nothing -> Spec.assertFailure s "fixture should have an attacker"
            Just (gs0, attacker, _) -> do
              let gs = S.withHumility humility gs0
              Spec.assertBool s (not (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker gs)) "the keyword is gone"
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "so bob takes no poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 0
              Spec.assertEqWith s "only the 1/1's one damage" (S.lifeOf S.bob after) (Just 19)
        -- CR 702.70a's "that player" is whoever was DEALT the damage. In a
        -- multiplayer game (CR 800.1) that is not derivable from the ability's
        -- controller, since CR 506.2a has the attacking player choose which
        -- opponent becomes the defending player. The two runs differ only in
        -- the answer to
        -- Prompt.ChooseDefender, so a "give it to the opponent" implementation
        -- cannot pass both.
        Spec.it s "CR 702.70a the poison follows whichever opponent was attacked" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case S.threePlayerCombat [piker] [] [] of
            (_, [], _, _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, attacker : _, _, _) -> do
              let gs = hang initiation 1 attacker base
                  hitBob = S.runCombat (S.attackTo S.bob) gs
                  hitCarol = S.runCombat (S.attackTo S.carol) gs
              Spec.assertEqWith s "bob, attacked, has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob hitBob) 3
              Spec.assertEqWith s "carol, untouched, has none" (S.playerCounterOf PlayerCounterKind.Poison S.carol hitBob) 0
              Spec.assertEqWith s "and the other way round" (S.playerCounterOf PlayerCounterKind.Poison S.carol hitCarol) 3
              Spec.assertEqWith s "bob untouched this time" (S.playerCounterOf PlayerCounterKind.Poison S.bob hitCarol) 0
        -- The whole card, through the real cast path (design.md section 4): pay
        -- {3}{B}, target the Piker, let the Aura enter attached (CR 303.4), then
        -- attack. Everything above hangs the Aura on by fiat.
        Spec.it s "CR 702.70 whole card: cast Snake Cult Initiation, attack, and bob is poisoned" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          initiation <- S.printingOf s registry "Snake Cult Initiation"
          case S.combatBoardOf [piker] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (gs0, attacker : _, _) -> do
              let withSwamps = foldl (\g _ -> snd (S.addCreature swamp S.alice g)) gs0 (replicate 4 ())
                  (spellId, inHand) = S.addHandCard initiation S.alice withSwamps
                  cast = S.runPure S.aggressiveAnswer inHand {GameState.priority = Just S.alice} (S.cast S.alice spellId)
                  resolved = S.runPure S.aggressiveAnswer cast Stack.resolveTop
                  after = S.runCombat S.aggressiveAnswer resolved
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker resolved) "the Aura granted poisonous 3"
              Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
              Spec.assertEqWith s "and took the Piker's two" (S.lifeOf S.bob after) (Just 18)

-- CR 702.115a: "Ingest is a triggered ability. 'Ingest' means 'Whenever this
-- creature deals combat damage to a player, that player exiles the top card of
-- their library.'" Poisonous' condition and poisonous' reserved "that player"
-- slot over a different payload, so what is new here is the PAYLOAD: a zone move
-- whose source is a library nobody targeted.
--
-- Culling Drone is the card -- {1}{B} 2/2 with devoid and ingest and nothing
-- else, so nothing else on it can produce the exile the assertions read.
--
-- Every board stocks the libraries with TWO DISTINCT printings, top and second,
-- and reads the exile zone by NAME. That is what tells "the top card" apart from
-- "a card": an implementation that exiled the bottom, or two, or the wrong
-- player's, puts a different name in exile rather than the same one.
ingestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ingestSpec s registry =
  let -- addLibraryCard puts each card ON TOP, so the deeper card is added first
      -- and `top` ends up as CR 401.2's head.
      stock deeper top pid gs = snd (S.addLibraryCard top pid (snd (S.addLibraryCard deeper pid gs)))
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      nameOfCard = CardName.MkCardName . Text.pack
   in Spec.describe s "Ingest" $ do
        -- CR 702.115b: "If a creature has multiple instances of ingest, each
        -- triggers separately." So the count is a MULTIPLICITY, poisonous'
        -- reading rather than shadow's redundancy. The falsifier is a mint that
        -- collapses the count to one ability.
        Spec.it s "CR 702.115b each instance of ingest is its own ability" $ do
          Spec.assertEqWith s "ingest held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Ingest 2)) [Keyword.ingest, Keyword.ingest]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Ingest 1)) [Keyword.ingest]
        -- The proving test. Unblocked combat damage to bob exiles bob's top card
        -- and leaves the card under it where it was. alice's library is stocked
        -- with two OTHER printings and is asserted untouched, which is what
        -- separates rule 702.115a's "that player" from the ability's controller:
        -- a payload built on Binding.you rather than Binding.triggerPlayer would
        -- exile the Island.
        Spec.it s "CR 702.115a whole card: Culling Drone exiles the damaged player's top card" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          case S.combatBoardOf [drone] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, attacker : _, _) -> do
              let gs = stock island mountain S.alice (stock swamp piker S.bob base)
                  after = S.runCombat S.aggressiveAnswer gs
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Ingest attacker gs) "the Drone has ingest"
              Spec.assertEqWith s "the Piker, bob's top card, is in exile" (namesIn Zone.Exile S.bob after) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "and the Swamp under it stayed in the library" (namesIn Zone.Library S.bob after) (Set.singleton (nameOfCard "Swamp"))
              Spec.assertEqWith s "alice, who controls the ability, exiles nothing" (namesIn Zone.Exile S.alice after) Set.empty
              Spec.assertEqWith s "and keeps both her cards" (namesIn Zone.Library S.alice after) (Set.fromList [nameOfCard "Island", nameOfCard "Mountain"])
              -- Ingest is not a replacement for the damage: rule 702.115a's
              -- ability is additional, so the two life still goes.
              Spec.assertEqWith s "bob took the Drone's two" (S.lifeOf S.bob after) (Just 18)
        -- The negative, on the SAME board but for one blocker: rule 702.115a is
        -- scoped to combat damage dealt TO A PLAYER, and a blocked creature
        -- assigns its damage to the creatures blocking it (CR 510.1c).
        Spec.it s "CR 702.115a a blocked Culling Drone exiles nothing" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          case S.combatBoardOf [drone] [piker] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, _, _) -> do
              let gs = stock swamp mountain S.bob base
                  after = S.runCombat S.aggressiveAnswer gs
              -- The Piker blocked and took the Drone's two, which is what keeps
              -- this from passing on a board where no damage was dealt at all.
              Spec.assertEqWith s "the blocking Piker died" (namesIn Zone.Graveyard S.bob after) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "nothing is exiled" (namesIn Zone.Exile S.bob after) Set.empty
              Spec.assertEqWith s "both cards stayed in the library" (namesIn Zone.Library S.bob after) (Set.fromList [nameOfCard "Mountain", nameOfCard "Swamp"])
              Spec.assertEqWith s "and bob lost no life" (S.lifeOf S.bob after) (Just 20)
        -- CR 702.115a says nothing about a shortfall, so an empty library exiles
        -- nothing and costs nothing: CR 104.3c's loss is on DRAWING, and this is
        -- a move. The same board as the proving test, one thing different.
        Spec.it s "CR 702.115a an empty library exiles nothing and loses nobody" $ do
          drone <- S.printingOf s registry "Culling Drone"
          case S.combatBoardOf [drone] [] of
            (_, [], _) -> Spec.assertFailure s "fixture should have an attacker"
            (base, _, _) -> do
              let after = S.runCombat S.aggressiveAnswer base
              Spec.assertEqWith s "nothing is exiled" (namesIn Zone.Exile S.bob after) Set.empty
              Spec.assertEqWith s "bob is still in the game" (S.lifeOf S.bob after) (Just 18)
        -- CR 800.1 at three seats, the poisonous spec's shape: rule 702.115a's
        -- "that player" is whoever was DEALT the damage, and at two players that
        -- is indistinguishable from "the attacker's one opponent". The two runs
        -- differ only in the answer to Prompt.ChooseDefender.
        Spec.it s "CR 702.115a the exile follows whichever opponent was attacked" $ do
          drone <- S.printingOf s registry "Culling Drone"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          case S.threePlayerCombat [drone] [] [] of
            (_, [], _, _) -> Spec.assertFailure s "fixture should have an attacker"
            (base0, _, _, _) -> do
              let base = stock swamp piker S.bob (stock island mountain S.carol base0)
                  hitBob = S.runCombat (S.attackTo S.bob) base
                  hitCarol = S.runCombat (S.attackTo S.carol) base
              Spec.assertEqWith s "bob, attacked, exiles his Piker" (namesIn Zone.Exile S.bob hitBob) (Set.singleton (nameOfCard "Goblin Piker"))
              Spec.assertEqWith s "carol, untouched, exiles nothing" (namesIn Zone.Exile S.carol hitBob) Set.empty
              Spec.assertEqWith s "and the other way round" (namesIn Zone.Exile S.carol hitCarol) (Set.singleton (nameOfCard "Mountain"))
              Spec.assertEqWith s "bob untouched this time" (namesIn Zone.Exile S.bob hitCarol) Set.empty

-- CR 702.86a: "Annihilator is a triggered ability. 'Annihilator N' means
-- 'Whenever this creature attacks, defending player sacrifices N permanents.'"
-- Rule 702 states it as a triggered ability, like CR 702.70a's poisonous and CR
-- 702.91a's battle cry, so it is minted by
-- Pawl.Engine.Keyword and gathered by the same Pawl.Engine.Event.eventTriggers
-- scan.
--
-- Slivdrazi Monstrosity is the card, and it reaches annihilator the long way
-- round: "Eldrazi you control are Slivers in addition to their other types"
-- (layer 4) feeds "Slivers you control have devoid and annihilator 1" (layer 6),
-- so a Slaughter Drone -- printed an Eldrazi with no annihilator anywhere on it
-- -- is what attacks. That dependency is CR 613.8's, already pinned for the
-- devoid half in Pawl.ColorSpec.
--
-- What separates this keyword from its two siblings is the PLAYER: rule 702.86a
-- names the DEFENDING player, whom CR 508.5 reads off what the creature is
-- attacking, and CR 508.5a makes that one specific player determined per
-- attacking creature. THREE SEATS is what makes that assertable -- at two
-- players "the defending player" and "the attacker's one opponent" are the same
-- player, so an implementation that bound the wrong one would pass.
annihilatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
annihilatorSpec s registry =
  let -- Declares `attacker` and nothing else, attacks `who`, declines all
      -- blocks, and sacrifices the LAST candidate offered.
      --
      -- Every clause is there to keep an assertion from passing by accident.
      -- Declaring one creature keeps the trigger count at one, so "annihilator 1
      -- sacrificed one permanent" is not two abilities coinciding. Declining
      -- blocks keeps combat damage from removing a permanent the edict did not
      -- take. And taking the LAST candidate rather than the first is what proves
      -- the PROMPT is honoured: Replay.defaultAnswer takes the first `count`
      -- candidates, so an engine that ignored the answer would take the other
      -- permanent.
      declaring :: ObjectId.ObjectId -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      declaring attacker who p = case p of
        Prompt.ChooseDefender {} -> who
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers {} -> Map.empty
        Prompt.ChooseSacrifices _ _ _ candidates _ -> Set.fromList (take 1 (reverse candidates))
        _ -> S.aggressiveAnswer p
      -- alice fields Slivdrazi Monstrosity and a Slaughter Drone; bob and carol
      -- each field a Goblin Piker and a Mountain.
      --
      -- TWO permanents each, and that is the point: Effect.PlayerSacrifices
      -- elides the prompt when the candidates do not outnumber the count (CR
      -- 609.3), so a player with exactly one permanent would prove only the
      -- forced path. One of the two is a LAND, which rule 702.86a's unqualified
      -- "N permanents" admits -- and which is the permanent that goes.
      board = do
        slivdrazi <- S.printingOf s registry "Slivdrazi Monstrosity"
        drone <- S.printingOf s registry "Slaughter Drone"
        piker <- S.printingOf s registry "Goblin Piker"
        mountain <- S.printingOf s registry "Mountain"
        pure (S.threePlayerCombat [slivdrazi, drone] [piker, mountain] [piker, mountain])
   in Spec.describe s "Annihilator" $ do
        -- CR 702.86b: "If a creature has multiple instances of annihilator, each
        -- triggers separately." The count is a MULTIPLICITY, exactly as CR
        -- 702.70b makes poisonous'. The falsifier is a mint that collapses the
        -- count to one ability.
        Spec.it s "CR 702.86b each instance of annihilator is its own ability" $ do
          Spec.assertEqWith s "annihilator 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Annihilator 1) 2)) [Keyword.annihilator 1, Keyword.annihilator 1]
          Spec.assertEqWith s "and annihilator 2 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Annihilator 2) 1)) [Keyword.annihilator 2]
        -- CR 508.5 through CR 603.2: the declaration event carries the defending
        -- player, and the scan stamps them under the reserved slot rule 702.86a's
        -- "defending player" reads. The falsifier is an arm that binds the
        -- attacking side instead.
        Spec.it s "CR 603.2 the defending player rides the declaration in the reserved slot" $ do
          let bindings = Event.eventBindings (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime) (GameEvent.AttackerDeclared (ObjectId.MkObjectId 7) S.carol 1)
          Spec.assertEqWith s "carol is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
        -- CR 613.8's dependency, read off the projection before any attack: WHICH
        -- permanents actually carry the granted keyword. Without this the two
        -- board cases below could pass off a keyword nobody has.
        Spec.it s "CR 702.86 Slivdrazi Monstrosity grants annihilator 1 to the Slivers it makes" $ do
          (gs, ours, yours, _) <- board
          case (ours, yours) of
            (slivdrazi : drone : _, piker : _) -> do
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Annihilator 1) drone gs) "the Eldrazi, made a Sliver, has annihilator 1"
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Annihilator 1) slivdrazi gs) "and so does Slivdrazi itself, being a Sliver"
              Spec.assertBool s (not (Projection.hasKeyword (Keyword.Type.Annihilator 1) piker gs)) "bob's creature, which alice does not control, does not"
            _ -> Spec.assertFailure s "fixture should have two permanents a side"
        -- The proving test. alice attacks bob, so CR 508.5 makes bob the
        -- defending player and rule 702.86a makes him sacrifice one permanent of
        -- HIS choice -- the Mountain, which is the candidate the interpreter
        -- named and not the one the engine's fallback would have taken. carol,
        -- an opponent who was not attacked, loses nothing.
        Spec.it s "CR 702.86a the attacked player sacrifices one permanent of their own choosing" $ do
          (gs, ours, yours, hers) <- board
          case (ours, yours, hers) of
            (_ : drone : _, bobsPiker : bobsMountain : _, carolsPiker : carolsMountain : _) -> do
              let after = S.runCombat (declaring drone S.bob) gs
              Spec.assertEqWith s "bob is left with only the Piker" (Game.zoneMembers Zone.Battlefield S.bob after) [bobsPiker]
              Spec.assertBool s (notElem bobsMountain (Game.zoneMembers Zone.Battlefield S.bob after)) "and the permanent he named, the Mountain, is what went"
              Spec.assertEqWith s "carol, not the defending player, sacrifices nothing" (Game.zoneMembers Zone.Battlefield S.carol after) [carolsPiker, carolsMountain]
            _ -> Spec.assertFailure s "fixture should have two permanents a side"
        -- CR 508.5a: the defending player is one SPECIFIC player, and which one
        -- is settled by CR 506.2a's choice. The only difference between this run
        -- and the one above is the answer to Prompt.ChooseDefender, so an
        -- implementation that bound the attacker's controller, or "the opponent",
        -- or a fixed seat cannot pass both.
        Spec.it s "CR 508.5 the sacrifice follows whichever opponent was attacked" $ do
          (gs, ours, yours, hers) <- board
          case (ours, yours, hers) of
            (slivdrazi : drone : _, bobsPiker : bobsMountain : _, carolsPiker : _) -> do
              let after = S.runCombat (declaring drone S.carol) gs
              Spec.assertEqWith s "carol, attacked this time, is left with only the Piker" (Game.zoneMembers Zone.Battlefield S.carol after) [carolsPiker]
              Spec.assertEqWith s "and bob, untouched, keeps both" (Game.zoneMembers Zone.Battlefield S.bob after) [bobsPiker, bobsMountain]
              Spec.assertEqWith s "alice, who controls the ability, sacrifices nothing" (Game.zoneMembers Zone.Battlefield S.alice after) [slivdrazi, drone]
            _ -> Spec.assertFailure s "fixture should have two permanents a side"

-- CR 702.91a: "Battle cry is a triggered ability. 'Battle cry' means 'Whenever
-- this creature attacks, each other attacking creature gets +1/+0 until end of
-- turn.'" Rule 702 states it as a triggered
-- ability, like CR 702.70a's poisonous and CR 702.86a's annihilator, so it is
-- minted by Pawl.Engine.Keyword
-- and gathered by the same Pawl.Engine.Event.eventTriggers scan.
--
-- Hero of Bladehold is the card, and it is here for a second reason: battle cry
-- and its printed "whenever this creature attacks, create two 1/1 white Soldier
-- creature tokens that are tapped and attacking" are TWO DISTINCT triggered
-- abilities of ONE source keyed on ONE event, so declaring it as an attacker
-- puts two entries into a single CR 603.3b ordering choice. That is #61's case:
-- under a source-only payload the two are identical on the wire while their
-- order genuinely matters, since battle cry pumps "each OTHER attacking
-- creature" and CR 611.2c fixes the affected set as the effect begins.
--
-- The card's official ruling (2011-06-01) states the outcome this group pins:
-- "Whenever Hero of Bladehold attacks, both abilities will trigger. You can put
-- them onto the stack in any order. If the token-creating ability resolves
-- first, the tokens each get +1/+0 until end of turn from the battle cry
-- ability."
battleCrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
battleCrySpec s registry =
  let -- Records every CR 603.3b ordering payload offered, verbatim, answering it
      -- canonically and leaving every other prompt to the aggressive answerer --
      -- which declares every legal attacker, so the declaration really happens.
      recordEntries :: Prompt.Prompt r -> State.State [[TriggerEntry.TriggerEntry]] r
      recordEntries p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [entries])
          pure (zipWith const [0 ..] entries)
        _ -> pure (S.aggressiveAnswer p)
      -- Names one of the two entries by WHICH ABILITY it is and puts it first or
      -- last in the permutation. `cryFirst` is about RESOLUTION: the answer is
      -- the order the abilities are PUT ON the stack, and the stack is LIFO, so
      -- the entry named LAST resolves FIRST.
      --
      -- This answerer is the discriminator's whole point (#61). Both entries hang
      -- on the one Hero, so under the source-only payload this replaced there was
      -- nothing to select on but a blind index -- and an index is not something a
      -- player can be asked to mean.
      resolvingFirst :: Bool -> Prompt.Prompt r -> r
      resolvingFirst cryFirst p = case p of
        Prompt.OrderTriggers _ _ entries ->
          let indexed = zip [0 ..] entries
              isCry entry = TriggerEntry.ability (snd entry) == Keyword.battleCry
              pick keep = fmap fst (filter ((==) keep . isCry) indexed)
           in if cryFirst then pick False <> pick True else pick True <> pick False
        _ -> S.aggressiveAnswer p
      powersOf oids gs = fmap (`Projection.powerOf` gs) oids
   in Spec.describe s "BattleCry" $ do
        -- CR 702.91b: "If a creature has multiple instances of battle cry, each
        -- triggers separately." So the count is a MULTIPLICITY, exactly as CR
        -- 702.70b makes poisonous' one -- the falsifier is a mint that collapses
        -- the count to a single ability. Asked of the mint directly rather than
        -- of a board, unlike poisonous' own gameplay-level pair: no card in this
        -- pool prints battle cry twice, and nothing here grants it, so a second
        -- instance is not reachable through play (Snake Cult Initiation is what
        -- makes it reachable for poisonous).
        Spec.it s "CR 702.91b each instance of battle cry is its own ability" $ do
          Spec.assertEqWith s "battle cry held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.BattleCry 2)) [Keyword.battleCry, Keyword.battleCry]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.BattleCry 1)) [Keyword.battleCry]
        -- THE proving test (#61). One source, two DIFFERENT abilities, one
        -- event: the payload's two entries must not be the same value, or the
        -- player being asked for an order has no way to say which order they
        -- mean. The falsifier is the source-only payload this replaced, where
        -- both entries read `OfObject hero`.
        Spec.it s "CR 603.3b two DIFFERENT abilities of one source are distinguishable entries" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              (_, payloads) = State.runState (Engine.runGame recordEntries gs Engine.runStep) []
          case payloads of
            [[a, b]] -> do
              Spec.assertBool s (a /= b) "the two entries are distinguishable"
              Spec.assertEqWith s "both hang on the one Hero" (TriggerEntry.source a) (TriggerEntry.source b)
              Spec.assertEqWith s "and exactly one of them is rule 702.91a's battle cry" (length (filter ((==) Keyword.battleCry . TriggerEntry.ability) [a, b])) 1
            other -> Spec.assertFailure s ("expected one ordering payload of two entries, got " <> show (fmap length other))
        -- The other half of #61: two triggers of the SAME ability must stay
        -- INDISTINGUISHABLE, or the engine would be asking a question with no
        -- answer. CR 603.6a fires the watcher's ability once per entering
        -- creature, and Hero of Bladehold's token-maker puts two Soldiers onto
        -- the battlefield at once, so the second ordering choice of the same
        -- combat is a pair of entries differing only in which token each
        -- remembers -- a difference the entry deliberately does not carry.
        --
        -- The watcher is Aether Flash rather than Soul Warden BECAUSE its payload
        -- reads the entrant: Engine.orderInert now elides the prompt outright for
        -- a watcher that reads nothing, so a batch that still reaches the wire is
        -- the only place two equal entries can be observed at all.
        Spec.it s "CR 603.6a two triggers of the SAME ability stay indistinguishable" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          case S.combatBoardOf [hero, aetherFlash] [] of
            (gs, [_, flashId], _) -> case snd (State.runState (Engine.runGame recordEntries gs Engine.runStep) []) of
              [[a, b], [w1, w2]] -> do
                Spec.assertBool s (a /= b) "the Hero's two abilities are still distinguishable"
                Spec.assertEqWith s "the second choice is the Flash's" (TriggerEntry.source w1) (TriggerSource.OfObject flashId)
                Spec.assertEqWith s "and its two triggers are the same ability from the same source" w1 w2
              other -> Spec.assertFailure s ("expected two ordering payloads of two entries each, got " <> show (fmap length other))
            _ -> Spec.assertFailure s "fixture should give alice a Hero and an Aether Flash"
        -- CR 702.91a's "each OTHER attacking creature", read one word at a time.
        -- The Piker is another attacking creature and gets +1/+0; the Hero is
        -- attacking but is not OTHER; the Wall is neither pumped nor an attacker
        -- at all (CR 702.3b's defender keeps it home), so it fixes that the set
        -- is attackers rather than "creatures you control".
        Spec.it s "CR 702.91a each OTHER attacking creature, and nothing else" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          piker <- S.printingOf s registry "Goblin Piker"
          wallOfStone <- S.printingOf s registry "Wall of Stone"
          case S.combatBoardOf [hero, piker, wallOfStone] [] of
            (gs, [heroId, pikerId, wallId], _) -> do
              let declared = S.runPure S.aggressiveAnswer gs Engine.runStep
              Spec.assertEqWith s "the other attacker is +1/+0" (Projection.powerOf pikerId declared) (Just 3)
              Spec.assertEqWith s "+1/+0 leaves toughness alone" (Projection.toughnessOf pikerId declared) (Just 1)
              Spec.assertEqWith s "the Hero does not pump itself" (Projection.powerOf heroId declared) (Just 3)
              Spec.assertEqWith s "and a creature that is not attacking is not pumped" (Projection.powerOf wallId declared) (Just 0)
            _ -> Spec.assertFailure s "fixture should give alice a Hero, a Piker and a Wall of Stone"
        -- THE order-matters pair, and the card's own ruling (2011-06-01): "If the
        -- token-creating ability resolves first, the tokens each get +1/+0 until
        -- end of turn from the battle cry ability."
        --
        -- CR 611.2c is why: "the set of objects it affects is determined when
        -- that continuous effect begins. After that point, the set won't change."
        -- So a Soldier that arrives after battle cry has begun is never in the
        -- set, and one that arrives before it is.
        Spec.it s "CR 603.3b/702.91a resolving the token-maker first pumps the Soldiers" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              after = S.runCombat (resolvingFirst False) gs
          Spec.assertEqWith s "two 2/1 Soldiers" (powersOf (S.tokensOf after) after) [Just 2, Just 2]
          Spec.assertEqWith s "so bob takes 3 + 2 + 2" (S.lifeOf S.bob after) (Just 13)
        -- The same board, the same cards, the opposite answer: battle cry
        -- resolves while the Hero is the only attacker, finds no other attacking
        -- creature, and the Soldiers arrive afterwards at their printed 1/1.
        Spec.it s "CR 603.3b/702.91a resolving battle cry first leaves the Soldiers unpumped" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          let (gs, _, _) = S.combatBoardOf [hero] []
              after = S.runCombat (resolvingFirst True) gs
          Spec.assertEqWith s "two 1/1 Soldiers" (powersOf (S.tokensOf after) after) [Just 1, Just 1]
          Spec.assertEqWith s "so bob takes 3 + 1 + 1" (S.lifeOf S.bob after) (Just 15)

-- CR 702.108a: "Prowess is a triggered ability. 'Prowess' means 'Whenever you
-- cast a noncreature spell, this creature gets +1/+1 until end of turn.'" The
-- rule text IS a triggered ability, like CR
-- 702.70a's, CR 702.86a's and CR 702.91a's, and the first minted trigger to watch
-- something other than its bearer's combat: the event is CR 601.2i's, so
-- Pawl.Engine.Keyword.prowess mints TriggerCondition.SpellCast.
--
-- Monastery Swiftspear, {R} Creature -- Human Monk 1/2 with haste and prowess.
-- 1/2 rather than a square body on purpose: prowess is +1/+1 and battle cry is
-- +1/+0, so every assertion below reads BOTH power and toughness -- a power-only
-- one cannot tell the two payloads apart -- and an asymmetric base also catches
-- a swapped pair of arguments to Modification.ModifyPowerToughness.
--
-- Boil, {3}{R} Instant "Destroy all Islands", is the noncreature spell, for
-- youngPyromancerSpec's reasons: it targets nothing, so no answerer choice
-- enters the fixture, and nobody here controls an Island, so its resolution
-- moves nothing an assertion reads. Goblin Piker, {2}{R}, is the creature spell.
--
-- The printed sentence narrows two things at once -- who cast it and what it was
-- -- so each case below moves exactly one, and the negatives carry the positive
-- control that the cast really happened. THREE seats, carol being the one that
-- is neither the caster nor the ability's controller: at two players "the caster
-- is not you" and "the caster is that one opponent" are the same sentence.
prowessSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
prowessSpec s registry =
  let -- alice bears the Swiftspear and has four Mountains, bob four as well, so
      -- a negative never fails for want of mana; carol is the third seat.
      board mountain swiftspear =
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature mountain pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 4 S.threePlayerGame)
            (spearId, withSpear) = S.addCreature swiftspear S.alice withLands
         in ( spearId,
              withSpear
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      sizeOf oid gs = (Projection.powerOf oid gs, Projection.toughnessOf oid gs)
   in Spec.describe s "Prowess" $ do
        -- THE case: the trigger fires, and the pump is the one rule 702.108a
        -- names rather than merely some pump.
        Spec.it s "CR 702.108a whole card: casting an instant makes Monastery Swiftspear 2/3" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (boilId, gs) = S.addHandCard boil S.alice base
              after = castAndResolve S.alice boilId gs
          Spec.assertEqWith s "1/2 before the cast" (sizeOf spearId gs) (Just 1, Just 2)
          Spec.assertEqWith s "and 2/3 once the trigger resolves" (sizeOf spearId after) (Just 2, Just 3)
        -- "Noncreature", moved on its own: alice still casts, and only what she
        -- casts changes. Without this a filter that admitted every spell and one
        -- that read the card type are indistinguishable.
        Spec.it s "CR 702.108a a CREATURE spell pumps nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          piker <- S.printingOf s registry "Goblin Piker"
          let (spearId, base) = board mountain swiftspear
              (pikerId, gs) = S.addHandCard piker S.alice base
              after = castAndResolve S.alice pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer and not a fixture that
          -- never cast anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and the Swiftspear is still 1/2" (sizeOf spearId after) (Just 1, Just 2)
        -- "You", moved on its own: the same instant from the seat to alice's
        -- left. The paired assertion on the same board is what proves the seat
        -- is the only thing the silence turns on.
        Spec.it s "CR 109.5 'you cast': an OPPONENT's instant pumps nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (bobsBoil, withBobs) = S.addHandCard boil S.bob base
              (alicesBoil, gs) = S.addHandCard boil S.alice withBobs
              byBob = castAndResolve S.bob bobsBoil gs
              byAlice = castAndResolve S.alice alicesBoil gs
          Spec.assertEqWith s "bob's cast really resolved" (length (Game.zoneMembers Zone.Graveyard S.bob byBob)) 1
          Spec.assertEqWith s "and left the Swiftspear at 1/2" (sizeOf spearId byBob) (Just 1, Just 2)
          Spec.assertEqWith s "the same board pumps for alice's own cast" (sizeOf spearId byAlice) (Just 2, Just 3)
        -- CR 514.2: "until end of turn" is armed to the cleanup step, and CR
        -- 611.2c's frozen set is a single creature, so the whole effect goes.
        -- Run as the turn-based action rather than by advancing turns, which
        -- would deck a fixture player (CR 104.3c).
        Spec.it s "CR 514.2 the pump is gone at the cleanup step" $ do
          mountain <- S.printingOf s registry "Mountain"
          swiftspear <- S.printingOf s registry "Monastery Swiftspear"
          boil <- S.printingOf s registry "Boil"
          let (spearId, base) = board mountain swiftspear
              (boilId, gs) = S.addHandCard boil S.alice base
              after = castAndResolve S.alice boilId gs
              cleaned = S.runPure S.identityAnswer after (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
          Spec.assertEqWith s "2/3 while the effect lasts" (sizeOf spearId after) (Just 2, Just 3)
          Spec.assertEqWith s "and 1/2 again afterwards" (sizeOf spearId cleaned) (Just 1, Just 2)
        -- CR 702.108b: "If a creature has multiple instances of prowess, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- battle cry's is: no card in this pool prints prowess twice and nothing
        -- here grants it, so a second instance is unreachable through play.
        Spec.it s "CR 702.108b each instance of prowess is its own ability" $ do
          Spec.assertEqWith s "prowess held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 2)) [Keyword.prowess, Keyword.prowess]
          Spec.assertEqWith s "and held once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Prowess 1)) [Keyword.prowess]

-- CR 702.29c: "'When you cycle this card' means 'When you discard this card to
-- pay an activation cost of a cycling ability.' These abilities trigger from
-- whatever zone the card winds up in after it's cycled."
--
-- Windcaller Aven is the card: a {4}{U}{U} 4/3 with flying, Cycling {U}, and
-- "When you cycle this card, target creature gains flying until end of turn".
-- The trigger is mandatory and its effect is Serpent's Gift's exact shape, so
-- the only new thing any test below can be passing on is the trigger itself.
cyclingTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cyclingTriggerSpec s registry =
  Spec.describe s "CyclingTrigger" $ do
    -- The whole card: cycle the Aven for {U}, its trigger targets the Piker as
    -- it is placed (CR 603.3d), and the Piker is flying once it resolves.
    Spec.it s "CR 702.29c whole card: cycling Windcaller Aven grants flying" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, avenId) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
      Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flying creature gs)) "the Piker does not start with flying"
      case Activate.abilitiesFor avenId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
              -- The settle PLACES the trigger and stamps its target (CR
              -- 603.3d); resolving it is the next thing to happen, and it is on
              -- top of the draw it was triggered alongside.
              placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
              after = S.runPure S.identityAnswer placed Stack.resolveTop
          Spec.assertEqWith s "the Aven is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
          Spec.assertBool s (length (GameState.stack placed) == 2) "the trigger is on the stack, above the draw"
          Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying creature after) "and the Piker has flying once it resolves"
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
    -- "These abilities trigger from whatever zone the card winds up in": the
    -- trigger's source is the graveyard incarnation, which CR 400.7 makes a
    -- DIFFERENT object from the card that was in hand. The scan finds it in
    -- neither of the two places it looked before this rule -- the battlefield,
    -- and a permanent that just left it.
    Spec.it s "CR 702.29c the trigger fires from the graveyard, off a new incarnation" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, avenId) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
      case Activate.abilitiesFor avenId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice avenId ability)
              placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
          Spec.assertEqWith s "the id that was in hand is gone" (Game.lookupObject avenId placed) Nothing
          Spec.assertEqWith s "the draw and the trigger are both on the stack" (length (GameState.stack placed)) 2
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
    -- The discriminating twin, and the reason CR 702.29c needs an event of its
    -- own rather than matching the zone change the discard already records: an
    -- ORDINARY discard of the same card, through the same CR 400.7 funnel, is
    -- not cycling and fires nothing.
    Spec.it s "CR 702.29c discarding the Aven without cycling fires nothing" $ do
      aven <- S.printingOf s registry "Windcaller Aven"
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      let (creature, g0) = S.addCreature piker S.alice (S.landsInPlay island 1)
          (g1, _) = S.handOne aven g0
          gs = g1 {GameState.priority = Just S.alice}
          -- The same card, the same graveyard, one component over: a cost that
          -- discards a card of the player's choice rather than this one.
          discarded = S.runPure S.identityAnswer gs (Cost.payComponent S.alice S.noSource (CostComponent.DiscardCards 1))
          after = S.runPure S.identityAnswer discarded Engine.settleForPriority
      Spec.assertEqWith s "the Aven really did reach the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice discarded)) 1
      Spec.assertEqWith s "nothing was put on the stack" (GameState.stack after) []
      Spec.assertBool s (not (Projection.hasKeyword Keyword.Type.Flying creature after)) "and the Piker never gained flying"
    -- The other control: cycling a card that has no cycling trigger fires
    -- nothing, so the trigger is the Aven's and not the act of cycling.
    Spec.it s "CR 702.29c cycling a card with no such trigger fires nothing" $ do
      mauler <- S.printingOf s registry "Barkhide Mauler"
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      let (_, g0) = S.addCreature piker S.alice (S.landsInPlay forest 2)
          (g1, maulerId) = S.handOne mauler g0
          gs = g1 {GameState.priority = Just S.alice}
      case Activate.abilitiesFor maulerId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice maulerId ability)
              placed = S.runPure S.identityAnswer cycled Engine.placePendingTriggers
          Spec.assertEqWith s "only the draw is on the stack" (length (GameState.stack placed)) 1
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))

-- CR 701.9a: "To discard a card, move it from its owner's hand to that player's
-- graveyard." Nothing in the pool triggered on that until Megrim, {2}{B}
-- Enchantment: "Whenever an opponent discards a card, this enchantment deals 2
-- damage to that player." One trigger condition, one effect, and the effect
-- targets nothing -- so the only new thing any case below can be passing on is
-- the condition.
--
-- The interaction is the reason the condition is hard rather than the condition
-- itself. CR 702.29a: "'Cycling [cost]' means '[Cost], Discard this card: Draw a
-- card'", so cycling IS discarding and a discard trigger has to see it. CR
-- 702.29d then bounds how often: "Some cards have abilities that trigger
-- whenever a player 'cycles or discards' a card. These abilities trigger only
-- once when a card is cycled." An engine that recorded the cycle and the discard
-- as two log entries, both of them describing the one discard, would answer 4
-- damage to the second case below instead of 2.
--
-- bob controls the Megrim throughout, so CR 109.5 fixes its "you" as bob and
-- every "an opponent" below is alice.
discardTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
discardTriggerSpec s registry =
  Spec.describe s "DiscardTrigger" $ do
    -- CR 601.2f's "costs may include ... discarding cards", and CR 701.9a is
    -- per CARD: Cathartic Reunion's additional cost discards two, so the one
    -- Megrim triggers twice and alice takes 4.
    Spec.it s "CR 701.9a whole cards: Cathartic Reunion's two discards fire bob's Megrim twice" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      reunion <- S.printingOf s registry "Cathartic Reunion"
      megrim <- S.printingOf s registry "Megrim"
      let base = snd (S.addCreature megrim S.bob (S.landsInPlay mountain 2))
          (reunionId, g1) = S.addHandCard reunion S.alice base
          -- Exactly two other cards, so CR 701.9b has nothing to ask and the
          -- discard is forced -- the prompt is not what this case is about.
          g2 = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) g1 [1 .. (2 :: Int)]
          g3 = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) g2 [1 .. (4 :: Int)]
          gs =
            g3
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
          cast = S.runPure S.identityAnswer gs (S.cast S.alice reunionId)
          placed = S.runPure S.identityAnswer cast Engine.settleForPriority
          after = S.runPure S.identityAnswer cast Engine.priorityLoop
      Spec.assertEqWith s "both cards were discarded as the cost was paid" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 2
      Spec.assertEqWith s "two triggers, above the sorcery that caused them" (length (GameState.stack placed)) 3
      Spec.assertEqWith s "alice took 2 per discarded card" (S.lifeOf S.alice after) (fmap (subtract 4) (S.lifeOf S.alice gs))
      Spec.assertEqWith s "bob discarded nothing and took nothing" (S.lifeOf S.bob after) (S.lifeOf S.bob gs)
    -- THE case. CR 702.29d: "These abilities trigger only once when a card is
    -- cycled." Barkhide Mauler's whole text is "Cycling {2}", so nothing on it
    -- can contribute a second trigger and the count is the discard's alone.
    Spec.it s "CR 702.29d cycling a card fires the discard trigger exactly once" $ do
      forest <- S.printingOf s registry "Forest"
      piker <- S.printingOf s registry "Goblin Piker"
      mauler <- S.printingOf s registry "Barkhide Mauler"
      megrim <- S.printingOf s registry "Megrim"
      let base = snd (S.addCreature megrim S.bob (S.landsInPlay forest 2))
          (_, withLibrary) = S.addLibraryCard piker S.alice base
          (gs, maulerId) = S.handOne mauler withLibrary
      case Activate.abilitiesFor maulerId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice maulerId ability)
              placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
              after = S.runPure S.identityAnswer cycled Engine.priorityLoop
          Spec.assertEqWith s "the Mauler was discarded to pay the cost" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
          Spec.assertEqWith s "cycling's own draw plus ONE Megrim trigger" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "so alice took 2, not 4" (S.lifeOf S.alice after) (fmap (subtract 2) (S.lifeOf S.alice gs))
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
    -- "An OPPONENT discards", not "a player": the axis is load-bearing, and a
    -- board where only the opponent ever discards cannot tell a correct
    -- implementation from one that ignores the player entirely. The same
    -- board, the same component, one discarder apart.
    Spec.it s "CR 102.2 'an opponent': bob discarding to his own Megrim fires nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      megrim <- S.printingOf s registry "Megrim"
      let base = snd (S.addCreature megrim S.bob (S.landsInPlay mountain 1))
          (_, withAlicesCard) = S.addHandCard piker S.alice base
          (_, gs0) = S.addHandCard piker S.bob withAlicesCard
          gs = gs0 {GameState.priority = Just S.alice}
          discardBy pid = S.runPure S.identityAnswer gs (Cost.payComponent pid S.noSource (CostComponent.DiscardCards 1))
          byAlice = discardBy S.alice
          byBob = discardBy S.bob
          settle g = S.runPure S.identityAnswer g Engine.priorityLoop
      Spec.assertEqWith s "alice's card reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice byAlice)) 1
      Spec.assertEqWith s "and bob's reached his" (length (Game.zoneMembers Zone.Graveyard S.bob byBob)) 1
      Spec.assertEqWith s "the opponent's discard costs her 2" (S.lifeOf S.alice (settle byAlice)) (fmap (subtract 2) (S.lifeOf S.alice gs))
      Spec.assertEqWith s "the controller's own discard costs him nothing" (S.lifeOf S.bob (settle byBob)) (S.lifeOf S.bob gs)
      Spec.assertEqWith s "and costs alice nothing either" (S.lifeOf S.alice (settle byBob)) (S.lifeOf S.alice gs)
      Spec.assertEqWith s "bob's discard put nothing on the stack at all" (GameState.stack (S.runPure S.identityAnswer byBob Engine.settleForPriority)) []

-- alice is the active player in her postcombat main phase, holding a Zealous
-- Conscripts and eight uncastable Goblin Pikers, with five Mountains out; bob
-- controls a Megrim and nothing else. Nothing is in either library, so no draw
-- can happen. Returns bob's Megrim, alice's first Mountain (the other thing the
-- Conscripts can be aimed at) and the Conscripts in her hand.
--
-- Nine cards in hand, so that casting the Conscripts leaves exactly eight and CR
-- 514.1 discards exactly one: the whole board turns on that single discard.
conscriptBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
conscriptBoard mountain piker megrim conscripts =
  let (megrimId, g1) = S.addCreature megrim S.bob (Setup.emptyGame S.bothPlayers)
      (landId, g2) = S.addCreature mountain S.alice g1
      g3 = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) g2 [1 .. (4 :: Int)]
      (conscriptsId, g4) = S.addHandCard conscripts S.alice g3
      g5 = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) g4 [1 .. (8 :: Int)]
   in ( megrimId,
        landId,
        conscriptsId,
        g5
          { GameState.activePlayer = S.alice,
            GameState.turnNumber = 1,
            GameState.phase = Phase.PostcombatMain,
            GameState.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
          }
      )

-- Cast exactly `spell` when it is offered and nothing else, aim every target at
-- `victim`, and otherwise answer as S.identityAnswer does (which passes, and
-- discards the cards CR 514.1 offers in the order it offers them).
--
-- Casting is pinned to the one card rather than left to S.castAnswer because the
-- eight Pikers padding alice's hand are castable in principle; a leg that spent
-- her Mountains on one of them would never reach the Conscripts.
aimedCast :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimedCast spell victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
  Prompt.ChooseAction _ _ actions -> case filter (S.isCastOf spell) actions of
    action : _ -> action
    [] -> A.Pass
  _ -> S.identityAnswer p

-- Run out the three steps conscriptBoard leaves scheduled -- the postcombat main
-- phase, the end step and the cleanup step -- so that every leg observes the same
-- board after CR 514.3a has had its say.
toCleanup :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
toCleanup answer gs = List.foldl' (\g _ -> S.runPure answer g Engine.runStep) gs [1 .. (3 :: Int)]

-- CR 603.3a: "A triggered ability is controlled by the player who controlled its
-- source at the time it triggered." AT THE TIME IT TRIGGERED -- which is not the
-- CR 117.5 boundary where Event.eventTriggers does the scanning, and the cleanup
-- step is where the pool can tell the two apart. CR 514.1 discards down to
-- maximum hand size; CR 514.2 then ends every "until end of turn" effect,
-- control effects included; and only then does CR 514.3a put the waiting
-- triggers on the stack. A permanent stolen until end of turn is therefore back
-- with its owner by the time the scan asks who controls it, one whole turn-based
-- action after the discard that fired its ability.
--
-- Zealous Conscripts, {4}{R} Creature -- Human Warrior 3/3: "Haste. When this
-- creature enters, gain control of target permanent until end of turn. Untap
-- that permanent. It gains haste until end of turn." TARGET PERMANENT is what
-- makes it the producer -- Act of Treason and Ray of Command, the pool's other
-- two "until end of turn" thefts, can only name a creature, and the only card in
-- the pool that triggers on a discard is an enchantment.
--
-- Megrim, {2}{B} Enchantment: "Whenever an opponent discards a card, this
-- enchantment deals 2 damage to that player." CR 109.5 fixes its "an opponent"
-- against "the controller of the object when the ability triggered", so with
-- alice holding it at CR 514.1 her own discard is not an opponent's and the
-- ability does not trigger at all. Reading control at the boundary instead makes
-- it bob's again, alice an opponent, and deals her 2 -- a trigger that the rules
-- say never happened.
--
-- Three legs on one board, one target apart: the theft, the same cast aimed at
-- alice's own Mountain instead, and the same board with nothing cast.
controllerAtTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
controllerAtTriggerSpec s registry =
  Spec.describe s "ControllerAtTrigger" $ do
    Spec.it s "CR 603.3a whole cards: a Megrim stolen until end of turn does not fire on its new controller's own cleanup discard" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      megrim <- S.printingOf s registry "Megrim"
      conscripts <- S.printingOf s registry "Zealous Conscripts"
      let (megrimId, _, conscriptsId, gs) = conscriptBoard mountain piker megrim conscripts
          after = toCleanup (aimedCast conscriptsId megrimId) gs
      Spec.assertEqWith s "CR 514.1 trimmed alice to her maximum hand size, so a discard really happened" (length (Game.zoneMembers Zone.Hand S.alice after)) 7
      Spec.assertEqWith s "CR 514.2 gave the Megrim back, which is what the boundary read would have seen" (Projection.controllerOf megrimId after) (Just S.bob)
      Spec.assertEqWith s "CR 603.3a alice controlled it at CR 514.1, so 'an opponent' was bob and nothing triggered" (S.lifeOf S.alice after) (Just 20)
      Spec.assertEqWith s "and bob, who discarded nothing, is untouched" (S.lifeOf S.bob after) (Just 20)
    Spec.it s "CR 109.5 the twin: the same cast aimed at alice's own Mountain leaves the Megrim with bob, and her discard costs her 2" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      megrim <- S.printingOf s registry "Megrim"
      conscripts <- S.printingOf s registry "Zealous Conscripts"
      let (megrimId, landId, conscriptsId, gs) = conscriptBoard mountain piker megrim conscripts
          after = toCleanup (aimedCast conscriptsId landId) gs
      Spec.assertEqWith s "the same one discard" (length (Game.zoneMembers Zone.Hand S.alice after)) 7
      Spec.assertEqWith s "bob held the Megrim throughout" (Projection.controllerOf megrimId after) (Just S.bob)
      Spec.assertEqWith s "so alice's discard IS an opponent's, and the trigger deals her 2" (S.lifeOf S.alice after) (Just 18)
    Spec.it s "the control leg: no Conscripts cast at all, and the Megrim still fires" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      megrim <- S.printingOf s registry "Megrim"
      conscripts <- S.printingOf s registry "Zealous Conscripts"
      let (_, _, _, gs) = conscriptBoard mountain piker megrim conscripts
          after = toCleanup S.identityAnswer gs
      Spec.assertEqWith s "alice kept the Conscripts, so she discards two down to seven" (length (Game.zoneMembers Zone.Hand S.alice after)) 7
      Spec.assertEqWith s "two discards, two triggers, 4 damage" (S.lifeOf S.alice after) (Just 16)

-- CR 701.6a: "to counter a spell or ability means to cancel it, removing it from
-- the stack. It doesn't resolve and none of its effects occur. A countered spell
-- is put into its owner's graveyard." Nothing in the pool triggered on that
-- until Baral, Chief of Compliance, {1}{U} Legendary Creature -- Human Wizard
-- 1/3: "Instant and sorcery spells you cast cost {1} less to cast. / Whenever a
-- spell or ability you control counters a spell, you may draw a card. If you do,
-- discard a card."
--
-- The condition is hard because the graveyard cannot answer it. Rule 701.6a's
-- last sentence and CR 608.2n send a spell to the same place -- "as the final
-- part of an instant or sorcery spell's resolution, the spell is put into its
-- owner's graveyard" -- so the stack-to-graveyard zone change a countering
-- records is indistinguishable from the one an ordinary resolution records. The
-- first three cases below are that distinction, from three sides: the countering
-- fires, a countering that CR 113.6g stopped does not, and a resolution into the
-- very same graveyard does not. The fourth is the PlayerRelation axis -- whose
-- spell did the countering -- and the fifth is Baral's other half, its CR 601.2f
-- cost reduction.
--
-- bob controls the Baral throughout, so CR 109.5 fixes its "you" as bob (CR
-- 603.3a).
--
-- Baral's reflexive "if you do" is one Optional mode over both instructions
-- (#487), so `Exercises` below draws AND discards.
counterTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterTriggerSpec s registry =
  let -- bob: a Baral, three Islands, one card in his library and a Cancel in
      -- hand. alice: `victim` on the stack. bob's library and hand each hold
      -- exactly one card, so the draw and the discard are both countable, and CR
      -- 701.9b has nothing to ask (a one-card hand discards forced, #63).
      board victim island cancel baral spare =
        let (_, withBaral) = S.addCreature baral S.bob (Setup.emptyGame S.bothPlayers)
            withLands = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) withBaral [1 .. (3 :: Int)]
            (_, withLibrary) = S.addLibraryCard spare S.bob withLands
            (victimId, onStack) = S.spellOnStack victim S.alice withLibrary
            (cancelId, gs) = S.addHandCard cancel S.bob onStack
         in (victimId, cancelId, gs)
      -- Targets the spell already on the stack, and takes rule 603.5's "may".
      answerWith victimId p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject victimId))) sets
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
   in Spec.describe s "CounterTrigger" $ do
        Spec.it s "CR 701.6a whole cards: bob's Cancel counters alice's spell, and Baral draws then discards" $ do
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let (victimId, cancelId, gs) = board piker island cancel baral mountain
              answer :: Prompt.Prompt r -> r
              answer = answerWith victimId
              cast = S.runPure answer gs (S.cast S.bob cancelId)
              countered = S.runPure answer cast Stack.resolveTop
              placed = S.runPure answer countered Engine.settleForPriority
              after = S.runPure answer placed Stack.resolveTop
          Spec.assertEqWith s "the victim was countered into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice countered)) 1
          Spec.assertEqWith s "and never reached the battlefield" (S.creaturesInPlay S.alice countered) 0
          Spec.assertEqWith s "Baral's trigger is the only thing on the stack" (length (GameState.stack placed)) 1
          -- The trigger LANDED, not merely fired: bob's one library card was
          -- drawn (library empty) and then discarded (his graveyard holds the
          -- Cancel and that card, and his hand is empty again).
          Spec.assertEqWith s "bob drew his only library card" (length (Game.zoneMembers Zone.Library S.bob after)) 0
          Spec.assertEqWith s "and discarded it, beside the spent Cancel" (length (Game.zoneMembers Zone.Graveyard S.bob after)) 2
          Spec.assertEqWith s "so bob's hand is empty again" (S.handSize S.bob after) 0
          Spec.assertEqWith s "the stack is empty" (length (GameState.stack after)) 0
        -- THE composition case, and the reason the pair exists. CR 113.6g: "an
        -- object's ability that states it can't be countered ... functions on
        -- the stack", and CR 101.2 makes the "can't" win -- so Rending Volley
        -- is not countered, no countering event happens, and Baral has nothing
        -- to see. The falsifier for an implementation that recorded the event
        -- before the gate, or that read the zone change instead.
        --
        -- Rending Volley rather than Blurred Mongoose, whose "this spell
        -- can't be countered" sits on a creature card: the Mongoose also
        -- prints shroud, and CR 702.18 is not implemented (#488), so it could
        -- not be modelled faithfully. Both cards reach this gate the same way
        -- -- through Face.counterability, read off the spell on the stack.
        Spec.it s "CR 113.6g the same Cancel at Rending Volley counters nothing, so Baral does not trigger" $ do
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          rendingVolley <- S.printingOf s registry "Rending Volley"
          mountain <- S.printingOf s registry "Mountain"
          let (victimId, cancelId, gs) = board rendingVolley island cancel baral mountain
              answer :: Prompt.Prompt r -> r
              answer = answerWith victimId
              cast = S.runPure answer gs (S.cast S.bob cancelId)
              resolved = S.runPure answer cast Stack.resolveTop
              placed = S.runPure answer resolved Engine.settleForPriority
          -- CR 101.2 from the other side: the Cancel itself was not stopped.
          -- It targeted legally (CR 113.6g grants no shroud), resolved, did
          -- nothing, and CR 608.2n put it into bob's graveyard.
          Spec.assertEqWith s "Rending Volley is still on the stack, alone" (GameState.stack placed) [victimId]
          Spec.assertEqWith s "the spent Cancel is bob's only graveyard card" (length (Game.zoneMembers Zone.Graveyard S.bob placed)) 1
          Spec.assertEqWith s "bob drew nothing" (length (Game.zoneMembers Zone.Library S.bob placed)) 1
        -- The negative that keeps the first case from passing vacuously. CR
        -- 608.2n puts a RESOLVED instant into its owner's graveyard -- the same
        -- zone change rule 701.6a's countering makes -- so an implementation
        -- that matched the zone pair rather than the recorded countering would
        -- fire here too.
        Spec.it s "CR 608.2n bob's own Bolt resolving into that same graveyard fires nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          bolt <- S.printingOf s registry "Lightning Bolt"
          let (_, withBaral) = S.addCreature baral S.bob (Setup.emptyGame S.bothPlayers)
              withLand = snd (S.addCreature mountain S.bob withBaral)
              (_, withLibrary) = S.addLibraryCard mountain S.bob withLand
              (boltId, gs) = S.addHandCard bolt S.bob withLibrary
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (S.cast S.bob boltId)
              resolved = S.runPure answer cast Stack.resolveTop
              placed = S.runPure answer resolved Engine.settleForPriority
          Spec.assertEqWith s "the Bolt really did resolve into bob's graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob resolved)) 1
          Spec.assertEqWith s "alice took 3, so it resolved rather than fizzling" (S.lifeOf S.alice resolved) (fmap (subtract 3) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "nothing was put on the stack" (GameState.stack placed) []
          Spec.assertEqWith s "bob drew nothing" (length (Game.zoneMembers Zone.Library S.bob placed)) 1
        -- "A spell or ability YOU CONTROL", not "a spell or ability": the
        -- PlayerRelation is load-bearing, and a board where only bob ever
        -- counters cannot tell a correct implementation from one that ignores
        -- the countering source's controller entirely. The same Cancel at the
        -- same victim, one caster apart -- alice's Cancel counters BOB's
        -- spell, and bob's Baral watches it happen and does nothing.
        --
        -- Also the other half of Baral's static: alice pays Cancel's full
        -- {1}{U}{U}, since "spells YOU cast" is scoped to bob.
        Spec.it s "CR 109.5 'you control': alice's Cancel countering bob's spell does not fire bob's Baral" $ do
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let (_, withBaral) = S.addCreature baral S.bob (Setup.emptyGame S.bothPlayers)
              withLands = List.foldl' (\g _ -> snd (S.addCreature island S.alice g)) withBaral [1 .. (3 :: Int)]
              (_, withLibrary) = S.addLibraryCard mountain S.bob withLands
              (victimId, onStack) = S.spellOnStack piker S.bob withLibrary
              (cancelId, gs) = S.addHandCard cancel S.alice onStack
              answer :: Prompt.Prompt r -> r
              answer = answerWith victimId
              cast = S.runPure answer gs (S.cast S.alice cancelId)
              countered = S.runPure answer cast Stack.resolveTop
              placed = S.runPure answer countered Engine.settleForPriority
          -- The countering really happened, so the silence below is the
          -- relation and not a broken board.
          Spec.assertEqWith s "bob's spell was countered into his graveyard" (length (Game.zoneMembers Zone.Graveyard S.bob countered)) 1
          -- By NAME, not S.creaturesInPlay: bob's own Baral is a creature on
          -- his battlefield throughout, so a bare count could never read 0.
          Spec.assertEqWith s "and the Piker never reached the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Piker") S.bob countered) 0
          Spec.assertEqWith s "nothing was put on the stack" (GameState.stack placed) []
          Spec.assertEqWith s "so bob drew nothing" (length (Game.zoneMembers Zone.Library S.bob placed)) 1
        -- THE discriminating case for rule 701.6a's OTHER subject. That rule is
        -- about "a spell or ability", and Stifle ({U} Instant, "Counter target
        -- activated or triggered ability") counters the second -- but Baral's
        -- printed object is "counters A SPELL", so Baral must stay silent. CR
        -- 113.9 is the rule that keeps the two apart: "activated and triggered
        -- abilities on the stack aren't spells."
        --
        -- ONE board, run two ways, because either half alone proves nothing: a
        -- silent Baral could be a Baral that never worked, and a firing one
        -- could be a condition that ignores what was countered. The Cancel run
        -- fires it and the Stifle run does not, from the same starting state,
        -- with the same interpreter answering `Exercises` to CR 603.5's "may" --
        -- so the silence is not a declined option either.
        --
        -- bob's LIBRARY is the readout, not his hand: Baral draws then discards,
        -- which leaves the hand the size it was.
        Spec.it s "CR 113.9 the same Baral: a countered SPELL fires it, a countered ABILITY does not" $ do
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          stifle <- S.printingOf s registry "Stifle"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          case Face.activatedAbilities (S.combinedFace sorcerer) of
            [] -> Spec.assertFailure s "Prodigal Sorcerer should declare one activated ability"
            ability : _ -> do
              -- bob: Baral, three Islands, one library card, and both a Cancel
              -- and a Stifle in hand. alice: a settled Prodigal Sorcerer (CR
              -- 302.6, so its {T} may be activated) and a Goblin Piker spell on
              -- the stack -- one victim of each kind, standing side by side.
              let (_, withBaral) = S.addCreature baral S.bob (Setup.emptyGame S.bothPlayers)
                  withLands = List.foldl' (\g _ -> snd (S.addCreature island S.bob g)) withBaral [1 .. (3 :: Int)]
                  (_, withLibrary) = S.addLibraryCard mountain S.bob withLands
                  (srcId, withSorcerer) = S.addCreature sorcerer S.alice withLibrary
                  settled = S.runPure S.identityAnswer withSorcerer (Engine.settleAll S.alice)
                  (victimId, onStack) = S.spellOnStack piker S.alice settled
                  (cancelId, withCancel) = S.addHandCard cancel S.bob onStack
                  (stifleId, gs) = S.addHandCard stifle S.bob withCancel
                  -- The SPELL run: bob's Cancel at alice's Piker spell.
                  spellRun = S.runPure (answerWith victimId) gs (S.cast S.bob cancelId)
                  spellCountered = S.runPure (answerWith victimId) spellRun Stack.resolveTop
                  spellPlaced = S.runPure (answerWith victimId) spellCountered Engine.settleForPriority
                  spellAfter = S.runPure (answerWith victimId) spellPlaced Stack.resolveTop
                  -- The ABILITY run: alice activates her Sorcerer at herself,
                  -- and bob's Stifle counters the ability. Aimed at alice so the
                  -- effect that must NOT occur is her own life total.
                  atAlice :: Prompt.Prompt r -> r
                  atAlice p = case p of
                    Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer S.alice))) sets
                    Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                    _ -> S.identityAnswer p
                  -- Stifle's only legal target is the ability -- the Pool.Abilities
                  -- set holds nothing else -- so the default interpreter picks it,
                  -- and its `Exercises` is what would take Baral's "may".
                  atAbility :: Prompt.Prompt r -> r
                  atAbility p = case p of
                    Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                    _ -> S.identityAnswer p
                  activated = S.runPure atAlice (gs {GameState.priority = Just S.alice}) (Activate.activateAbility S.alice srcId ability)
                  abilityRun = S.runPure atAbility activated (S.cast S.bob stifleId)
                  abilityCountered = S.runPure atAbility abilityRun Stack.resolveTop
                  abilityPlaced = S.runPure atAbility abilityCountered Engine.settleForPriority
              -- Half one: a countered SPELL. Baral fires, and lands.
              Spec.assertEqWith s "the Piker spell was countered into alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice spellCountered)) 1
              Spec.assertEqWith s "Baral's trigger is the only thing on the stack" (length (GameState.stack spellPlaced)) 1
              Spec.assertEqWith s "and bob drew his only library card" (length (Game.zoneMembers Zone.Library S.bob spellAfter)) 0
              -- Half two: a countered ABILITY. The countering really happened --
              -- the ability is off the stack and alice took no damage -- and
              -- Baral saw nothing.
              Spec.assertEqWith s "the ability is gone, leaving only the untouched Piker spell" (GameState.stack abilityPlaced) [victimId]
              Spec.assertEqWith s "alice took no damage, so the ability never resolved" (S.lifeOf S.alice abilityPlaced) (Just 20)
              Spec.assertEqWith s "no ability went to a graveyard: alice's is empty" (length (Game.zoneMembers Zone.Graveyard S.alice abilityPlaced)) 0
              Spec.assertEqWith s "bob's holds the spent Stifle alone" (length (Game.zoneMembers Zone.Graveyard S.bob abilityPlaced)) 1
              Spec.assertEqWith s "and Baral never fired: bob's library is untouched" (length (Game.zoneMembers Zone.Library S.bob abilityPlaced)) 1
        -- Baral's OTHER half, and the reason the board above gives bob exactly
        -- three Islands: "instant and sorcery spells you cast cost {1} less to
        -- cast" (CR 601.2f's cost reductions) turns Cancel's {1}{U}{U} into
        -- {U}{U}, so one Island is still untapped once it is paid for.
        Spec.it s "CR 601.2f Baral's reduction leaves an Island untapped after Cancel is cast" $ do
          island <- S.printingOf s registry "Island"
          cancel <- S.printingOf s registry "Cancel"
          baral <- S.printingOf s registry "Baral, Chief of Compliance"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let (victimId, cancelId, gs) = board piker island cancel baral mountain
              answer :: Prompt.Prompt r -> r
              answer = answerWith victimId
              cast = S.runPure answer gs (S.cast S.bob cancelId)
              untapped g =
                length
                  [ oid
                  | oid <- Game.zoneMembers Zone.Battlefield S.bob g,
                    Just obj <- [Game.lookupObject oid g],
                    Object.tapped obj == TapState.Untapped
                  ]
          -- Three Islands and the Baral start untapped; paying {U}{U} taps two.
          Spec.assertEqWith s "four untapped permanents before" (untapped gs) 4
          Spec.assertEqWith s "two after, so only two Islands were tapped" (untapped cast) 2

-- CR 509.3e: "Whenever [a creature] blocks two or more creatures, . . ." -- the
-- form that reads HOW MANY, matched against the same grouped
-- GameEvent.BlocksDeclared SelfBlocks reads.
--
-- Lairwatch Giant {5}{W} Creature -- Giant Warrior 5/3, "This creature can block
-- an additional creature each combat / Whenever this creature blocks two or more
-- creatures, it gains first strike until end of turn", is the card, and the only
-- one that can reach the condition on its own text: the permission it prints is
-- what lets the count get to two.
selfBlocksAtLeastSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksAtLeastSpec s registry =
  let blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      blockOne :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockOne blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> case attackers of
          [] -> Map.empty
          a : _ -> Map.singleton blocker (Set.singleton a)
        _ -> S.aggressiveAnswer p
   in Spec.describe s "SelfBlocksAtLeast" . Spec.it s "CR 509.3e blocking two grants first strike, blocking one does not" $ do
        piker <- S.printingOf s registry "Goblin Piker"
        giant <- S.printingOf s registry "Lairwatch Giant"
        let (gs, _, theirs) = S.combatBoardOf [piker, piker] [giant]
            atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState
            atDamage answer = S.runToStep (Phase.Combat CombatStep.CombatDamage) answer gs
        case theirs of
          [oid] ->
            -- Both legs are the same board and the same card, differing only in
            -- how many attackers the declaration gave it -- which is rule
            -- 509.3e's own variable.
            Spec.assertEqWith
              s
              "two blocks grant it, one does not"
              ( Projection.hasKeyword Keyword.Type.FirstStrike oid (atDamage (blockEverything oid)),
                Projection.hasKeyword Keyword.Type.FirstStrike oid (atDamage (blockOne oid))
              )
              (True, False)
          _ -> Spec.assertFailure s "fixture should give bob one Lairwatch Giant"

-- CR 509.3a: "Whenever [a creature] blocks, . . ." -- the blocking side's
-- declaration trigger, matched against GameEvent.BlocksDeclared, which only
-- Pawl.Engine.Combat.declareBlockers appends, once per blocking creature.
--
-- Pride Guardian {W} Creature -- Cat Monk 0/3, "Defender / Whenever this creature
-- blocks, you gain 3 life", is the card. It is the cheapest producer in the pool:
-- its payload names nothing about the attacker it blocked, so these cases isolate
-- the trigger CONDITION, and 0 power keeps combat damage from moving the number
-- the assertions read.
--
-- The blocker is BOB's, since CR 509.1 has the defending player declare blocks,
-- so every life total below is read off the defending seat.
selfBlocksSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksSpec s registry =
  let -- Attacks with everything and declines every block. aggressiveAnswer's
      -- control leg: the same game with CR 509.1's declaration switched off, and
      -- the only difference between the two answerers.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- Blocks EVERY attacker with `blocker` alone, which aggressiveAnswer
      -- cannot express: it puts every blocker on the first attacker.
      blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
   in Spec.describe s "SelfBlocks" $ do
        -- The proving test, and its control. alice attacks with a 2/1 Goblin
        -- Piker; bob blocks with the Guardian. 20 + 3 = 23 blocking, and
        -- 20 - 2 = 18 declining -- distinct numbers, and neither reachable from
        -- the other by an off-by-one.
        Spec.it s "CR 509.3a whole card: blocking gains 3 life, declining to block gains none" $ do
          (gs, _, theirs) <- board ["Goblin Piker"] ["Pride Guardian"]
          let blocked = S.runCombat S.aggressiveAnswer gs
              unblocked = S.runCombat noBlocks gs
          case theirs of
            [guardian] -> do
              Spec.assertEqWith s "the 0/3 Guardian survives the Piker's 2" (S.lifeOf S.bob blocked) (Just 23)
              Spec.assertBool s (S.onBattlefield guardian blocked) "and is still on the battlefield"
              Spec.assertEqWith s "alice gains nothing: the trigger is the blocker controller's (CR 603.3a)" (S.lifeOf S.alice blocked) (Just 20)
              Spec.assertEqWith s "control leg: no block, no gain, and the Piker's 2 gets through" (S.lifeOf S.bob unblocked) (Just 18)
            _ -> Spec.assertFailure s "fixture should give bob one Pride Guardian"
        -- CR 509.2a: the abilities that triggered on blockers being declared go
        -- onto the stack before the active player gets priority, so they resolve
        -- in the declare blockers step -- not at combat damage, and not at end of
        -- combat. Read at the combat damage step, before any damage is dealt.
        Spec.it s "CR 509.2a the trigger has already resolved when the combat damage step begins" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Pride Guardian"]
          let atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer gs
          Spec.assertEqWith s "the fixture reached the combat damage step" (GameState.phase atDamage) (Phase.Combat CombatStep.CombatDamage)
          Spec.assertEqWith s "and bob is already at 23" (S.lifeOf S.bob atDamage) (Just 23)
        -- CR 603.2: the condition is the BEARER's own declaration. bob blocks one
        -- attacker with two creatures, so two declarations are recorded and only
        -- one of them is the Guardian's. The falsifier is a match that ignores
        -- the blocker on the event: that fires twice, for 26.
        Spec.it s "CR 603.2 another creature's block does not fire the Guardian's ability" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Pride Guardian", "Goblin Piker"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "one gain of 3, not two" (S.lifeOf S.bob after) (Just 23)
        -- CR 509.1a gives each blocker one creature to block by default, so a
        -- second ATTACKER adds a declaration the Guardian is not in. alice attacks with
        -- two Pikers and aggressiveAnswer blocks the first; the second's 2 gets
        -- through. 20 + 3 - 2 = 21. The falsifier is a condition that matched an
        -- attacker's declaration too -- three events rather than one, for 25.
        Spec.it s "CR 509.3a an attacker's own declaration is not a block" $ do
          (gs, _, _) <- board ["Goblin Piker", "Goblin Piker"] ["Pride Guardian"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "gained 3 once, then took 2 from the unblocked Piker" (S.lifeOf S.bob after) (Just 21)
        -- Rule 509.3a's "even if it blocks multiple creatures", now that a
        -- creature can. A High Ground gives bob's team the arity, the Guardian
        -- blocks both Pikers, and the gain is 3 once rather than 3 twice. The
        -- falsifier is a match on the PAIRWISE GameEvent.BlockerDeclared, which
        -- fires per attacker blocked: 26.
        Spec.it s "CR 509.3a blocking TWO creatures still gains 3 once" $ do
          (gs, _, theirs) <- board ["Goblin Piker", "Goblin Piker"] ["Pride Guardian", "High Ground"]
          case theirs of
            [guardian, _] -> do
              let after = S.runCombat (blockEverything guardian) gs
              Spec.assertEqWith s "one gain of 3, not two" (S.lifeOf S.bob after) (Just 23)
            _ -> Spec.assertFailure s "fixture should give bob a Guardian and a High Ground"
        -- The other side of the same coin, and CR 508.3a's own words: a creature
        -- that BLOCKS did not attack. Hanweir Garrison {2}{R} 2/3, "Whenever this
        -- creature attacks, create two 1/1 red Human creature tokens that are
        -- tapped and attacking", is the pool's cheapest attack trigger; here it
        -- is bob's, and blocking. The falsifier is a SelfAttacks arm that matched
        -- the blocking declaration: two tokens rather than none.
        Spec.it s "CR 508.3a a block is not an attack, so a blocking Hanweir Garrison makes no tokens" $ do
          (blocking, _, _) <- board ["Goblin Piker"] ["Hanweir Garrison"]
          (attacking, _, _) <- board ["Hanweir Garrison"] ["Goblin Piker"]
          Spec.assertEqWith s "the Garrison blocked and made nothing" (length (S.tokensOf (S.runCombat S.aggressiveAnswer blocking))) 0
          -- The positive control: the same card on the attacking side really does
          -- have the ability, so the zero above is a fact about blocking rather
          -- than about the fixture.
          Spec.assertEqWith s "the same card attacking makes two" (length (S.tokensOf (S.runCombat S.aggressiveAnswer attacking))) 2

-- CR 509.3b: "Whenever [a creature] blocks a creature, . . ." -- selfBlocksSpec's
-- condition with the attacker NAMED, bound under Binding.blockedCreature.
--
-- Loyal Sentry {W} Creature -- Human Soldier 1/1, "When this creature blocks a
-- creature, destroy that creature and this creature", is the card: the trigger is
-- its whole text, and "that creature" is the binding under test. Every reading is
-- taken at the COMBAT DAMAGE step, before damage is dealt, so a death there is
-- the trigger's (CR 509.2a) and never combat's.
selfBlocksCreatureSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksCreatureSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- aggressiveAnswer blocks the FIRST attacker with everything, which cannot
      -- tell "the attacker the bearer blocked" from "the first attacker". This
      -- one blocks the SECOND.
      blockSecond :: Prompt.Prompt r -> r
      blockSecond p = case p of
        Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
          _ : a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) mine)
          _ -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
      atDamageWithout = S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks
      atDamageSecond = S.runToStep (Phase.Combat CombatStep.CombatDamage) blockSecond
   in Spec.describe s "SelfBlocksCreature" $ do
        -- The proving test, and its control: the same board with CR 509.1's
        -- declaration switched off. Blocking, both creatures are gone before
        -- damage; declining, both are alive and the Piker's 2 gets through.
        Spec.it s "CR 509.3b whole card: blocking destroys the attacker and the Sentry" $ do
          (gs, mine, theirs) <- board ["Goblin Piker"] ["Loyal Sentry"]
          case (mine, theirs) of
            ([piker], [sentry]) -> do
              Spec.assertEqWith
                s
                "both are gone at the combat damage step, and bob took nothing"
                (S.onBattlefield piker (atDamage gs), S.onBattlefield sentry (atDamage gs), S.lifeOf S.bob (S.runCombat S.aggressiveAnswer gs))
                (False, False, Just 20)
              Spec.assertEqWith
                s
                "control leg: no block, so neither dies and the Piker's 2 gets through"
                (S.onBattlefield piker (atDamageWithout gs), S.onBattlefield sentry (atDamageWithout gs), S.lifeOf S.bob (S.runCombat noBlocks gs))
                (True, True, Just 18)
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- The binding, which is the whole difference from CR 509.3a: "that
        -- creature" is the attacker THIS blocker was declared against, not the
        -- first one nor the bearer. alice attacks with a 2/1 Piker and a 3/3 Hill
        -- Giant; the Sentry blocks the Giant.
        --
        -- The load-bearing reading is the Piker's: a binding taken off the wrong
        -- attacker kills it instead, and one that named the bearer kills nothing
        -- but the Sentry.
        Spec.it s "CR 509.3b that creature is the attacker the bearer blocked" $ do
          (gs, mine, theirs) <- board ["Goblin Piker", "Hill Giant"] ["Loyal Sentry"]
          case (mine, theirs) of
            ([piker, giant], [sentry]) -> do
              let struck = atDamageSecond gs
              Spec.assertEqWith
                s
                "the blocked Giant died, the unblocked Piker lived, and the Sentry died with it"
                (S.onBattlefield giant struck, S.onBattlefield piker struck, S.onBattlefield sentry struck)
                (False, True, False)
              Spec.assertEqWith s "and only the Piker's 2 reached bob" (S.lifeOf S.bob (S.runCombat blockSecond gs)) (Just 18)
            _ -> Spec.assertFailure s "fixture should give alice two attackers and bob one blocker"
        -- CR 509.3b's bearer is the BLOCKER. The same card attacking and becoming
        -- blocked matches nothing, which is what pins the arm's `blocker ==
        -- bearer` against reading the pair the other way round -- under that
        -- reading the attacking Sentry triggers and destroys itself in the
        -- declare blockers step.
        Spec.it s "CR 509.3b becoming blocked is not blocking" $ do
          (gs, mine, theirs) <- board ["Loyal Sentry"] ["Goblin Piker"]
          case (mine, theirs) of
            ([sentry], [piker]) -> do
              let struck = atDamage gs
              Spec.assertEqWith
                s
                "nothing triggered, so both are still there when damage is about to be dealt"
                (S.onBattlefield sentry struck, S.onBattlefield piker struck)
                (True, True)
              -- The positive control on the same pair of cards: with the Sentry
              -- blocking instead, both are gone by then.
              (blocking, otherPikers, otherSentries) <- board ["Goblin Piker"] ["Loyal Sentry"]
              let controlStruck = atDamage blocking
              Spec.assertEqWith
                s
                "the same two cards with the Sentry blocking do trigger"
                (fmap (`S.onBattlefield` controlStruck) (otherPikers <> otherSentries))
                [False, False]
            _ -> Spec.assertFailure s "fixture should give each seat one creature"

-- CR 509.3e's FILTERED forms, both halves of one printed sentence: "whenever
-- [a creature] blocks or becomes blocked by one or more [black] creatures". The
-- rule's last sentence covers "at least a certain number", and one is the number
-- every filtered printing states.
--
-- Serra Inquisitors {4}{W} Creature -- Human Cleric 3/3, "Whenever this creature
-- blocks or becomes blocked by one or more black creatures, this creature gets
-- +2/+0 until end of turn", is the card, and one CR 603.1b ability with two
-- conditions rather than two abilities. A creature cannot block and be blocked in
-- the same combat (CR 509.1a chooses from the DEFENDING player's creatures), so
-- at most one branch of the AnyOf can fire per combat.
--
-- Bog Wraith 3/3 is the black creature and Hill Giant 3/3 the control: same stats
-- and same seat, differing only in colour, so a leg that stops firing stopped on
-- the Filter. Every reading is taken at the COMBAT DAMAGE step, before damage is
-- dealt, so 3 -> 5 is the trigger's and never combat's.
selfBlocksOneOrMoreSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBlocksOneOrMoreSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Blocks EVERY attacker with `blocker` alone, which aggressiveAnswer cannot
      -- express: it puts every blocker on the first attacker.
      blockEverything :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      blockEverything blocker p = case p of
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "SelfBlocksOneOrMore" $ do
        -- The proving test for the BLOCKING half, and its control: two boards
        -- differing only in the attacker's colour.
        Spec.it s "CR 509.3e whole card: blocking a black creature is +2/+0, blocking a nonblack one is nothing" $ do
          (black, _, mine) <- board ["Bog Wraith"] ["Serra Inquisitors"]
          (other, _, theirs) <- board ["Hill Giant"] ["Serra Inquisitors"]
          case (mine, theirs) of
            ([blocking], [control]) ->
              Spec.assertEqWith
                s
                "5/3 against the Wraith, 3/3 against the Giant"
                (S.powerToughnessOf blocking (atDamage S.aggressiveAnswer black), S.powerToughnessOf control (atDamage S.aggressiveAnswer other))
                (Just (5, 3), Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give bob one Serra Inquisitors on each board"
        -- Rule 509.3e's "one or more" is ONE trigger however many admitted
        -- creatures were blocked. A High Ground gives bob the arity, the
        -- Inquisitors blocks both Wraiths, and the answer is 5/3 rather than 7/3.
        -- The falsifier is a match on the pairwise GameEvent.BlockerDeclared,
        -- which is CR 509.3b's arity: that fires twice.
        Spec.it s "CR 509.3e blocking TWO black creatures is +2/+0 once" $ do
          (gs, _, theirs) <- board ["Bog Wraith", "Bog Wraith"] ["Serra Inquisitors", "High Ground"]
          case theirs of
            [inquisitors, _] ->
              Spec.assertEqWith
                s
                "one pump, not two"
                (S.powerToughnessOf inquisitors (atDamage (blockEverything inquisitors) gs))
                (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give bob an Inquisitors and a High Ground"
        -- The ATTACKING half, and its control: the same pair of boards with the
        -- Inquisitors on alice's side, so the branch that fires is the other one.
        Spec.it s "CR 509.3e whole card: becoming blocked by a black creature is +2/+0, by a nonblack one is nothing" $ do
          (black, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith"]
          (other, theirs, _) <- board ["Serra Inquisitors"] ["Hill Giant"]
          case (mine, theirs) of
            ([attacking], [control]) ->
              Spec.assertEqWith
                s
                "5/3 blocked by the Wraith, 3/3 blocked by the Giant"
                (S.powerToughnessOf attacking (atDamage S.aggressiveAnswer black), S.powerToughnessOf control (atDamage S.aggressiveAnswer other))
                (Just (5, 3), Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors on each board"
        -- The attacking half's arity, which is what separates this condition from
        -- SelfBecomesBlockedBy: two Wraiths block the one Inquisitors, so two
        -- GameEvent.BlockerDeclared are recorded and exactly one
        -- GameEvent.AttackerBlocked. The falsifier is a match on the pairwise
        -- event: that fires twice, for 7/3.
        Spec.it s "CR 509.3e becoming blocked by TWO black creatures is +2/+0 once" $ do
          (gs, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith", "Bog Wraith"]
          case mine of
            [inquisitors] ->
              Spec.assertEqWith s "one pump, not two" (S.powerToughnessOf inquisitors (atDamage S.aggressiveAnswer gs)) (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors"
        -- "One or more" is a floor over the ADMITTED blockers, not a demand on all
        -- of them: a Wraith and a Giant block together and the trigger still
        -- fires. The falsifier is an `all` in place of the `any`, which answers
        -- 3/3 here while agreeing with every other case in this group.
        Spec.it s "CR 509.3e one admitted blocker among two is enough" $ do
          (gs, mine, _) <- board ["Serra Inquisitors"] ["Bog Wraith", "Hill Giant"]
          case mine of
            [inquisitors] ->
              Spec.assertEqWith s "the Wraith alone fires it" (S.powerToughnessOf inquisitors (atDamage S.aggressiveAnswer gs)) (Just (5, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Serra Inquisitors"
        -- CR 603.2: the condition is the BEARER's own block. A Goblin Piker blocks
        -- the Wraith while the Inquisitors stands by, so the declaration records a
        -- GameEvent.BlocksDeclared naming somebody else. A regression fence rather
        -- than a proof of one line: the arm's `blocker == bearer` and its
        -- Combat.blockers read each rule this board out on their own, so no single
        -- mutation turns it red.
        Spec.it s "CR 603.2 another creature's block does not pump the Inquisitors" $ do
          (gs, _, theirs) <- board ["Bog Wraith"] ["Serra Inquisitors", "Goblin Piker"]
          case theirs of
            [inquisitors, piker] ->
              Spec.assertEqWith
                s
                "the bystander stays 3/3"
                (S.powerToughnessOf inquisitors (atDamage (blockEverything piker) gs))
                (Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give bob an Inquisitors and a Piker"

-- CR 509.3c: "Whenever [a creature] becomes blocked, . . ." -- the ATTACKING
-- side of the same declaration selfBlocksSpec reads, matched against
-- GameEvent.AttackerBlocked.
--
-- Sacred Prey {G} Creature -- Horse 1/1, "Whenever this creature becomes blocked,
-- you gain 1 life", is the card: the cheapest producer in the pool, and its
-- payload names nothing about the blockers, so these cases isolate the
-- CONDITION. The gain lands on the ATTACKING seat (alice), which is the seat
-- combat damage never moves here, so every number below is the trigger's alone.
selfBecomesBlockedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfBecomesBlockedSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
   in Spec.describe s "SelfBecomesBlocked" $ do
        -- The proving test, and its control: the same game with CR 509.1's
        -- declaration switched off. 20 + 1 = 21 blocked, 20 declining -- and bob
        -- moves the other way, 20 blocked against 20 - 1 = 19 letting it through,
        -- so no single number can be read two ways.
        Spec.it s "CR 509.3c whole card: becoming blocked gains 1 life, going unblocked gains none" $ do
          (gs, mine, _) <- board ["Sacred Prey"] ["Goblin Piker"]
          let blocked = S.runCombat S.aggressiveAnswer gs
              unblocked = S.runCombat noBlocks gs
          case mine of
            [prey] -> do
              Spec.assertEqWith s "alice gained 1" (S.lifeOf S.alice blocked) (Just 21)
              Spec.assertEqWith s "and bob took nothing: the blocked Prey's 1 went to the Piker" (S.lifeOf S.bob blocked) (Just 20)
              Spec.assertBool s (not (S.onBattlefield prey blocked)) "the 1/1 Prey died to the Piker's 2, after its trigger had resolved"
              Spec.assertEqWith s "control leg: unblocked, so no gain" (S.lifeOf S.alice unblocked) (Just 20)
              Spec.assertEqWith s "and its 1 gets through" (S.lifeOf S.bob unblocked) (Just 19)
            _ -> Spec.assertFailure s "fixture should give alice one Sacred Prey"
        -- CR 509.3c's "only once each combat for that creature, even if it's
        -- blocked by multiple creatures". Two Pikers block the one Prey, so two
        -- GameEvent.BlockerDeclared are recorded and exactly one
        -- GameEvent.AttackerBlocked. The falsifier is a condition matched against
        -- the declaration's pairs instead: that fires twice, for 22.
        Spec.it s "CR 509.3c two blockers on one attacker still gain 1, not 2" $ do
          (gs, _, _) <- board ["Sacred Prey"] ["Goblin Piker", "Goblin Piker"]
          Spec.assertEqWith s "one gain of 1" (S.lifeOf S.alice (S.runCombat S.aggressiveAnswer gs)) (Just 21)
        -- CR 509.3a and CR 509.3c on one board, which is what tells the two arms
        -- apart: alice's Prey becomes blocked by bob's Guardian, so alice gains 1
        -- and bob gains 3 off a single declaration. Either arm reading the other's
        -- event moves one of those two numbers.
        Spec.it s "CR 509.3a and CR 509.3c fire on opposite sides of one declaration" $ do
          (gs, _, _) <- board ["Sacred Prey"] ["Pride Guardian"]
          let after = S.runCombat S.aggressiveAnswer gs
          Spec.assertEqWith s "the attacker's controller gained 1" (S.lifeOf S.alice after) (Just 21)
          Spec.assertEqWith s "the blocker's controller gained 3" (S.lifeOf S.bob after) (Just 23)
        -- The converse, and CR 509.3c's own words: a creature that BLOCKS does not
        -- become blocked. Here the Prey is bob's and blocking a Piker; the
        -- falsifier is an arm that matched GameEvent.BlockerDeclared, which would
        -- put bob at 21.
        Spec.it s "CR 509.3c blocking is not becoming blocked, so a blocking Sacred Prey gains nothing" $ do
          (gs, _, _) <- board ["Goblin Piker"] ["Sacred Prey"]
          Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob (S.runCombat S.aggressiveAnswer gs)) (Just 20)

-- CR 702.83a's exalted, which rule 702 states as a triggered
-- ability, and with it CR 506.5 -- "attacks alone", the one attack-trigger form
-- that is about the DECLARATION's size rather than about one creature.
--
-- Aven Squire {1}{W} Creature -- Bird Soldier 1/1 is the card: flying and
-- exalted, and flying decides nothing here, since every reading is taken before
-- damage whoever blocked. Hill Giant 3/3 is the creature it pumps, chosen so no
-- reading lands on the same pair -- 3/3 -> 4/4 is not 1/1 -> 2/2, and neither is
-- +2/+2's 5/5.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt, so
-- the pump is read directly rather than through what survives combat.
exaltedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
exaltedSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to one named creature. The whole point of
      -- the group: S.aggressiveAnswer attacks with EVERYTHING, which is the
      -- not-alone board rather than the alone one.
      only :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      only oid p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== oid) ids
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
   in Spec.describe s "Exalted" $ do
        -- The proving test, and the one that pins WHICH object the payload moves.
        -- alice's Aven Squire stays home while her Hill Giant attacks alone: the
        -- GIANT is 4/4 and the Squire is untouched. A payload written as prowess'
        -- Filter.IsSource would move the Squire's 1/1 and leave the Giant at 3/3,
        -- and a self-scoped condition would not fire at all -- one assertion over
        -- both, so neither can hide behind the other.
        Spec.it s "CR 702.83a whole card: the Squire stays home and the lone attacker is 4/4" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "the attacking Giant took the +1/+1 and the Squire took none"
                (S.powerToughnessOf giant (atDamage (only giant) gs), S.powerToughnessOf squire (atDamage (only giant) gs))
                (Just (4, 4), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- CR 506.5's "the ONLY creature declared as an attacker", which is the
        -- count on GameEvent.AttackerDeclared. The SAME board as above, differing
        -- only in the declaration: with the Squire attacking too, nobody is alone
        -- and neither creature is pumped.
        Spec.it s "CR 506.5 two attackers is nobody attacking alone" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [squire, giant] ->
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant (atDamage S.aggressiveAnswer gs), S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs))
                (Just (3, 3), Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- "A creature you control" reaches the bearer as readily as anything else:
        -- rule 702.83a excludes nothing, so an Aven Squire attacking by itself
        -- pumps itself to 2/2.
        Spec.it s "CR 702.83a the bearer attacking alone pumps itself" $ do
          (gs, mine, _) <- board ["Aven Squire"] []
          case mine of
            [squire] -> Spec.assertEqWith s "1/1 became 2/2" (S.powerToughnessOf squire (atDamage S.aggressiveAnswer gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Squire"
        -- "YOU control", read against CR 109.5's "you" -- the ability's controller
        -- (CR 603.3a). bob's Aven Squire watches alice's Giant attack alone and
        -- stays silent. Same declaration as the proving test, same Giant, and the
        -- only difference is which seat holds the Squire.
        Spec.it s "CR 702.83a an opponent's Squire does not pump the attacker" $ do
          (gs, mine, _) <- board ["Hill Giant"] ["Aven Squire"]
          case mine of
            [giant] -> Spec.assertEqWith s "the Giant is its printed 3/3" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (3, 3))
            _ -> Spec.assertFailure s "fixture should give alice one Giant"
        -- Two exalted permanents are two abilities and two +1/+1s, which is CR
        -- 603.2 rather than a clause of rule 702.83: unlike CR 702.28c's shadow,
        -- rule 702.83 prints no "multiple instances are redundant" sentence.
        Spec.it s "CR 603.2 two Squires make the lone attacker 5/5" $ do
          (gs, mine, _) <- board ["Aven Squire", "Aven Squire", "Hill Giant"] []
          case mine of
            [_, _, giant] -> Spec.assertEqWith s "3/3 took both pumps" (S.powerToughnessOf giant (atDamage (only giant) gs)) (Just (5, 5))
            _ -> Spec.assertFailure s "fixture should give alice two Squires and a Giant"
        -- CR 508.1a's declaration is a SET, so a broken interpreter naming one
        -- creature twice has still declared one attacker. Combat.declareAttackers
        -- deduplicates before it counts; without that the count would be 2, so
        -- the Giant would not be attacking alone and would go unpumped.
        Spec.it s "CR 508.1a a repeated id is still one attacker" $ do
          (gs, mine, _) <- board ["Aven Squire", "Hill Giant"] []
          case mine of
            [_, giant] -> do
              let twice :: Prompt.Prompt r -> r
                  twice p = case p of
                    Prompt.DeclareAttackers _ _ ids -> concatMap (\i -> if i == giant then [i, i] else []) ids
                    _ -> S.aggressiveAnswer p
              Spec.assertEqWith s "the Giant is 4/4 all the same" (S.powerToughnessOf giant (atDamage twice gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Squire and a Giant"
        -- The same multiplicity one permanent over, asserted of the MINT because
        -- no printing in the pool carries exalted twice -- as flanking's, bushido's
        -- and prowess' instance cases are.
        Spec.it s "CR 603.2 two instances mint two abilities, both CR 506.5" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Exalted 2
              expected =
                TriggerCondition.CreatureAttacksAlone
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 506.5, filtered on the attacker's controller" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.134a's mentor, which rule 702 states as a triggered ability
-- and the FIRST whose ability TARGETS -- so this is the group that runs a
-- keyword-minted TargetSpec through CR 601.2c's choosing, and with it
-- Filter.PowerLessThanSource, the one atom whose bound is the source's own power
-- rather than a literal.
--
-- Blade Instructor {2}{W} Creature -- Human Soldier 3/1 is the card: mentor and
-- nothing else, so every number below is the keyword's. Its fellow attackers are
-- the pool's vanillas, picked so no two readings land on the same pair -- a
-- mentored Goblin Piker is 3/2, which is neither its printed 2/1 nor the
-- Instructor's 3/1, and a mentored Icehide Golem is 3/3, which is neither.
--
-- Every reading is taken at the COMBAT DAMAGE step, after the trigger has
-- resolved in the declare attackers step and before damage is dealt, so the
-- counter is read directly rather than through what survives combat.
mentorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 603.3d's
      -- target named outright. S.aggressiveAnswer attacks with everything and
      -- Replay.defaultAnswer would take whichever target sorts first, so a case
      -- that is about WHICH creature has to say both itself.
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 3/2.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Mentor" $ do
        -- The proving test. Both attack, the Instructor mentors the smaller
        -- attacker, and the assertion covers all three things at once: the
        -- counter lands on the TARGET, the bearer takes none, and what landed is
        -- a CR 122.1a counter rather than a pump.
        Spec.it s "CR 702.134a whole card: the mentored attacker takes a +1/+1 counter" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] -> do
              let after = atDamage (plan [instructor, piker] piker) gs
              Spec.assertEqWith
                s
                "the Piker is 3/2 and the Instructor is untouched"
                (S.powerToughnessOf piker after, S.powerToughnessOf instructor after)
                (Just (3, 2), Just (3, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- "POWER LESS THAN this creature's power" is strict, so a 3/3 attacking
        -- beside a 3-power Instructor is no legal target -- and neither is the
        -- Instructor itself, which is why nothing at all is mentored here. Same
        -- declaration as the proving test; only the fellow attacker's power
        -- differs. S.aggressiveAnswer rather than `plan`, so that a filter that
        -- admitted the Giant would take the default target and go red.
        Spec.it s "CR 702.134a a creature whose power is not less is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Hill Giant"] []
          case mine of
            [instructor, giant] -> do
              let after = atDamage S.aggressiveAnswer gs
              Spec.assertEqWith
                s
                "both are at their printed sizes"
                (S.powerToughnessOf giant after, S.powerToughnessOf instructor after)
                (Just (3, 3), Just (3, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Giant"
        -- CR 508.1k's "attacking": the same Piker, small enough and on the same
        -- side, is no target while it stays home. The answerer aims at it anyway,
        -- so an ability that dropped the IsAttacking conjunct would mentor it.
        Spec.it s "CR 508.1k a creature that stayed home is no legal target" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker"] []
          case mine of
            [instructor, piker] ->
              Spec.assertEqWith
                s
                "the Piker is its printed 2/1"
                (S.powerToughnessOf piker (atDamage (plan [instructor] piker) gs))
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor and a Piker"
        -- CR 603.3d, which sends a trigger through CR 601.2c-d: with TWO smaller
        -- attackers the rules leave which one open,
        -- so the controller is asked and the answer is honoured. More candidates
        -- than the slot needs, so the prompt cannot be short-circuited away.
        Spec.it s "CR 603.3d the controller picks which smaller attacker is mentored" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem"] []
          case mine of
            [instructor, piker, golem] -> do
              let after = atDamage (plan [instructor, piker, golem] golem) gs
              Spec.assertEqWith
                s
                "the Golem took the counter and the Piker did not"
                (S.powerToughnessOf golem after, S.powerToughnessOf piker after)
                (Just (3, 3), Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker and a Golem"
        -- The bound is the SOURCE's power and not a number written into the
        -- ability: Hammer Dropper {2}{R}{W} Creature -- Giant Soldier 5/2 is the
        -- pool's other mentor, and the Hill Giant its 3-power sibling could not
        -- touch two cases up is a legal target for it -- same board, same
        -- declaration, only the mentor's power differs.
        Spec.it s "CR 702.134a a 5-power mentor reaches the 3/3 a 3-power one cannot" $ do
          (gs, mine, _) <- board ["Hammer Dropper", "Hill Giant"] []
          case mine of
            [dropper, giant] ->
              Spec.assertEqWith
                s
                "3 < 5, so the Giant is 4/4"
                (S.powerToughnessOf giant (atDamage (plan [dropper, giant] giant) gs))
                (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Dropper and a Giant"
        -- CR 608.2b re-checks the slot as the ability resolves, and rule 702.134a's
        -- comparison is part of what it re-checks. Two Instructors both aim at the
        -- 2/1 Piker; the first counter makes it 3/2, and 3 is no longer less than
        -- 3, so the second ability has no legal target and does not resolve. An
        -- engine that only checked at CR 601.2c would leave a 4/3.
        --
        -- That the second ability EXISTS is asserted at the mint below, not here:
        -- this board cannot tell a fizzled second trigger from a missing one.
        Spec.it s "CR 608.2b the second mentor's target is no longer legal" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Blade Instructor", "Goblin Piker"] []
          case mine of
            [first, second, piker] -> do
              let after = atDamage (plan [first, second, piker] piker) gs
              Spec.assertEqWith s "one counter landed" (S.powerToughnessOf piker after) (Just (3, 2))
              Spec.assertEqWith s "one, not two" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice two Instructors and a Piker"
        -- The same multiplicity asserted of the MINT, as exalted's and flanking's
        -- instance cases are, and with it the slot the gameplay cases above can
        -- only see through its effects: CR 508.3a's condition, and a spec whose
        -- filter is the rule's two printed narrowings.
        Spec.it s "CR 702.134b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Mentor 2
              expectedSpec =
                TargetSpec.required
                  Pool.Creatures
                  (Just (Filter.Type.And [Filter.Type.IsAttacking, Filter.Type.PowerLessThanSource]))
              specsOf ability = concatMap (Map.elems . Mode.targetSpecs) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 508.3a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.134a's one slot" (concatMap specsOf abilities) [expectedSpec, expectedSpec]

-- CR 702.134c, the OTHER half of rule 702.134: not mentor's own attack trigger but
-- an ability that watches a mentor ability RESOLVE. "An ability that triggers
-- whenever a creature mentors another creature triggers whenever a mentor ability
-- whose source is the first creature and whose target is the second creature
-- resolves", which is TriggerCondition.AttachedCreatureMentors read off
-- GameEvent.Mentored.
--
-- Aegis of the Legion {R}{W} Artifact -- Equipment is the card and the only printing
-- that reads rule 702.134c: "Equipped creature gets +1/+1 and has mentor. Whenever
-- equipped creature mentors a creature, put a shield counter on that creature. Equip
-- {3}". Every case below equips it by fixture (CR 301.5a's attachment as a state,
-- not the ability that makes it), so what is under test is the trigger rather than
-- CR 702.6a's equip.
--
-- Hill Giant 3/3 wears it, which makes it a 4/4 with mentor -- so no number here is
-- printed on any card in the board: the mentor's 4 is the Equipment's bonus, the
-- mentored Goblin Piker's 3/2 is its printed 2/1 plus rule 702.134a's counter, and 4
-- is not 3 is not 2. The Aegis itself is a fourth reading again, holding no counters
-- at all.
--
-- What separates "a creature MENTORED another" from "a creature WITH MENTOR
-- attacked" is the pair of declarations: the Giant attacking beside the Piker
-- mentors it, and the Giant attacking ALONE triggers rule 702.134a all the same and
-- mentors nothing, rule 702.134a's target having to be an attacking creature. The
-- two boards are the same board; only the attackers differ.
mentorsTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mentorsTriggerSpec s registry =
  let board mine = do
        ours <- mapM (S.printingOf s registry) mine
        pure (S.combatBoardOf ours [])
      plan :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan attackers target p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        _ -> S.aggressiveAnswer p
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 702.134c a creature mentoring another" $ do
        -- The proving test, and the whole of rule 702.134c in one board: the mentor
        -- ability resolves, and the ability watching it puts its counter on the
        -- creature that was MENTORED -- rule 702.134c's "second creature", which is
        -- neither the Aegis (the ability's own source) nor the Giant (the first
        -- creature). Rule 702.134a's +1/+1 counter sits beside it on the same
        -- permanent, so the two kinds are told apart rather than counted together.
        Spec.it s "CR 702.134c the mentored creature takes the shield counter" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "the Piker carries rule 702.134a's counter and rule 702.134c's"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 1), (CounterKind.Shield, 1)])
              Spec.assertEqWith
                s
                "and neither the mentor nor the Equipment carries either"
                (countersOn giant after, countersOn aegis after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "the equipped Giant is a 4/4 and the mentored Piker a 3/2"
                (S.powerToughnessOf giant after, S.powerToughnessOf piker after)
                (Just (4, 4), Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- The negative, and the case that makes the one above about MENTORING rather
        -- than about attacking: the same board, with the Piker held back. Rule
        -- 702.134a's ability still triggers -- the Giant attacked -- but rule
        -- 702.134a's target must be an attacking creature (CR 508.1k), so the
        -- ability has no legal target, never resolves, and rule 702.134c's event
        -- never happens. The answerer still aims at the Piker, so an engine that
        -- mentored a creature that stayed home would put both counters on it.
        Spec.it s "CR 702.134c attacking is not mentoring: nothing was mentored" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion"]
          case mine of
            [giant, piker, aegis] -> do
              let after = atDamage (plan [giant] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "no counters anywhere"
                (countersOn piker after, countersOn giant after)
                (Map.empty, Map.empty)
              Spec.assertEqWith
                s
                "and the Piker is its printed 2/1"
                (S.powerToughnessOf piker after)
                (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker and an Aegis"
        -- CR 122.6: BOTH counters go on through the placement funnel, so a CR 614.16
        -- replacement reaches them. Doubling Season ({5}{G}, "If an effect would put
        -- one or more counters on a permanent you control, it puts twice that many")
        -- doubles each, and 2 and 2 is a different reading from 1 and 1: rule
        -- 702.134a's counter would not double if the mentor opcode wrote it straight
        -- onto the permanent, and rule 702.134c's would not if the shield counter
        -- did.
        Spec.it s "CR 122.6 Doubling Season doubles both of them" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Doubling Season"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two of each"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 2)])
              Spec.assertEqWith s "so the Piker is a 4/3" (S.powerToughnessOf piker after) (Just (4, 3))
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Doubling Season"
        -- The same funnel narrowed to ONE of the two kinds, which is what tells the
        -- readings apart that Doubling Season above leaves symmetrical: Hardened
        -- Scales ({G}, "If one or more +1/+1 counters would be put on a creature you
        -- control, that many plus one are put instead") reaches rule 702.134a's
        -- counter and not rule 702.134c's, so the Piker ends on two +1/+1 counters
        -- and one shield counter -- a pair no other reading of this board produces.
        Spec.it s "CR 614.16 Hardened Scales reaches the +1/+1 counter alone" $ do
          (gs, mine, _) <- board ["Hill Giant", "Goblin Piker", "Aegis of the Legion", "Hardened Scales"]
          case mine of
            [giant, piker, aegis, _] -> do
              let after = atDamage (plan [giant, piker] piker) (S.attach aegis giant gs)
              Spec.assertEqWith
                s
                "two +1/+1 counters, one shield counter"
                (countersOn piker after)
                (Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.Shield, 1)])
            _ -> Spec.assertFailure s "fixture should give alice a Giant, a Piker, an Aegis and a Hardened Scales"
        -- CR 301.5f's "equipped creature", which is the whole of what the condition
        -- narrows by. A mentoring happens -- Blade Instructor's own printed mentor
        -- (CR 702.134a) puts its counter on the Piker -- and the Aegis, worn by an
        -- Icehide Golem that stayed home, is watching the wrong creature, so no
        -- shield counter is put. An engine that read the condition as "a creature
        -- mentors" rather than "equipped creature mentors" would fire here.
        Spec.it s "CR 702.134c another creature's mentoring is not the equipped creature's" $ do
          (gs, mine, _) <- board ["Blade Instructor", "Goblin Piker", "Icehide Golem", "Aegis of the Legion"]
          case mine of
            [instructor, piker, golem, aegis] -> do
              let after = atDamage (plan [instructor, piker] piker) (S.attach aegis golem gs)
              Spec.assertEqWith
                s
                "the Instructor's counter landed and no shield counter did"
                (countersOn piker after)
                (Map.singleton CounterKind.PlusOnePlusOne 1)
              Spec.assertEqWith
                s
                "nor anywhere else"
                (countersOn golem after, countersOn instructor after, countersOn aegis after)
                (Map.empty, Map.empty, Map.empty)
            _ -> Spec.assertFailure s "fixture should give alice an Instructor, a Piker, a Golem and an Aegis"

-- CR 702.149a's training, which rule 702 states as a triggered
-- ability -- and the first whose trigger CONDITION reads the rest of the
-- declaration, through Filter.PowerGreaterThanSource and the source power
-- TriggerCondition.SelfAttacksWithAnother supplies.
--
-- Apprentice Sharpshooter {2}{G} Creature -- Human Archer 1/4 is the card: reach
-- and training, and reach touches nothing here, so every number below is the
-- keyword's. Its 1 power is what the companions are measured against -- Goblin
-- Piker's 2 clears it, a second Sharpshooter's 1 does not.
--
-- Readings are taken at the DECLARE BLOCKERS step, one step after the trigger
-- resolves, so the counter is read before combat damage can move anything.
trainingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trainingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures. S.aggressiveAnswer
      -- attacks with everything, so a case about WHO attacks has to say it.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      -- CR 122.1: what is actually on the permanent, which a +1/+1 EFFECT would
      -- leave empty while reading the same 2/5.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "Training" $ do
        -- The proving test. Both attack, the Piker's 2 beats the Sharpshooter's 1,
        -- and the assertion covers all three things at once: the counter lands on
        -- the BEARER, the companion takes none, and what landed is a CR 122.1a
        -- counter rather than a pump.
        Spec.it s "CR 702.149a whole card: attacking beside a bigger creature trains" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Goblin Piker"] []
          case mine of
            [sharpshooter, piker] -> do
              let after = atBlockers (plan [sharpshooter, piker]) gs
              Spec.assertEqWith
                s
                "the Sharpshooter is 2/5 and the Piker is untouched"
                (S.powerToughnessOf sharpshooter after, S.powerToughnessOf piker after)
                (Just (2, 5), Just (2, 1))
              Spec.assertEqWith s "and it is a counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Piker"
        -- "POWER GREATER THAN this creature's power" is strict, so two 1-power
        -- Sharpshooters attacking together train neither. Same declaration shape
        -- as the proving test; only the companion's power differs.
        Spec.it s "CR 702.149a a companion whose power is only equal trains nobody" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Apprentice Sharpshooter"] []
          case mine of
            [first, second] -> do
              let after = atBlockers (plan [first, second]) gs
              Spec.assertEqWith
                s
                "both are at their printed size"
                (S.powerToughnessOf first after, S.powerToughnessOf second after)
                (Just (1, 4), Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice two Sharpshooters"
        -- CR 508.3a's "attack": the Hill Giant is bigger and on the same side, and
        -- it trains nothing while it stays home. The falsifier for a condition that
        -- swept the battlefield instead of the declaration.
        Spec.it s "CR 508.3a a bigger creature that stayed home is no companion" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Hill Giant"] []
          case mine of
            [sharpshooter, _] ->
              Spec.assertEqWith
                s
                "the Sharpshooter is its printed 1/4"
                (S.powerToughnessOf sharpshooter (atBlockers (plan [sharpshooter]) gs))
                (Just (1, 4))
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and a Giant"
        -- Two combat phases in one turn, which is what makes the COMBAT RECORD the
        -- right source and the event log the wrong one: the log keeps the whole
        -- turn's declarations, so a log-fold would find Aurelia in the
        -- second declaration she is not part of and train the Sharpshooter twice.
        -- Aurelia, the Warleader {2}{R}{R}{W}{W} 3/4 is the pool's extra-combat
        -- attacker, and her 3 power clears the Sharpshooter's 1 in the first phase.
        Spec.it s "CR 702.149a the added combat phase counts only its own declaration" $ do
          (gs, mine, _) <- board ["Apprentice Sharpshooter", "Aurelia, the Warleader"] []
          case mine of
            [sharpshooter, aurelia] -> do
              let first = atBlockers (plan [sharpshooter, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [sharpshooter]) first
                  after = atBlockers (plan [sharpshooter]) second
              Spec.assertEqWith s "one counter from the first declaration" (S.powerToughnessOf sharpshooter first) (Just (2, 5))
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and it added no second counter" (countersOn sharpshooter after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            _ -> Spec.assertFailure s "fixture should give alice a Sharpshooter and Aurelia"
        -- The same multiplicity asserted of the MINT, as mentor's and flanking's
        -- instance cases are: CR 702.149b says each instance triggers separately,
        -- and no card in the pool prints training twice.
        Spec.it s "CR 702.149b two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Training 2
              expected =
                TriggerCondition.SelfAttacksWithAnother
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.PowerGreaterThanSource])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 702.149a's declaration"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.147a's decayed: a combat restriction and a triggered ability that arms
-- a CR 603.7 DELAYED one -- the first minted ability to arm anything, and so the
-- first Effect.ArmDelayedTrigger whose name is on no face
-- (Keyword.mintedDelayedAbilities).
--
-- Falcon Abomination {2}{U} Creature -- Zombie Bird 2/2 is the producer: flying,
-- and "when this creature enters, create a 2/2 black Zombie creature token with
-- decayed". Decayed is printed on tokens far more often than on cards, so the
-- keyword arrives here through the card's own Create -- codec-parsed card data,
-- never a hand-built face -- and the Falcon beside it is the control, a creature
-- of the same size and controller with no decayed.
--
-- bob defends with NOTHING, deliberately: a blocked 2/2 token would die to CR
-- 704.5g whether or not rule 702.147a did anything, so the sacrifice assertion
-- would pass for the wrong reason.
decayedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
decayedSpec s registry =
  let settleFor gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 302.6: the token is minted this turn and so is summoning sick, and
      -- nothing in the pool gives a decayed token haste -- so this is the state a
      -- turn later would reach, and the one fixture step below that is not the
      -- card's own doing.
      settled oid gs =
        gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Settled S.alice}) oid (GameState.objects gs)}
      -- alice's Falcon Abomination, entered and its trigger resolved, with the
      -- Zombie token settled beside it.
      board = do
        falcon <- S.printingOf s registry "Falcon Abomination"
        let (gs0, _, _) = S.combatBoardOf [] []
            (bird, gs1) = S.entersWithTrigger falcon S.alice gs0
            made = resolveAll (settleFor gs1)
        pure (bird, made, S.tokensOf made)
      noAttacks :: Prompt.Prompt r -> r
      noAttacks p = case p of
        Prompt.DeclareAttackers {} -> []
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atMain = S.runToStep Phase.PostcombatMain
   in Spec.describe s "Decayed (CR 702.147)" $ do
        -- CR 509.1b through rule 702.147a's static half, unleash's carrier with
        -- no gate. The Falcon is on the same board with the same controller, so a
        -- restriction that stopped every creature blocking cannot pass.
        Spec.it s "CR 702.147a the token enters with decayed and cannot block" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Decayed zombie made) "the token has decayed"
              Spec.assertBool s (not (Combat.canBlock S.alice zombie made)) "so it cannot block"
              Spec.assertBool s (Combat.canBlock S.alice bird made) "while the Falcon that made it can"
              Spec.assertEqWith s "and only the Falcon is offered" (Combat.legalBlockers S.alice made) [bird]
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 508.3a's declaration puts the minted trigger on the stack, and its
        -- resolution arms rule 702's own delayed ability -- the only armed entry
        -- on the board, since no card here declares one.
        Spec.it s "CR 702.147a attacking arms one delayed ability" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atBlockers S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (elem zombie (S.attackerDeclarationsOf after)) "the token really attacked"
              Spec.assertEqWith s "one delayed ability waiting" (Seq.length (GameState.delayedTriggers after)) 1
              Spec.assertBool s (S.onBattlefield zombie after) "and it is still there at declare blockers"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 511.2: an ability that triggers "at end of combat" triggers as the
        -- end of combat step begins. The Falcon attacked too and survives, so
        -- "everything alice attacked with died" cannot pass this.
        Spec.it s "CR 511.2 the delayed ability sacrifices it at end of combat" $ do
          (bird, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain S.aggressiveAnswer (settled zombie made)
              Spec.assertBool s (not (S.onBattlefield zombie after)) "the token is gone"
              Spec.assertBool s (S.onBattlefield bird after) "while the Falcon that attacked beside it is still there"
              Spec.assertEqWith s "and the delayed ability is spent" (Seq.length (GameState.delayedTriggers after)) 0
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- THE PAIR THAT MAKES THE TRIGGER REAL. Same board, same fixture, and
        -- only the declaration different: rule 702.147a sacrifices a creature
        -- that ATTACKED, so a decayed creature held back survives its own end of
        -- combat. CR 508.8 skips the declare blockers and combat damage steps on
        -- this run, which the end of combat step is not among.
        Spec.it s "CR 702.147a a decayed creature that did not attack is not sacrificed" $ do
          (_, made, tokens) <- board
          case tokens of
            [zombie] -> do
              let after = atMain noAttacks (settled zombie made)
              Spec.assertEqWith s "nothing was declared" (S.attackerDeclarationsOf after) []
              Spec.assertEqWith s "so nothing was armed" (Seq.length (GameState.delayedTriggers after)) 0
              Spec.assertBool s (S.onBattlefield zombie after) "and the token is still there"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))

-- CR 603.2's "that player" reaching a TARGET SPEC rather than an effect's
-- operand: Trygon Predator's "whenever this creature deals combat damage to a
-- player, you may destroy target artifact or enchantment THAT PLAYER controls".
-- The slot is narrowed by Filter.ControlledByBound, baked to the damaged player
-- by Pawl.Engine.Filter.bakeBound at both of CR 115's moments -- CR 603.3d's
-- choosing and CR 608.2b's re-check.
--
-- THREE SEATS, and every seat holds the same permanent (Bad Moon), so the board
-- differs in exactly one thing: who controls it. A filter that read "an
-- opponent" would admit bob's, and one that dropped the controller conjunct
-- would admit alice's own; the answerer below takes the LOWEST-numbered legal
-- target of each slot, and alice's Moon is added before bob's and bob's before
-- carol's, so either mistake takes a different permanent rather than passing.
trygonPredatorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
trygonPredatorSpec s registry =
  let plan :: Prompt.Prompt r -> r
      plan p = case p of
        Prompt.ChooseDefender {} -> S.carol
        -- CR 603.5's printed "may", always exercised: a declined clause would
        -- destroy nothing, and this group is about which permanent it reaches.
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        Prompt.ChooseTargets _ _ _ asked -> fmap (maybe Set.empty Set.singleton . Set.lookupMin . snd) asked
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board = do
        trygon <- S.printingOf s registry "Trygon Predator"
        badMoon <- S.printingOf s registry "Bad Moon"
        let (gs0, mine, theirs, others) = S.threePlayerCombat [trygon, badMoon] [badMoon] [badMoon]
            -- S.threePlayerCombat starts at the beginning of combat, so the
            -- declarations are run as steps (which is what fills CR 508.5's
            -- defending player) and only the damage is dealt by hand -- that is
            -- the seam CR 603.3d's placement needs to be observable in, since a
            -- whole step would resolve the trigger as well.
            atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) plan gs0
            fought = S.runPure plan atDamage Damage.dealCombatDamage
        pure (mine, theirs, others, S.runPure plan fought Engine.settleForPriority)
   in Spec.describe s "TrygonPredator" $ do
        -- THE proving test. CR 603.3d picks the target as the ability is put on
        -- the stack, and the binding it stamps is read back here rather than
        -- inferred from what died -- so this says which permanent was OFFERED,
        -- not merely which one an effect happened to reach.
        Spec.it s "CR 603.3d the slot admits only the damaged player's permanent" $ do
          (mine, theirs, others, placed) <- board
          case (mine, theirs, others, GameState.stack placed) of
            ([_, alices], [bobs], [carols], [abilityId]) -> do
              let bindings = maybe Map.empty Object.bindings (Game.lookupObject abilityId placed)
                  slotOf name = Map.lookup (SlotName.MkSlotName (Text.pack name)) (Binding.targetsOf bindings)
              Spec.assertEqWith
                s
                "carol took the damage, so she is the player the event bound"
                (slotOf "thatPlayer")
                (Just (Set.singleton (Recipient.ToPlayer S.carol)))
              Spec.assertEqWith
                s
                "and carol's Bad Moon is the one target the slot admitted"
                (slotOf "target")
                (Just (Set.singleton (Recipient.ToObject carols)))
              Spec.assertBool s (alices /= bobs && bobs /= carols) "the three Moons are distinct objects"
            _ -> Spec.assertFailure s "fixture should give alice a Predator and a Moon, bob and carol a Moon each, and place one trigger"
        -- The same board run to the end: the ability resolves and destroys the
        -- one permanent, leaving both other seats' untouched. The whole card,
        -- CR 701.8a's destruction included.
        Spec.it s "CR 608.2c whole card: only carol's Bad Moon is destroyed" $ do
          (mine, theirs, others, placed) <- board
          let after = S.runPure plan placed Engine.priorityLoop
          case (mine, theirs, others) of
            ([_, alices], [bobs], [carols]) -> do
              Spec.assertBool s (not (S.onBattlefield carols after)) "carol's Moon is destroyed"
              Spec.assertBool s (S.onBattlefield bobs after) "bob's Moon is untouched"
              Spec.assertBool s (S.onBattlefield alices after) "and so is alice's own"
            _ -> Spec.assertFailure s "fixture should give each seat a Moon"
        -- CR 608.2b at the OTHER moment: the target changes hands after it was
        -- chosen, so it is no longer a permanent that player controls and the
        -- ability's only target is illegal. The pair differs in exactly the
        -- control change -- same board, same answers, same stack -- which is
        -- what makes the survival the rule's and not the fixture's.
        Spec.it s "CR 608.2b a target that changes hands is no longer that player's" $ do
          (_, _, others, placed) <- board
          case others of
            [carols] -> do
              let stolen = S.runPure plan (S.giveControl carols S.bob placed) Engine.priorityLoop
                  kept = S.runPure plan placed Engine.priorityLoop
              Spec.assertBool s (S.onBattlefield carols stolen) "bob controls it now, so the ability fizzles"
              Spec.assertBool s (not (S.onBattlefield carols kept)) "and without the change it is destroyed"
            _ -> Spec.assertFailure s "fixture should give carol a Moon"

-- CR 702.39a's provoke, which rule 702 states as a triggered
-- ability and the FIRST whose payload creates a CR 509.1c blocking REQUIREMENT
-- -- so this is the group that runs a resolution-created requirement through the
-- declare blockers step, and with it Filter.ControlledByDefendingPlayer.
--
-- Goblin Grappler {R} Creature -- Goblin 1/1 is the card: provoke and nothing
-- else, so every reading below is the keyword's. Its victims are the pool's
-- vanillas.
--
-- The answerer DECLINES to block throughout. That is what makes every positive
-- reading a claim about CR 509.1c: the block that happens is the one the rules
-- force, never one the interpreter asked for.
provokeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
provokeSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- Exercise or decline rule 702.39a's "may", aim its target, and never
      -- block voluntarily. S.aggressiveAnswer would block with everything and
      -- Script.declining would decline the "may", so a case about either has to
      -- say both itself.
      plan :: OptionalDecision.OptionalDecision -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      plan may target p = case p of
        Prompt.ChooseOptional {} -> may
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature target))) sets
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atBlockers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
      atDamage :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage)
      tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)
   in Spec.describe s "Provoke" $ do
        -- The proving test, and it covers both halves of rule 702.39a at once:
        -- bob's only creature is TAPPED, so CR 509.1a makes it no candidate at
        -- all until the untap, and the block that follows is CR 509.1c's
        -- requirement overriding an interpreter that declined to block.
        Spec.it s "CR 702.39a whole card: the provoked creature untaps and blocks" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Exercises piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "the Piker is blocking the Grappler" (Combat.blockersOf grappler after) (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- CR 603.5 / 608.2e: one printed "may" over one clause, so declining it
        -- withholds BOTH instructions. The same board and the same declining
        -- blocker answer as the proving test; only the answer to the "may"
        -- differs, which is what makes that test's block the keyword's.
        Spec.it s "CR 603.5 declining the may leaves the creature tapped and blocking nothing" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) -> do
              let after = atDamage (plan OptionalDecision.Declines piker) (S.tapObject piker gs0)
              Spec.assertEqWith s "nothing blocks" (Combat.blockersOf grappler after) Set.empty
              Spec.assertEqWith s "and it is still tapped" (tapStateOf piker after) (Just TapState.Tapped)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The REQUIREMENT alone, with the untap taken out of the picture: bob's
        -- creature is already untapped, so it could have blocked or not, and CR
        -- 509.1c is the only thing that makes declining illegal.
        Spec.it s "CR 509.1c an untapped provoked creature must block anyway" $ do
          (gs0, mine, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case (mine, theirs) of
            ([grappler], [piker]) ->
              Spec.assertEqWith
                s
                "the Piker is blocking"
                (Combat.blockersOf grappler (atDamage (plan OptionalDecision.Exercises piker) gs0))
                (Set.singleton piker)
            _ -> Spec.assertFailure s "fixture should give alice a Grappler and bob a Piker"
        -- The control for the case above, and the reason it is not vacuous: the
        -- same board with a provokeless attacker lets the declining answer
        -- stand. Goblin Piker {1}{R} 2/1 is the pool's vanilla, so the ONLY
        -- difference between the two boards is the keyword.
        Spec.it s "CR 509.1 the same board without provoke lets the defender decline" $ do
          (gs0, mine, theirs) <- board ["Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([attacker], [piker]) ->
              Spec.assertEqWith
                s
                "nothing blocks"
                (Combat.blockersOf attacker (atDamage (plan OptionalDecision.Exercises piker) gs0))
                Set.empty
            _ -> Spec.assertFailure s "fixture should give alice and bob a Piker each"
        -- CR 508.5 at THREE seats, where "defending player" and "an opponent"
        -- come apart: alice attacks carol, so bob's creature is an opponent's and
        -- is still no legal target. Asked of the slot itself rather than through
        -- a block, because a wrongly admitted target would be untapped and then
        -- pruned by CR 509.1b anyway -- the illegal CHOICE is the observable.
        --
        -- The slot is read off the MINTED ability rather than restated here, so
        -- this is a claim about what provoke writes and not about the atom alone.
        Spec.it s "CR 508.5 only the defending player's creature is a legal target" $ do
          grappler <- S.printingOf s registry "Goblin Grappler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, mine, theirs, others) = S.threePlayerCombat [grappler] [piker] [piker]
              minted = concatMap (concatMap (Map.elems . Mode.targetSpecs) . Modal.modes . TriggeredAbility.modal) (Keyword.abilitiesFor Keyword.Type.Provoke 1)
          case (mine, theirs, others, minted) of
            ([attacker], [bobs], [carols], [slot]) -> do
              let after = atBlockers (S.attackTo S.carol) gs0
              Spec.assertEqWith
                s
                "carol is the defending player, so only her creature"
                (Target.legalRecipients (Just S.alice) attacker slot after)
                (Set.singleton (Recipient.ToCreature carols))
              Spec.assertBool
                s
                (Set.notMember (Recipient.ToCreature bobs) (Target.legalRecipients (Just S.alice) attacker slot after))
                "bob is an opponent but not the defender"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 500.5a: "this combat" ends with the combat PHASE, so the stored
        -- requirement is swept before the postcombat main phase. Read off the
        -- store rather than through a block, there being no second declare
        -- blockers step in one combat phase to observe it in.
        Spec.it s "CR 500.5a the stored requirement lasts exactly the combat phase" $ do
          (gs0, _, theirs) <- board ["Goblin Grappler"] ["Goblin Piker"]
          case theirs of
            [piker] -> do
              let atBlock = atBlockers (plan OptionalDecision.Exercises piker) gs0
                  -- S.runToStep stops as soon as combat is left, so naming the
                  -- postcombat main phase runs the rest of the combat phase.
                  afterCombat = S.runToStep Phase.PostcombatMain (plan OptionalDecision.Exercises piker) atBlock
              Spec.assertEqWith s "stored while the ability has resolved" (length (GameState.blockRequirements atBlock)) 1
              Spec.assertEqWith s "and gone once the phase ends" (GameState.blockRequirements afterCombat) []
            _ -> Spec.assertFailure s "fixture should give bob a Piker"
        -- CR 702.39b's instances, asserted of the MINT as mentor's are, and with
        -- them the slot the gameplay cases can only see through its effects: CR
        -- 508.3a's condition, and rule 702.39a's one printed narrowing.
        Spec.it s "CR 702.39b two instances mint two targeting abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Provoke 2
              expectedSpec = TargetSpec.required Pool.Creatures (Just Filter.Type.ControlledByDefendingPlayer)
              specsOf ability = concatMap (Map.elems . Mode.targetSpecs) (Modal.modes (TriggeredAbility.modal ability))
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each on CR 508.3a's condition"
            (fmap TriggeredAbility.condition abilities)
            [TriggerCondition.SelfAttacks TriggerFrequency.EveryTime, TriggerCondition.SelfAttacks TriggerFrequency.EveryTime]
          Spec.assertEqWith s "each with rule 702.39a's one slot" (concatMap specsOf abilities) [expectedSpec, expectedSpec]

-- CR 702.112a's renown, which rule 702 states as a triggered
-- ability -- and the first minted one carrying an intervening "if" (CR 603.4),
-- which is the whole of why it fires once and not once per connection.
--
-- Rhox Maulers {4}{G} Creature -- Rhino Soldier 4/4 is the card: trample and
-- renown 2. The 2 is what separates "N counters" from "a counter"; the trample is
-- why the blocked case needs a blocker that absorbs all four damage (Apprentice
-- Sharpshooter, 1/4), since a smaller one would let renown's own event through.
--
-- Valeron Wardens {2}{G} Creature -- Human Monk 1/3 is the second card, and the
-- only printing that WATCHES the designation: renown 2 plus "whenever a creature
-- you control becomes renowned, draw a card" (CR 702.112b's marker read by
-- something other than renown itself).
--
-- CR 702.112b's "until it leaves the battlefield" is read on Object.newIncarnation
-- directly, below. Pawl.SetupSpec's "no per-incarnation state survives" case does
-- NOT cover it -- that case asks whether the forgetting is idempotent, which is
-- blind to a field it never touches.
-- CR 702.100: evolve, whose rule text IS a triggered
-- ability, and the first whose intervening "if" is about the EVENT's object
-- rather than its bearer -- so this is the group that runs a Condition reading
-- another object through Quantity.AgainstSlot at Binding.became, and the first
-- disjunction (Condition.Any) in the pool.
--
-- Cloudfin Raptor {U} Creature -- Bird Mutant 0/1 is the card: flying and evolve,
-- so every counter below is the keyword's. Its 0/1 body is what makes the two
-- halves of rule 702.100a's "and/or" separable at all, and each entrant is chosen
-- to satisfy exactly one of them:
--
--   * Goblin Piker 2/1 -- power only (2 > 0, and 1 is not > 1).
--   * Llanowar Augur 0/3 -- toughness only (0 is not > 0, and 3 > 1).
--   * Birds of Paradise 0/1 -- neither, which is what makes "greater" strict.
--
-- A test whose entrant beat the Raptor on both axes would pass whichever half
-- were implemented, and would not be a test of the disjunction at all.
--
-- The last two cases are CR 608.2a's re-check read against CR 608.2h, and they
-- are a pair: the same Piker leaves the battlefield before the trigger resolves
-- either way, and the only difference is the numbers the record filed for it --
-- its own 2/1 when damage killed it, 0/0 when a shrink did.
evolveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
evolveSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- CR 122.1: what is on the permanent, which a +1/+1 EFFECT would leave
      -- empty while reading the same size.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- A Raptor under alice, then one creature entering under `pid` with CR
      -- 603.6a's event, so the scan has something to match.
      board raptor printing pid =
        let (raptorId, gs1) = S.addCreature raptor S.alice (Setup.emptyGame S.bothPlayers)
            (enteringId, gs2) = S.entersWithTrigger printing pid gs1
         in (raptorId, enteringId, gs2)
      evolvesAgainst raptor printing = do
        entrant <- S.printingOf s registry printing
        let (raptorId, _, gs) = board raptor entrant S.alice
        pure (raptorId, resolveAll (settle gs))
   in Spec.describe s "Evolve" $ do
        -- The proving test, and rule 702.100a's POWER half alone: the Piker's 1
        -- toughness does not beat the Raptor's 1, so an implementation that read
        -- only toughness leaves this board untouched.
        Spec.it s "CR 702.100a whole card: a 2/1 entering beats the Raptor's power and evolves it" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Goblin Piker"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- Rule 702.100a's TOUGHNESS half alone, and the mirror of the case above:
        -- the Augur's 0 power does not beat the Raptor's 0, so an implementation
        -- that read only power leaves this board untouched.
        Spec.it s "CR 702.100a a 0/3 entering beats only the toughness, and evolves it all the same" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Llanowar Augur"
          Spec.assertEqWith s "one counter" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- "GREATER" is strict on both axes: a 0/1 entering ties the Raptor twice
        -- and evolves nothing. The falsifier for a comparison written as "at
        -- least".
        Spec.it s "CR 702.100a a 0/1 entering ties both axes and evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          (raptorId, after) <- evolvesAgainst raptor "Birds of Paradise"
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "it is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- "A creature YOU CONTROL": the same Piker that evolves the Raptor from
        -- alice's side does nothing from bob's, and nothing reaches the stack --
        -- CR 603.4 says an ability whose "if" is false does not trigger, but here
        -- it is the CONDITION that rejects the event.
        Spec.it s "CR 702.100a an opponent's creature entering is not a trigger at all" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.bob
              settled = settle gs
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no counters" (countersOn raptorId (resolveAll settled)) Map.empty
        -- CR 608.2a, the case that makes rule 702.100a's "if" an intervening one
        -- rather than part of the event: the trigger is on the stack legitimately,
        -- and a pump on the BEARER in response makes it resolve doing nothing. The
        -- proving test above is the control -- same board, same Piker.
        Spec.it s "CR 608.2a pumping the Raptor in response takes the counter away" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, _, gs) = board raptor piker S.alice
              onStack = settle gs
              responded = S.withEffect raptorId (Modification.ModifyPowerToughness (Quantity.Type.Literal 2) (Quantity.Type.Literal 2)) onStack
              after = resolveAll responded
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Raptor is a 2/3, which the Piker beats on neither axis" (S.powerToughnessOf raptorId responded) (Just (2, 3))
          Spec.assertEqWith s "so no counter on resolution" (countersOn raptorId after) Map.empty
        -- CR 608.2h, the other half of that re-check: the ENTRANT is killed while
        -- the trigger waits, and rule 702.100a's rulings say the comparison is
        -- made against the power and toughness it last had on the battlefield --
        -- not against an object with no characteristics. Only the RESOLUTION check
        -- can observe this: at gather time the entrant has just entered and is
        -- still there by construction, so the read this pins is Stack's alone.
        --
        -- LETHAL DAMAGE rather than a shrink is what kills it, and that is the
        -- whole design of the board: a shrink would change the very numbers under
        -- test, where damage leaves them alone (CR 704.5g destroys the Piker at
        -- the 2/1 the record files). So last known information answers TRUE here
        -- and a blank object answers False, which is what makes the two readings
        -- distinguishable at all.
        Spec.it s "CR 608.2h a Piker killed in response evolves the Raptor from its last known 2/1" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              dead = settle (S.markDamage pikerId 1 onStack)
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "and the Piker is gone before it resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "the counter goes on all the same" (countersOn raptorId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
          Spec.assertEqWith s "so it is a 1/2" (S.powerToughnessOf raptorId after) (Just (1, 2))
        -- The case above's paired negative -- same Raptor, same Piker, same
        -- departure before resolution, and the single difference is HOW it left:
        -- a -2/-1 kills it at 0/0 (CR 704.5f) instead, and 0/0 beats the Raptor's
        -- 0/1 on neither axis. So the numbers the record filed are what the
        -- re-check reads, rather than the entrant's PRINTED 2/1 -- which would put
        -- the counter on -- and rather than a departed entrant being waved through
        -- unexamined.
        Spec.it s "CR 608.2h an entrant shrunk to 0/0 as it died evolves nothing" $ do
          raptor <- S.printingOf s registry "Cloudfin Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, pikerId, gs) = board raptor piker S.alice
              onStack = settle gs
              shrunk = S.withEffect pikerId (Modification.ModifyPowerToughness (Quantity.Type.Literal (-2)) (Quantity.Type.Literal (-1))) onStack
              dead = settle shrunk
              after = resolveAll dead
          Spec.assertBool s (not (null (GameState.stack onStack))) "the trigger really was on the stack"
          Spec.assertEqWith s "the Piker left as a 0/0" (S.powerToughnessOf pikerId shrunk) (Just (0, 0))
          Spec.assertEqWith s "and is gone before the trigger resolves" (S.onBattlefield pikerId dead) False
          Spec.assertEqWith s "no counters" (countersOn raptorId after) Map.empty
          Spec.assertEqWith s "the Raptor is its printed 0/1" (S.powerToughnessOf raptorId after) (Just (0, 1))
        -- CR 702.100d: each instance triggers separately, asserted of the MINT as
        -- prowess' and training's are, no printing carrying evolve twice.
        Spec.it s "CR 702.100d two instances mint two abilities" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Evolve 2
              expected =
                TriggerCondition.PermanentEnters
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith
            s
            "each watching CR 603.6a's entry"
            (fmap TriggeredAbility.condition abilities)
            [expected, expected]

-- CR 702.100b: a creature "evolves" when one or more +1/+1 counters are put on it
-- as a result of its evolve ability RESOLVING -- the marker rule 702.100b makes
-- other abilities able to identify. Renegade Krasis {1}{G}{G} 3/2 is the card:
-- evolve, plus "whenever this creature evolves, put a +1/+1 counter on each other
-- creature you control with a +1/+1 counter on it".
--
-- Four permanents, each pinning one conjunct of that sentence:
--
--   * the Krasis itself -- "each OTHER", so its own count must stay at the one
--     its evolve put there.
--   * alice's Goblin Piker and Hill Giant, each seeded with a counter -- two
--     recipients, so "EACH other creature" is more than one object.
--   * alice's Birds of Paradise, with none -- "with a +1/+1 counter on it".
--   * bob's Piker, seeded with one -- "you control".
--
-- The ENTRANT is Llanowar Augur 0/3: it beats the Krasis' 2 toughness and nothing
-- else, so the Krasis evolves. Goblin Piker 2/1 is the entrant that does not,
-- which the self-scope case below turns on.
krasisSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
krasisSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      seeded printing pid gs =
        let (oid, g1) = S.addCreature printing pid gs
         in (oid, S.addCounter CounterKind.PlusOnePlusOne 1 oid g1)
      boardOn base = do
        krasisPrinting <- S.printingOf s registry "Renegade Krasis"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        birdsPrinting <- S.printingOf s registry "Birds of Paradise"
        giantPrinting <- S.printingOf s registry "Hill Giant"
        let (krasis, g1) = S.addCreature krasisPrinting S.alice base
            (mine, g2) = seeded pikerPrinting S.alice g1
            (giant, g3) = seeded giantPrinting S.alice g2
            (birds, g4) = S.addCreature birdsPrinting S.alice g3
            (theirs, g5) = seeded pikerPrinting S.bob g4
        pure (krasis, mine, giant, birds, theirs, g5)
      board = boardOn (Setup.emptyGame S.bothPlayers)
   in Spec.describe s "Renegade Krasis" $ do
        -- The proving test.
        Spec.it s "CR 702.100b whole card: the Krasis evolves and pays out its other counter-bearers" $ do
          augur <- S.printingOf s registry "Llanowar Augur"
          (krasis, mine, giant, birds, theirs, gs) <- board
          let (_, entered) = S.entersWithTrigger augur S.alice gs
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Krasis keeps only its evolve counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "alice's Piker gains a second" (plusOnes mine after) 2
          Spec.assertEqWith s "and so does her Giant -- EACH other creature" (plusOnes giant after) 2
          Spec.assertEqWith s "the counterless Bird gains none" (plusOnes birds after) 0
          Spec.assertEqWith s "bob's counter-bearer gains none" (plusOnes theirs after) 1
        -- Self-scoped, not filtered: a Cloudfin Raptor evolving beside the Krasis
        -- is another creature alice controls evolving, and the Krasis' ability
        -- says "this creature". The Piker 2/1 beats the Raptor's 0/1 power and
        -- neither of the Krasis' numbers, so exactly one of the two evolves.
        Spec.it s "CR 702.100b another creature evolving is not this creature evolving" $ do
          raptorPrinting <- S.printingOf s registry "Cloudfin Raptor"
          pikerPrinting <- S.printingOf s registry "Goblin Piker"
          (krasis, mine, _, _, _, gs) <- board
          let (raptor, withRaptor) = S.addCreature raptorPrinting S.alice gs
              (_, entered) = S.entersWithTrigger pikerPrinting S.alice withRaptor
              after = resolveAll (settle entered)
          Spec.assertEqWith s "the Raptor did evolve" (plusOnes raptor after) 1
          Spec.assertEqWith s "the Krasis did not" (plusOnes krasis after) 0
          Spec.assertEqWith s "so its trigger paid out nothing" (plusOnes mine after) 1
        -- CR 702.100b's "as a result of its evolve ability resolving": the same
        -- counter, on the same permanent, from Battlegrowth instead, is not an
        -- evolution. The falsifier for a condition written against
        -- GameEvent.CountersPut.
        Spec.it s "CR 702.100b a +1/+1 counter from anything else is not an evolution" $ do
          forest <- S.printingOf s registry "Forest"
          battlegrowth <- S.printingOf s registry "Battlegrowth"
          (krasis, mine, _, _, _, gs) <- boardOn (S.landsInPlay forest 1)
          let (handed, spellId) = S.handOne battlegrowth gs
              cast = snd (Engine.runGamePure (aimedCast spellId krasis) handed (S.cast S.alice spellId))
              after = resolveAll (settle cast)
          Spec.assertEqWith s "the Krasis took the counter" (plusOnes krasis after) 1
          Spec.assertEqWith s "and nothing evolved, so nothing was paid out" (plusOnes mine after) 1

renownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
renownSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- CR 508.1's declaration narrowed to the named creatures, trainingSpec's
      -- plan: S.aggressiveAnswer attacks with everything, so a case about who
      -- attacks in which phase has to say so.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      -- CR 122.1: what is actually on the permanent, which a +2/+2 EFFECT would
      -- leave empty while reading the same 6/6.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- CR 702.112b's designation itself, which no characteristic reports.
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
      -- CR 104.3c: a draw case needs a library to draw from, and more of one than
      -- it draws, so an extra draw is visible rather than fatal.
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      -- CR 509.1: no blocks. S.aggressiveAnswer blocks with everything, which
      -- would put the defender's own watcher in front of an attacker.
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Renown" $ do
        -- The proving test. CR 702.112a: two counters on the BEARER, and the
        -- designation with them. The counter assertion is what separates rule
        -- 702.112a's placement from a pump, and the 2 what separates N from 1.
        Spec.it s "CR 702.112a whole card: Rhox Maulers connects and takes two +1/+1 counters" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob took the printed four" (S.lifeOf S.bob after) (Just 16)
              Spec.assertEqWith s "two counters, not one" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "so it is a 6/6" (S.powerToughnessOf maulers after) (Just (6, 6))
              Spec.assertEqWith s "and it is renowned" (renownedness maulers after) (Just True)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112a is scoped to combat damage dealt TO A PLAYER. The 1/4
        -- absorbs all four (CR 702.19b leaves nothing to trample over), so the
        -- event never happens and neither half of the ability runs.
        Spec.it s "CR 702.112a a fully blocked Maulers is renowned by nobody" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] ["Apprentice Sharpshooter"]
          case mine of
            [maulers] -> do
              let after = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "bob lost no life" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "no counters" (countersOn maulers after) Map.empty
              Spec.assertEqWith s "and no designation" (renownedness maulers after) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 603.4's intervening "if", at the board level: a second connection in
        -- the same turn finds the creature already renowned, so nothing is added.
        -- Aurelia, the Warleader is the pool's extra combat phase, and she untaps
        -- the Maulers to attack again. The life drop is the discriminator -- it
        -- proves the second combat really connected, so a green assertion cannot
        -- mean the phase never ran.
        Spec.it s "CR 702.112a a second connection adds nothing, the creature being renowned" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader"] []
          case mine of
            [maulers, aurelia] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers]) first
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers]) second
              Spec.assertEqWith s "the first combat renowned it" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second really connected, for six" (S.lifeOf S.bob after) (Just 7)
              Spec.assertEqWith s "but added no third counter" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 2)
            _ -> Spec.assertFailure s "fixture should give alice a Maulers and Aurelia"
        -- What separates renown from a damage rider: it is a TRIGGERED ability, so
        -- the counters arrive when it resolves, not as the damage is dealt.
        -- S.fightWith deals combat damage without reaching a priority boundary,
        -- so nothing has been gathered yet -- poisonous' case, read on an object.
        Spec.it s "CR 702.112a the counters ride the stack, not the damage" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> do
              let fought = S.fightWith S.aggressiveAnswer gs
              Spec.assertEqWith s "damage is dealt" (S.lifeOf S.bob fought) (Just 16)
              Spec.assertEqWith s "but no counters until the trigger resolves" (countersOn maulers fought) Map.empty
              Spec.assertEqWith s "and no designation either" (renownedness maulers fought) (Just False)
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112b's "it stays renowned UNTIL IT LEAVES THE BATTLEFIELD": the
        -- designation is per-incarnation state, so CR 400.7's forgetting is what
        -- ends it and a Maulers that dies and returns must connect again.
        --
        -- Asserted on Object.newIncarnation directly, as Pawl.RoomSpec's unlocked
        -- designations are: nothing writes this field on an entry, so a bounce
        -- would read the same forgetting through more machinery. Pawl.SetupSpec's
        -- CR 400.7 case does NOT cover it -- `forgotten` asks whether the
        -- forgetting is idempotent, which is blind to a field it never touches.
        Spec.it s "CR 702.112b the designation does not survive CR 400.7" $ do
          (gs, mine, _) <- board ["Rhox Maulers"] []
          case mine of
            [maulers] -> case Game.lookupObject maulers (S.runCombat S.aggressiveAnswer gs) of
              Nothing -> Spec.assertFailure s "expected to find the Maulers"
              Just obj -> do
                Spec.assertEqWith s "the control: this incarnation is renowned" (Set.member Designation.Renowned (Object.designations obj)) True
                Spec.assertEqWith s "the next one is not" (Set.member Designation.Renowned (Object.designations (Object.newIncarnation obj))) False
            _ -> Spec.assertFailure s "fixture should give alice a Rhox Maulers"
        -- CR 702.112b's designation read by a WATCHER, which is what the rule
        -- calls it a marker FOR: Valeron Wardens {2}{G} Creature -- Human Monk
        -- 1/3, renown 2 and "whenever a creature you control becomes renowned,
        -- draw a card". Both attackers connect, so the Wardens' trigger fires
        -- TWICE -- once for the Maulers and once for itself, which is what "a
        -- creature you control" says and a self-scoped reading would not.
        --
        -- The library is stocked past the two draws, so a third draw would show as
        -- an extra card rather than as CR 104.3c losing alice the game before the
        -- assertions run.
        Spec.it s "CR 702.112b a watcher draws once per creature that becomes renowned" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Rhox Maulers"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, maulers] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "and so is the Maulers" (renownedness maulers after) (Just True)
              Spec.assertEqWith s "so two cards were drawn, not one" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "leaving one in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers"
        -- What the condition is NOT: combat damage. Goblin Piker connects for two
        -- and has no renown, so it never becomes renowned and contributes no draw
        -- -- the one card is the Wardens' own designation.
        Spec.it s "CR 702.112b a creature that connects without renown draws nothing" $ do
          (gs, mine, _) <- board ["Valeron Wardens", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          case mine of
            [wardens, goblin] -> do
              let after = S.runCombat S.aggressiveAnswer (stock piker 3 S.alice gs)
              Spec.assertEqWith s "both connected, for three" (S.lifeOf S.bob after) (Just 17)
              Spec.assertEqWith s "the Wardens is renowned" (renownedness wardens after) (Just True)
              Spec.assertEqWith s "the Piker is not" (renownedness goblin after) (Just False)
              Spec.assertEqWith s "so exactly one card was drawn" (length (Game.zoneMembers Zone.Hand S.alice after)) 1
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Piker"
        -- CR 109.5's "you control", and with it WHICH permanent the Filter reads:
        -- bob has a Valeron Wardens of his own, watching from the defending side.
        -- Nothing he controls becomes renowned, so he draws nothing -- an arm that
        -- read the BEARER instead of the event's subject would have his Wardens
        -- match itself and draw twice.
        Spec.it s "CR 702.112b the defender's own Wardens sees no creature of his become renowned" $ do
          (gs, mine, theirs) <- board ["Valeron Wardens", "Rhox Maulers"] ["Valeron Wardens"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([wardens, maulers], [hisWardens]) -> do
              let after = S.runCombat noBlocks (stock piker 3 S.bob (stock piker 3 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for five" (S.lifeOf S.bob after) (Just 15)
              Spec.assertEqWith s "hers are renowned" (fmap (`renownedness` after) [wardens, maulers]) [Just True, Just True]
              Spec.assertEqWith s "his is not" (renownedness hisWardens after) (Just False)
              Spec.assertEqWith s "she drew two" (length (Game.zoneMembers Zone.Hand S.alice after)) 2
              Spec.assertEqWith s "and he drew none" (length (Game.zoneMembers Zone.Hand S.bob after)) 0
            _ -> Spec.assertFailure s "fixture should give alice a Wardens and a Maulers, bob a Wardens"
        -- CR 702.112c: "if a creature has multiple instances of renown, each
        -- triggers separately". Asserted of the MINT, as poisonous' multiplicity
        -- is, no card in the pool printing renown twice. What rule 702.112c says
        -- happens NEXT -- the second resolving to nothing -- is the intervening
        -- "if" the gameplay cases above read.
        Spec.it s "CR 702.112c each instance of renown is its own ability" $ do
          Spec.assertEqWith s "renown 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 2) 2)) [Keyword.renown 2, Keyword.renown 2]
          Spec.assertEqWith s "and renown 6 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Renown 6) 1)) [Keyword.renown 6]

-- CR 701.37b's designation watched from outside the monstrosity action that sets
-- it: "monstrous is a designation ... that the monstrosity action and OTHER SPELLS
-- AND ABILITIES can identify", read through
-- TriggerCondition.PermanentBecomesDesignated -- the same condition Valeron
-- Wardens uses for renowned, with the other designation as its payload.
--
-- Arbor Colossus {2}{G}{G}{G} Creature -- Giant 6/6, "Reach. {3}{G}{G}{G}:
-- Monstrosity 3. When this creature becomes monstrous, destroy target creature
-- with flying an opponent controls."
--
-- bob holds Bird Maiden 1/2 flying and Goblin Piker 2/1: the Piker is the
-- falsifier for a target spec that dropped "with flying", and both are his, so no
-- assertion here turns on the seat.
--
-- TWELVE Forests, not six: the second-monstrosity case has to be able to PAY for
-- its activation, or it would prove nothing but an unpayable cost.
--
-- The DESIGNATION is what the last case turns on. Valeron Wardens watches the same
-- condition with Renowned, so a matcher that compared only the event's SHAPE would
-- draw alice a card when her Colossus became monstrous.
arborColossusSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
arborColossusSpec s registry =
  let monstrousness oid gs = fmap (Set.member Designation.Monstrous . Object.designations) (Game.lookupObject oid gs)
      plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- The trigger TARGETS (CR 603.3d), so the answerer has to aim it; `victim`
      -- pins the choice rather than searching for a legal one, which is what lets
      -- the Piker case below fail rather than repair itself.
      aimed victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just victim) . Recipient.objectOf) legal) sets
        _ -> S.identityAnswer p
      -- One activation of the one monstrosity ability, its trigger settled onto
      -- the stack and resolved, with `victim` aimed at.
      monstrosity colossus victim gs = case Activate.abilitiesFor colossus gs of
        [ability]
          -- CR 701.37a's condition is the CLAUSE's, not an activation
          -- restriction, so a monstrous permanent's ability stays activatable --
          -- which is what makes the second case below a real activation rather
          -- than an unpaid one.
          | Activate.activatable S.alice colossus ability gs ->
              Right . snd . Engine.runGamePure (aimed victim) gs $ do
                Activate.activateAbility S.alice colossus ability
                Stack.resolveTop
                Engine.settleForPriority
                Engine.priorityLoop
        [_] -> Left 0
        other -> Left (length other)
      board extra = do
        colossusPrinting <- S.printingOf s registry "Arbor Colossus"
        forest <- S.printingOf s registry "Forest"
        maidenPrinting <- S.printingOf s registry "Bird Maiden"
        pikerPrinting <- S.printingOf s registry "Goblin Piker"
        base <- extra (S.landsInPlay forest 12)
        let (colossus, g1) = S.addCreature colossusPrinting S.alice base
            (maiden, g2) = S.addCreature maidenPrinting S.bob g1
            (piker, g3) = S.addCreature pikerPrinting S.bob g2
        pure (colossus, maiden, piker, g3)
   in Spec.describe s "Arbor Colossus" $ do
        -- The proving test. CR 701.37a's counters and designation, and then rule
        -- 701.37b's marker read by an ability of the same permanent: the flier dies.
        Spec.it s "CR 701.37b whole card: monstrosity 3 marks the Colossus and its trigger destroys the flier" $ do
          (colossus, maiden, piker, gs) <- board pure
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "not monstrous to begin with" (monstrousness colossus gs) (Just False)
              Spec.assertEqWith s "three counters, not one" (plusOnes colossus after) 3
              Spec.assertEqWith s "so it is a 9/9" (S.powerToughnessOf colossus after) (Just (9, 9))
              Spec.assertEqWith s "and it is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "the targeted flier was destroyed"
              Spec.assertBool s (S.onBattlefield piker after) "and the ground creature was not"
        -- CR 701.37a's "if this permanent isn't monstrous": the SECOND activation
        -- on the same board does nothing, so the trigger never fires and bob's
        -- second flier lives. The same mana, the same seats, the same creatures --
        -- the one difference is that the Colossus is already monstrous.
        Spec.it s "CR 701.37a a second monstrosity marks nothing, so nothing triggers" $ do
          (colossus, maiden, _, gs) <- board pure
          maidenPrinting <- S.printingOf s registry "Bird Maiden"
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right once -> do
              let (second, withSecond) = S.addCreature maidenPrinting S.bob once
              case monstrosity colossus second withSecond of
                Left n -> Spec.assertFailure s ("expected the monstrous Colossus to stay activatable, got " <> show n)
                Right twice -> do
                  Spec.assertEqWith s "still three counters, not six" (plusOnes colossus twice) 3
                  Spec.assertBool s (S.onBattlefield second twice) "the second flier survived, nothing having become monstrous"
        -- The designation is LOAD-BEARING in the CLAUSE CONDITION too, and one board
        -- can carry two designations at once: Rune-Brand Juggler {2}{B}{R} 3/3,
        -- "When this creature enters, suspect up to one target creature you control",
        -- aimed at the Colossus. CR 701.60b's mark is not CR 701.37b's, so CR
        -- 701.37a's "if this permanent isn't monstrous" still holds and monstrosity
        -- still does its whole job. A Quantity arm that read "has SOME designation"
        -- would fail the condition and put nothing on the Colossus at all.
        Spec.it s "CR 701.37a a suspected Colossus is still not monstrous" $ do
          jugglerPrinting <- S.printingOf s registry "Rune-Brand Juggler"
          (colossus, maiden, _, gs) <- board pure
          let (_, entering) = S.entersWithTrigger jugglerPrinting S.alice gs
              suspected = snd (Engine.runGamePure (aimed colossus) entering Engine.priorityLoop)
          Spec.assertBool s (Set.member Designation.Suspected (maybe Set.empty Object.designations (Game.lookupObject colossus suspected))) "the Juggler suspected the Colossus"
          Spec.assertEqWith s "which leaves it not monstrous" (monstrousness colossus suspected) (Just False)
          case monstrosity colossus maiden suspected of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "so monstrosity still places its three counters" (plusOnes colossus after) 3
              Spec.assertEqWith s "and still marks it monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "and its trigger still fired"
        -- The designation is LOAD-BEARING in the match, not just the event's shape.
        -- Valeron Wardens {2}{G} 1/3 watches "whenever a creature you control
        -- becomes renowned" -- the same TriggerCondition constructor with Renowned
        -- in it -- and the Colossus becoming monstrous is not that. alice's library
        -- is stocked, so a spurious draw is visible rather than fatal (CR 104.3c).
        Spec.it s "CR 701.37b a creature becoming monstrous is not a creature becoming renowned" $ do
          wardensPrinting <- S.printingOf s registry "Valeron Wardens"
          piker <- S.printingOf s registry "Goblin Piker"
          (colossus, maiden, _, gs) <- board (\base -> pure (snd (S.addLibraryCard piker S.alice (snd (S.addCreature wardensPrinting S.alice base)))))
          case monstrosity colossus maiden gs of
            Left n -> Spec.assertFailure s ("expected one activatable monstrosity ability, got " <> show n)
            Right after -> do
              Spec.assertEqWith s "the Colossus is monstrous" (monstrousness colossus after) (Just True)
              Spec.assertBool s (not (S.onBattlefield maiden after)) "so its own trigger did fire"
              Spec.assertEqWith s "and the Wardens drew nothing" (length (Game.zoneMembers Zone.Hand S.alice after)) 0

-- CR 702.63 vanishing, which rule 702 states as triggered
-- abilities -- and the first whose rule text spans BOTH mints, since rule
-- 702.63a's three abilities are one CR 614.1c entry replacement
-- (Keyword.mintedReplacementsFor, riot's position) and two triggers.
--
-- Waning Wurm {3}{B} Creature -- Zombie Wurm 7/6 is the card, and it is nothing
-- but the keyword: no second ability can put a counter on it, take one off, or
-- keep it alive, so every number below is vanishing's own.
--
-- Vanishing 2 rather than a larger printing (Calciderm's 4) because two is the
-- smallest N that tells the two triggers apart: the first upkeep must remove one
-- and NOT sacrifice, the second must do both.
vanishingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
vanishingSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- One upkeep for `pid`, run to the end of the priority loop, so the
      -- trigger is gathered (CR 603.3) and resolved.
      upkeepOf pid gs =
        let began = Event.recordEvent (GameEvent.StepBegan upkeep pid) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = snd (Engine.runGamePure S.identityAnswer began Engine.settleForPriority)
         in (settled, snd (Engine.runGamePure S.identityAnswer settled Engine.priorityLoop))
      after pid gs = snd (upkeepOf pid gs)
      times = S.counterOf CounterKind.Time
      -- The wurm CAST rather than placed, because rule 702.63a's first ability is
      -- a replacement on the entry -- S.addCreature builds the object directly and
      -- so reaches no CR 616.1 loop, which is what the counterless case below
      -- turns on.
      castWurm = do
        swamp <- S.printingOf s registry "Swamp"
        wurm <- S.printingOf s registry "Waning Wurm"
        let base = S.landsInPlay swamp 4
            (held, gs0) = S.addHandCard wurm S.alice base
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
        pure (wurmOn entered, entered)
      wurmOn gs =
        let named oid = fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Waning Wurm"))
         in List.find named (Set.toList (GameState.battlefield gs))
   in Spec.describe s "Vanishing" $ do
        -- The proving test, and all three of rule 702.63a's abilities in one
        -- board: two counters on the entry, one removed at each of alice's
        -- upkeeps, and the sacrifice when the last one goes.
        Spec.it s "CR 702.63a whole card: the Wurm enters with two time counters and counts them down" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              Spec.assertEqWith s "two time counters on the entry" (times wurm entered) 2
              let first = after S.alice entered
              Spec.assertEqWith s "one after the first upkeep" (times wurm first) 1
              Spec.assertBool s (S.onBattlefield wurm first) "and it is still on the battlefield"
              let second = after S.alice first
              Spec.assertEqWith s "none after the second" (times wurm second) 0
              Spec.assertBool s (not (S.onBattlefield wurm second)) "so the last removal sacrificed it"
              -- CR 701.21a: a sacrifice is a move to the OWNER's graveyard, and
              -- not a destruction -- so this is the zone the wurm is in.
              Spec.assertEqWith s "in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice second)) 1
        -- Rule 702.63a says "YOUR upkeep", which is TurnScope.ControllersTurn: an
        -- opponent's upkeep is not this trigger, and an arm reading EachTurn would
        -- count the wurm down twice as fast.
        Spec.it s "CR 702.63a bob's upkeep removes nothing" $ do
          (found, entered) <- castWurm
          case found of
            Nothing -> Spec.assertFailure s "Waning Wurm did not reach the battlefield"
            Just wurm -> do
              let (settled, resolved) = upkeepOf S.bob entered
              Spec.assertEqWith s "nothing was even put on the stack" (GameState.stack settled) []
              Spec.assertEqWith s "so both counters are still there" (times wurm resolved) 2
              Spec.assertBool s (S.onBattlefield wurm resolved) "and the wurm is untouched"
        -- CR 603.4's intervening "if": rule 702.63a's second ability does not
        -- trigger AT ALL on an upkeep where the permanent has no time counter, so
        -- nothing reaches the stack. S.addCreature is what reaches this board --
        -- it places the wurm without running rule 702.63a's entry replacement, the
        -- position a card that lost its counters some other way would be in.
        --
        -- It also pins rule 702.63a's THIRD ability to the REMOVAL rather than to
        -- the count: a wurm sitting at zero is not sacrificed, because no last
        -- counter came off.
        Spec.it s "CR 603.4 a wurm with no time counters neither triggers nor is sacrificed" $ do
          wurm <- S.printingOf s registry "Waning Wurm"
          let (oid, gs) = S.addCreature wurm S.alice (Setup.emptyGame S.bothPlayers)
              (settled, resolved) = upkeepOf S.alice gs
          Spec.assertEqWith s "it really has none" (times oid gs) 0
          Spec.assertEqWith s "nothing on the stack" (GameState.stack settled) []
          Spec.assertBool s (S.onBattlefield oid resolved) "and it survives its own upkeep"
        -- CR 702.63c: "if a permanent has multiple instances of vanishing, each
        -- works separately". Asserted of BOTH mints, as renown's multiplicity is
        -- asserted of one, no card in the pool printing vanishing twice.
        --
        -- Spelled out rather than compared against Keyword.vanishing itself: an
        -- assertion written that way says only that two copies are two copies,
        -- and a mint that dropped one of the pair would repair it silently.
        Spec.it s "CR 702.63c each instance is its own three abilities" $ do
          let counted = TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.ControllersTurn
              emptied = TriggerCondition.SelfLastCounterRemoved CounterKind.Time
          Spec.assertEqWith
            s
            "vanishing 2 held twice mints four triggers, two of each kind"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Vanishing 2) 2)))
            [counted, emptied, counted, emptied]
          Spec.assertEqWith
            s
            "and two entry rewrites of two time counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Vanishing 2) 2)
            (replicate 2 (ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.WithCounters CounterKind.Time 2)))

-- CR 702.43 modular, whose rule text also spans BOTH of
-- Pawl.Engine.Keyword's mints -- one CR 614.1c entry replacement and one death
-- trigger. What is new is the trigger's PAYLOAD: rule 702.43a
-- counts "each +1/+1 counter on this permanent" at a moment when the permanent
-- is in a graveyard, so the number comes from CR 608.2h last known information.
-- counterLookBackSpec above proves the same record answering an intervening "if";
-- this is the first read of it at RESOLUTION.
--
-- Two printings, so no number below can be read two ways:
--
--   * Arcbound Hybrid {4} Artifact Creature -- Beast 0/0, haste and modular 2.
--   * Arcbound Worker {1} Artifact Creature -- Construct 0/0, modular 1.
--
-- The dying Hybrid is SEEDED to three counters against its printed modular 2,
-- which is the discriminator that matters: an implementation reading the
-- keyword's N instead of the counters on the permanent moves 2, and one reading a
-- literal moves 1. Only counting the pile moves 3.
--
-- Murder does the killing, counterLookBackSpec's reason: a 0/0 body plus counters
-- makes lethal damage a different number per leg, and CR 701.8a's destroy does
-- not care.
modularSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
modularSpec s registry =
  let plusOnes = S.counterOf CounterKind.PlusOnePlusOne
      -- Rule 702.43a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- A Hybrid seeded with three +1/+1 counters and one companion creature,
      -- with a Murder in hand. The Hybrid is added FIRST so it holds the lesser
      -- ObjectId: Murder's pool is Pool.Creatures and identityAnswer takes the
      -- least recipient, so this is what aims the removal at it rather than at
      -- the companion.
      board companion = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        hybrid <- S.printingOf s registry "Arcbound Hybrid"
        other <- S.printingOf s registry companion
        let lands = S.landsInPlay swamp 3
            (hybridId, g1) = S.addCreature hybrid S.alice lands
            g2 = S.addCounter CounterKind.PlusOnePlusOne 3 hybridId g1
            (otherId, g3) = S.addCreature other S.alice g2
            -- The companion carries a counter of its own, so a payload that
            -- overwrote rather than added would be visible, and so that a 0/0
            -- Worker survives CR 704.5f.
            g4 = S.addCounter CounterKind.PlusOnePlusOne 1 otherId g3
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 .. 5 :: Int]
        pure (hybridId, otherId, S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Hybrid dies), settle so the death
      -- trigger is gathered (CR 603.3), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      -- A printing CAST rather than placed, because rule 702.43a's first ability
      -- is a replacement on the ENTRY -- S.addCreature reaches no CR 616.1 loop.
      castOne name lands = do
        swamp <- S.printingOf s registry "Swamp"
        printing <- S.printingOf s registry name
        let (held, gs0) = S.addHandCard printing S.alice (S.landsInPlay swamp lands)
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            entered = S.runPure S.identityAnswer gs (S.cast S.alice held >> Stack.resolveTop)
            named oid = fmap Face.name (Game.faceOf oid entered) == Just (CardName.MkCardName (Text.pack name))
        pure (List.find named (Set.toList (GameState.battlefield entered)), entered)
   in Spec.describe s "Modular" $ do
        -- Rule 702.43a's FIRST ability, at both printed values: the N is the
        -- card's and not the rule's, so one leg alone could not tell a mint that
        -- always placed one counter from a correct one.
        Spec.it s "CR 702.43a the entry places the printed N of +1/+1 counters" $ do
          (foundWorker, workerBoard) <- castOne "Arcbound Worker" 1
          case foundWorker of
            Nothing -> Spec.assertFailure s "Arcbound Worker did not reach the battlefield"
            Just worker -> do
              Spec.assertEqWith s "modular 1 enters with one counter" (plusOnes worker workerBoard) 1
              -- CR 122.1a at layer 7c, which is also why a printed 0/0 survives
              -- CR 704.5f at all.
              Spec.assertEqWith s "so the printed 0/0 is a 1/1" (S.powerToughnessOf worker workerBoard) (Just (1, 1))
          (foundHybrid, hybridBoard) <- castOne "Arcbound Hybrid" 4
          case foundHybrid of
            Nothing -> Spec.assertFailure s "Arcbound Hybrid did not reach the battlefield"
            Just hybrid -> do
              Spec.assertEqWith s "modular 2 enters with two" (plusOnes hybrid hybridBoard) 2
              Spec.assertEqWith s "a 2/2" (S.powerToughnessOf hybrid hybridBoard) (Just (2, 2))
        -- The proving test. Rule 702.43a's SECOND ability, counting the pile the
        -- dead permanent had rather than its printed N.
        Spec.it s "CR 702.43a whole card: the dead Hybrid moves all three of its counters" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the Hybrid held three, not its printed two" (plusOnes hybridId (fst gs)) 3
          Spec.assertEqWith s "the Worker held one" (plusOnes workerId (fst gs)) 1
          Spec.assertEqWith s "the death trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield hybridId after)) "and the Hybrid is gone"
          -- CR 608.2h: four is one plus THREE, so the count came from the last
          -- known record. Two would be the printed N and one a literal.
          Spec.assertEqWith s "the Worker is up to four" (plusOnes workerId after) 4
          Spec.assertEqWith s "so it is a 4/4" (S.powerToughnessOf workerId after) (Just (4, 4))
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may leaves the counters nowhere" $ do
          (_, workerId, gs) <- board "Arcbound Worker"
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Worker is still on one" (plusOnes workerId after) 1
          Spec.assertEqWith s "a 1/1" (S.powerToughnessOf workerId after) (Just (1, 1))
        -- CR 608.2h in isolation, counterLookBackSpec's third case in the payload
        -- rather than in an intervening "if": the record is emptied while the
        -- trigger sits on the stack, which no rule can do to last known
        -- information -- so only a payload that really reads it notices.
        Spec.it s "CR 608.2h the count comes from the last known record, not from the board" $ do
          (hybridId, workerId, gs) <- board "Arcbound Worker"
          let (settled, _) = murderIt exercising gs
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust (\lk -> lk {LastKnown.counters = Map.empty}) hybridId (GameState.lastKnown settled)
                  }
              after = S.runPure exercising forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and resolved off it" (GameState.stack after) []
          Spec.assertEqWith s "and moved nothing, the record being empty" (plusOnes workerId after) 1
        -- "Target ARTIFACT creature": Goblin Piker 2/1 is a creature and not an
        -- artifact, so CR 603.3d finds no legal target and the ability never
        -- reaches the stack. The case above is the control -- the only difference
        -- between the two boards is which creature stands beside the Hybrid.
        Spec.it s "CR 702.43a a nonartifact creature is no target at all" $ do
          (_, pikerId, gs) <- board "Goblin Piker"
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and the Piker is still on the one it started with" (plusOnes pikerId after) 1
        -- CR 702.43b: each instance works separately. Asserted of BOTH mints,
        -- vanishing's position, no printing in the pool carrying modular twice.
        -- Spelled out rather than compared against Keyword.modular itself, for
        -- vanishingSpec's reason.
        Spec.it s "CR 702.43b each instance is its own two abilities" $ do
          Spec.assertEqWith
            s
            "modular 2 held twice mints two death triggers"
            (fmap TriggeredAbility.condition (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Modular 2) 2)))
            [TriggerCondition.SelfDies, TriggerCondition.SelfDies]
          Spec.assertEqWith
            s
            "and two entry rewrites of two counters each, which is what makes them add up"
            (Keyword.mintedReplacementsFor (Keyword.Type.Modular 2) 2)
            (replicate 2 (ReplacementEffect.EntryR Filter.Type.IsSource (EntryRewrite.WithCounters CounterKind.PlusOnePlusOne 2)))

-- CR 510.1b / 510.2's combat damage watched by a BYSTANDER rather than by the
-- creature that dealt it -- TriggerCondition.PermanentDealsCombatDamageToPlayer,
-- the filtered twin of poisonousSpec's SelfDealsCombatDamageToPlayer.
--
-- Tovolar, Dire Overlord {1}{R}{G} Legendary Creature -- Human Werewolf 3/3 is
-- the card: "whenever a Wolf or Werewolf you control deals combat damage to a
-- player, draw a card". Both faces print it; the back face's copy goes through
-- Pawl.CardSpec's corpus lints, but no case here reaches it -- that needs the CR
-- 731 transform Pawl.DaytimeSpec drives.
--
-- Tovolar is himself a Werewolf, so the filter admits the watcher: a self-scoped
-- reading would draw one card where these cases draw two. Russet Wolves (Wolf
-- 3/3) is the other subtype of the printed "or", and Goblin Piker (Goblin Warrior
-- 2/1) is the creature the filter must reject.
--
-- Every library is stocked past the draws, so an extra draw shows as an extra
-- card rather than as CR 104.3c ending the game before the assertions run.
tovolarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tovolarSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      stock printing n pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)
      -- CR 508.1's declaration narrowed to the named creatures, and CR 509.1's
      -- left empty: S.aggressiveAnswer attacks and blocks with everything, which
      -- a case about one attacker connecting cannot allow.
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      -- CR 508.1 / 509.1: one named attacker, met by one named blocker.
      oneOnOne :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
      oneOnOne attacker blocker p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers _ _ _ attackers -> Map.singleton blocker (Set.fromList attackers)
        _ -> S.aggressiveAnswer p
   in Spec.describe s "Filtered combat damage" $ do
        -- The proving test. Three unblocked attackers, two of which the filter
        -- admits: Tovolar for "Werewolf" and the Wolves for "Wolf". The Piker
        -- connects too and draws nothing, which is the filter doing its work
        -- inside the same event.
        Spec.it s "CR 510.2 a bystander draws once per Wolf or Werewolf that connects" $ do
          (gs, _, _) <- board ["Tovolar, Dire Overlord", "Russet Wolves", "Goblin Piker"] []
          piker <- S.printingOf s registry "Goblin Piker"
          let after = S.runCombat S.aggressiveAnswer (stock piker 4 S.alice gs)
          Spec.assertEqWith s "all three connected, for eight" (S.lifeOf S.bob after) (Just 12)
          Spec.assertEqWith s "so two cards were drawn, not three and not one" (handSize S.alice after) 2
          Spec.assertEqWith s "leaving two in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 2
        -- CR 510.1c: a BLOCKED creature assigns its combat damage to the blocker,
        -- so the Wolves deals its three to bob's Piker and the condition's
        -- player-recipient half rejects the event. The watcher is on the board and
        -- the damager is a Wolf she controls; only the recipient differs.
        Spec.it s "CR 510.1c combat damage dealt to a creature draws nothing" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Goblin Piker"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([_, wolves], [blocker]) -> do
              let after = S.runCombat (oneOnOne wolves blocker) (stock piker 4 S.alice gs)
              Spec.assertEqWith s "bob took none of it" (S.lifeOf S.bob after) (Just 20)
              Spec.assertEqWith s "and his Piker died for it" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "so no card was drawn" (handSize S.alice after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Piker"
        -- CR 109.5's "you control", which is what makes this a bystander's
        -- condition rather than the board's: bob has a Tovolar of his own,
        -- watching alice's two connect. He controls neither, so he draws nothing
        -- -- an arm that read the event's damager without the Filter's
        -- ControlledBy would have him draw twice.
        Spec.it s "CR 109.5 the defender's own Tovolar sees no Wolf of his connect" $ do
          (gs, mine, theirs) <- board ["Tovolar, Dire Overlord", "Russet Wolves"] ["Tovolar, Dire Overlord"]
          piker <- S.printingOf s registry "Goblin Piker"
          case (mine, theirs) of
            ([tovolar, wolves], [_]) -> do
              let after = S.runCombat (plan [tovolar, wolves]) (stock piker 4 S.bob (stock piker 4 S.alice gs))
              Spec.assertEqWith s "both of alice's connected, for six" (S.lifeOf S.bob after) (Just 14)
              Spec.assertEqWith s "she drew two" (handSize S.alice after) 2
              Spec.assertEqWith s "and he drew none" (handSize S.bob after) 0
            _ -> Spec.assertFailure s "fixture should give alice a Tovolar and a Wolves, bob a Tovolar"

-- What the filtered condition is FOR: a payload that aims at the creature that
-- dealt the damage (Pawl.Engine.Binding.combatDamager) rather than at the bearer
-- -- Aragorn, Hornburg Hero {1}{R}{G}{W} Legendary Creature -- Human Soldier 4/4,
-- "attacking creatures you control have first strike and renown 1" and "whenever
-- a renowned creature you control deals combat damage to a player, double the
-- number of +1/+1 counters on it".
--
-- Three capabilities meet here, and each has a way to fail that the counts below
-- tell apart: the slot naming the damager (aim it at the source and Aragorn takes
-- the counters), Quantity.AgainstSlot reading the damager's counters (read the
-- source's and the number is 0), and Filter.HasDesignation rejecting a candidate that
-- is not renowned yet (drop it and the Piker doubles too).
--
-- Aurelia, the Warleader supplies the second combat phase, as she does in
-- renownSpec: the doubling needs a creature that was ALREADY renowned when it
-- connected, and CR 603.2 checks this condition against the damage event itself,
-- where renown's own counters arrive only as ITS trigger resolves -- so one
-- connection can never both renown a creature and double it.
--
-- Aragorn arrives BETWEEN the two combats so the Maulers takes its two counters
-- from printed renown 2 alone: with him out on the first swing the Maulers would
-- hold renown 2 and a granted renown 1 at once, and CR 702.112c leaves which
-- resolves first to its controller.
aragornSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aragornSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      plan :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
      plan attackers p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (`elem` attackers) ids
        _ -> S.aggressiveAnswer p
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      renownedness oid gs = fmap (Set.member Designation.Renowned . Object.designations) (Game.lookupObject oid gs)
   in Spec.describe s "Doubling a damager's counters" $ do
        -- The proving test. 2 -> 4 rather than 2 -> 3, which is what separates
        -- "double" from "add one", and 4 rather than 2, which is what separates
        -- reading the damager's counters from reading the bearer's.
        Spec.it s "CR 702.112b whole card: a renowned creature's counters double when it connects" $ do
          (gs, mine, _) <- board ["Rhox Maulers", "Aurelia, the Warleader", "Goblin Piker"] []
          aragorn <- S.printingOf s registry "Aragorn, Hornburg Hero"
          case mine of
            [maulers, aurelia, piker] -> do
              let first = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, aurelia]) gs
                  (hero, staged) = S.addCreature aragorn S.alice first
                  loaded = S.addCounter CounterKind.PlusOnePlusOne 3 piker staged
                  second = S.runToStep (Phase.Combat CombatStep.DeclareAttackers) (plan [maulers, piker]) loaded
                  after = S.runToStep (Phase.Combat CombatStep.EndOfCombat) (plan [maulers, piker]) second
              Spec.assertEqWith s "the first combat connected for seven" (S.lifeOf S.bob first) (Just 13)
              Spec.assertEqWith s "renown 2 alone renowned the Maulers" (countersOn maulers first) (Map.singleton CounterKind.PlusOnePlusOne 2)
              Spec.assertEqWith s "the second phase really ran a declaration" (GameState.phase second) (Phase.Combat CombatStep.DeclareAttackers)
              Spec.assertEqWith s "and the second combat connected for eleven" (S.lifeOf S.bob after) (Just 2)
              Spec.assertEqWith s "so the Maulers doubled to four, not three" (countersOn maulers after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              Spec.assertEqWith s "making it an 8/8" (S.powerToughnessOf maulers after) (Just (8, 8))
              -- CR 702.112b's designation read of a CANDIDATE: the Piker carries
              -- three counters from outside renown and was NOT renowned when it
              -- dealt its damage, so Aragorn's ability never triggered for it. The
              -- fourth counter is the renown 1 he granted it, which becomes
              -- renowned only as that trigger resolves -- 4 rather than the 7 a
              -- filter without the designation conjunct would leave.
              Spec.assertEqWith s "the Piker was renowned by that same damage" (renownedness piker after) (Just True)
              Spec.assertEqWith s "but took one counter rather than doubling" (countersOn piker after) (Map.singleton CounterKind.PlusOnePlusOne 4)
              -- The slot: had the payload aimed at CR 113.7a's source, these
              -- counters would be here instead.
              Spec.assertEqWith s "and Aragorn himself took none" (countersOn hero after) Map.empty
            _ -> Spec.assertFailure s "fixture should give alice a Maulers, an Aurelia and a Piker"
        -- The other half of the same static ability, which the case above only
        -- passes through: CR 702.7b's first strike, granted to an ATTACKING
        -- creature. Two identical 2/1s meet, and only the attacker's controller
        -- has an Aragorn -- so the blocker is dead before it assigns (CR 510.4),
        -- where without the grant both would die.
        Spec.it s "CR 702.7b the same static grants first strike to attackers" $ do
          (gs, mine, theirs) <- board ["Aragorn, Hornburg Hero", "Goblin Piker"] ["Goblin Piker"]
          case (mine, theirs) of
            ([_, piker], [blocker]) -> do
              let after = S.runCombat (plan [piker]) gs
              Spec.assertEqWith s "the blocker is dead" (Game.lookupObject blocker after) Nothing
              Spec.assertEqWith s "the attacker survived, unrenowned" (renownedness piker after) (Just False)
              Spec.assertEqWith s "alice keeps both creatures" (S.creaturesInPlay S.alice after) 2
              Spec.assertEqWith s "bob none" (S.creaturesInPlay S.bob after) 0
              Spec.assertEqWith s "and nothing reached bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice an Aragorn and a Piker, bob a Piker"

-- CR 702.25a's flanking, which rule 702 states as a triggered
-- ability, and with it CR 509.3d -- "becomes blocked by a creature", the one
-- block-trigger form that fires once per BLOCKER and names it.
--
-- Benalish Cavalry {1}{W} Creature -- Human Knight 2/2 is the card: flanking and
-- nothing else, so every number below is the keyword's. Its blockers are drawn
-- from the pool's vanilla creatures for the same reason.
--
-- Every reading is taken at the COMBAT DAMAGE step, before damage is dealt (CR
-- 509.2a puts these triggers on the stack in the declare blockers step), so the
-- -1/-1 is read directly rather than through what survives combat.
flankingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flankingSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Flanking" $ do
        -- The proving test, and its control: the same 2/2 attacker WITHOUT
        -- flanking (Icehide Golem) against the same 2/1 blocker. The flanker's
        -- Piker is 1/0 and already dead when damage would be dealt, so the
        -- flanker survives; the Golem trades with it.
        Spec.it s "CR 702.25a whole card: the blocking Piker is -1/-1 and dies before damage" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker"]
          (control, controlMine, controlTheirs) <- board ["Icehide Golem"] ["Goblin Piker"]
          case (mine, theirs, controlMine, controlTheirs) of
            ([cavalry], [piker], [golem], [otherPiker]) -> do
              let struck = atDamage gs
                  traded = S.runCombat S.aggressiveAnswer gs
                  controlStruck = atDamage control
                  controlTraded = S.runCombat S.aggressiveAnswer control
              Spec.assertBool s (not (S.onBattlefield piker struck)) "the 2/1 Piker went to 1/0 and CR 704.5f buried it"
              Spec.assertEqWith s "the Cavalry itself is untouched" (S.powerToughnessOf cavalry struck) (Just (2, 2))
              Spec.assertBool s (S.onBattlefield cavalry traded) "so nothing was left to deal it damage"
              Spec.assertEqWith s "control leg: a 2/2 without flanking leaves the Piker at 2/1" (S.powerToughnessOf otherPiker controlStruck) (Just (2, 1))
              Spec.assertBool s (not (S.onBattlefield golem controlTraded)) "and the Piker's 2 kills it"
              Spec.assertBool s (not (S.onBattlefield otherPiker controlTraded)) "both die, where the flanker died alone"
            _ -> Spec.assertFailure s "fixture should give each seat one creature"
        -- CR 509.3d's arity, and the whole difference from CR 509.3c: "triggers
        -- once for each creature that blocks the specified creature". Two
        -- blockers, two triggers, and each -1/-1 lands on its OWN blocker.
        --
        -- The Hill Giant is the load-bearing reading: a condition matched against
        -- the GROUPED GameEvent.AttackerBlocked fires once and leaves it 3/3,
        -- and a binding that named the bearer instead moves the Cavalry's own
        -- 2/2.
        Spec.it s "CR 509.3d two blockers are two triggers, each on its own blocker" $ do
          (gs, mine, theirs) <- board ["Benalish Cavalry"] ["Goblin Piker", "Hill Giant"]
          case (mine, theirs) of
            ([cavalry], [piker, giant]) -> do
              let struck = atDamage gs
              -- One assertion over all three readings, so a mutation cannot hide
              -- behind whichever of them is checked first.
              Spec.assertEqWith
                s
                "the 3/3 Giant is 2/2, the 2/1 Piker is gone, and the Cavalry took neither -1/-1"
                (S.powerToughnessOf giant struck, S.onBattlefield piker struck, S.powerToughnessOf cavalry struck)
                (Just (2, 2), False, Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give bob two blockers"
        -- CR 702.25a's "without flanking", read as CR 509.3f asks -- the blocker's
        -- characteristics as it becomes a blocking creature. A second Benalish
        -- Cavalry blocking is 2/2 still; the Icehide Golem, the same 2/2 without
        -- the keyword, is 1/1. The two boards differ in nothing else.
        Spec.it s "CR 702.25a a blocker WITH flanking is spared and one without is not" $ do
          (withIt, _, theirs) <- board ["Benalish Cavalry"] ["Benalish Cavalry"]
          (without, _, others) <- board ["Benalish Cavalry"] ["Icehide Golem"]
          case (theirs, others) of
            ([blockingCavalry], [golem]) -> do
              Spec.assertEqWith s "the flanking blocker is untouched" (S.powerToughnessOf blockingCavalry (atDamage withIt)) (Just (2, 2))
              Spec.assertEqWith s "the one without takes -1/-1" (S.powerToughnessOf golem (atDamage without)) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give bob one blocker on each board"
        -- CR 702.25b: each instance triggers separately, which is abilitiesFor's
        -- replicate. No card in the pool prints flanking twice, so this is
        -- asserted of the MINT rather than of a board -- as bushido's, prowess'
        -- and battle cry's are.
        Spec.it s "CR 702.25b two instances mint two abilities, both CR 509.3d" $ do
          let abilities = Keyword.abilitiesFor Keyword.Type.Flanking 2
              expected =
                TriggerCondition.SelfBecomesBlockedBy
                  (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.Not (Filter.Type.HasKeyword Keyword.Type.Flanking)])
          Spec.assertEqWith s "two abilities" (length abilities) 2
          Spec.assertEqWith s "each watching CR 509.3d, filtered on the blocker's own flanking" (fmap TriggeredAbility.condition abilities) [expected, expected]

-- CR 702.45a: "'Bushido N' means 'Whenever this creature blocks or becomes
-- blocked, it gets +N/+N until end of turn.'" Rule 702 states it as a triggered
-- ability, as it does CR 702.70a, CR 702.86a, CR
-- 702.91a and CR 702.108a, and it is the only one that names TWO events: "blocks" is
-- CR 509.3a and "becomes blocked" is CR 509.3c, so Pawl.Engine.Keyword.bushido
-- mints two abilities and the two cases below fire one each.
--
-- Inner-Chamber Guard, {1}{W} Creature -- Human Samurai 0/2 with bushido 2 and
-- nothing else. Chosen for its numbers: 0/2 becoming 2/4 is unmistakable, an
-- asymmetric base means no reading of the rule lands on the same pair, and
-- bushido 2 rather than 1 keeps +N/+N apart from a hardcoded +1/+1. Goblin Piker
-- 2/1 is the other side, and the two flip TOGETHER on the pump: at 2/4 the Guard
-- kills the Piker and lives, at 0/2 it kills nothing and dies. Those two survival
-- assertions are regression fences rather than proofs -- every mutation tried
-- against this group tripped the power/toughness assertion above them first --
-- and what they fence is the TIMING: a pump that landed after CR 510's damage
-- would leave both creatures where an unpumped Guard does.
bushidoSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bushidoSpec s registry =
  let noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      -- The board as combat damage is about to be dealt, so the pump is readable
      -- before it decides anything.
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Bushido" $ do
        -- CR 509.3a's half, whole card: bob's Guard blocks alice's Piker.
        Spec.it s "CR 702.45a whole card: blocking makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case (attackers, blockers) of
            ([piker], [guard]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "0/2 before blockers are declared" (S.powerToughnessOf guard gs) (Just (0, 2))
              Spec.assertEqWith s "and 2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Piker and bob a Guard"
        -- The control leg for the case above, on the same board: nothing blocks,
        -- so CR 509.3a's event never happens and the Guard stays 0/2. Without it
        -- an ability that pumped on any combat event at all would pass.
        Spec.it s "CR 509.3a a Guard that does not block is not pumped" $ do
          (gs, _, blockers) <- board ["Goblin Piker"] ["Inner-Chamber Guard"]
          case blockers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and the unblocked Piker's 2 reached bob" (S.lifeOf S.bob after) (Just 18)
            _ -> Spec.assertFailure s "fixture should give bob a Guard"
        -- CR 509.3c's half, the other arm of the same printed sentence: now the
        -- Guard is alice's and attacks, and bob's Piker blocks it. An
        -- implementation with only the CR 509.3a arm passes every case above and
        -- fails this one.
        Spec.it s "CR 702.45a whole card: becoming blocked makes Inner-Chamber Guard 2/4" $ do
          (gs, attackers, blockers) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case (attackers, blockers) of
            ([guard], [piker]) -> do
              let pumped = atDamage gs
                  fought = S.runCombat S.aggressiveAnswer gs
              Spec.assertEqWith s "2/4 once the trigger has resolved" (S.powerToughnessOf guard pumped) (Just (2, 4))
              Spec.assertBool s (not (S.onBattlefield piker fought)) "the pumped Guard's 2 killed the 2/1 Piker"
              Spec.assertBool s (S.onBattlefield guard fought) "and its 4 toughness survived the Piker's 2"
            _ -> Spec.assertFailure s "fixture should give alice a Guard and bob a Piker"
        -- The control leg for CR 509.3c, on the same board as the case above:
        -- attacking is not becoming blocked, so an unblocked Guard stays 0/2 and
        -- takes nothing from bob.
        Spec.it s "CR 509.3c a Guard that goes unblocked is not pumped" $ do
          (gs, attackers, _) <- board ["Inner-Chamber Guard"] ["Goblin Piker"]
          case attackers of
            [guard] -> do
              let after = S.runCombat noBlocks gs
              Spec.assertEqWith s "still 0/2" (S.powerToughnessOf guard after) (Just (0, 2))
              Spec.assertEqWith s "and 0 power took nothing from bob" (S.lifeOf S.bob after) (Just 20)
            _ -> Spec.assertFailure s "fixture should give alice a Guard"
        -- CR 702.45b: "If a creature has multiple instances of bushido, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- prowess' and battle cry's are: no card in this pool prints bushido twice
        -- and nothing here grants it. The count is FOUR rather than two because
        -- one instance is already two abilities -- rule 702.45a's one sentence,
        -- CR 509.3a's event and CR 509.3c's.
        Spec.it s "CR 702.45b each instance of bushido is its own ability" $ do
          Spec.assertEqWith s "bushido 2 held once is its two halves" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 1)) (Keyword.bushido 2)
          Spec.assertEqWith s "and held twice is four abilities" (length (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Bushido 2) 2))) 4

-- CR 702.130a: "'Afflict N' means 'Whenever this creature becomes blocked,
-- defending player loses N life.'" Rule 702 states it as a triggered ability,
-- and it is the first to put CR 509.3c's event and CR
-- 508.5's defending player in one sentence.
--
-- Khenra Eternal {1}{B} Creature -- Zombie Jackal Warrior 2/2 with afflict 1 and
-- nothing else printed on it, so every number below is the keyword's.
--
-- THREE SEATS, for annihilatorSpec's reason: at two players "the defending
-- player" and "the attacker's one opponent" are the same seat.
--
-- Afflict 1 is the only N a card in this pool puts on a board, so no case below
-- can tell the keyword's N from a hardcoded 1. The mint inequality in the last
-- case is what does, and it is there for that and no other reason.
afflictSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afflictSpec s registry =
  let -- Attacks `who` with everything and lets them block with everything.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      -- The same, with CR 509.1's declaration switched off -- the control leg, and
      -- the only difference between the two answerers.
      unblocked :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      unblocked who p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> attacking who p
      -- alice fields the Khenra; bob and carol each field a Goblin Piker, so
      -- either can block and neither is the only possible defender.
      board = do
        khenra <- S.printingOf s registry "Khenra Eternal"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [khenra] [piker] [piker])
      -- All three life totals as one reading, so no mutation can hide behind the
      -- order the assertions happen to be written in.
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Afflict" $ do
        -- The proving test. alice attacks bob, bob blocks, and CR 508.5 makes bob
        -- the defending player, so bob alone loses 1. No combat damage reaches a
        -- player: the Khenra is blocked, and its 2 and the Piker's 2 trade.
        Spec.it s "CR 702.130a whole card: a blocked Khenra Eternal costs the defending player 1 life" $ do
          (gs, ours, yours, _) <- board
          case (ours, yours) of
            ([khenra], [piker]) -> do
              let after = S.runCombat (attacking S.bob) gs
              Spec.assertEqWith s "bob, and nobody else, is down 1" (lives after) (Just 20, Just 19, Just 20)
              Spec.assertBool s (not (S.onBattlefield khenra after)) "the 2/2 Khenra died to the Piker's 2"
              Spec.assertBool s (not (S.onBattlefield piker after)) "and the 2/1 Piker to the Khenra's"
            _ -> Spec.assertFailure s "fixture should give alice a Khenra and bob a Piker"
        -- CR 508.5a: the defending player is one SPECIFIC player, determined per
        -- attacking creature. The only difference from the case above is the
        -- answer to Prompt.ChooseDefender, so an implementation that bound the
        -- attacker's controller, or "an opponent", or a fixed seat cannot pass
        -- both.
        Spec.it s "CR 508.5 the life follows whichever opponent was attacked" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "carol, attacked this time, is the one down 1" (lives (S.runCombat (attacking S.carol) gs)) (Just 20, Just 20, Just 19)
        -- The control leg, on the same board: no block, so CR 509.3c's event never
        -- happens and no life is lost to afflict. bob is down TWO instead of one,
        -- the Khenra's combat damage -- distinct from 1, so the two legs cannot be
        -- read as each other.
        Spec.it s "CR 509.3c an unblocked Khenra Eternal afflicts nobody" $ do
          (gs, _, _, _) <- board
          Spec.assertEqWith s "bob took 2 combat damage and no afflict" (lives (S.runCombat (unblocked S.bob) gs)) (Just 20, Just 18, Just 20)
        -- CR 603.2 through CR 508.5: the becomes-blocked event carries the
        -- defending player, and the scan stamps them under the reserved slot rule
        -- 702.130a's "defending player" reads. The falsifier is an arm that binds
        -- the attacking side, or none at all.
        Spec.it s "CR 603.2 the defending player rides the becomes-blocked event in the reserved slot" $ do
          let bindings = Event.eventBindings TriggerCondition.SelfBecomesBlocked (GameEvent.AttackerBlocked (ObjectId.MkObjectId 9) S.carol)
          Spec.assertEqWith s "carol is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Set.singleton (Recipient.ToPlayer S.carol)))
        -- CR 702.130b: "If a creature has multiple instances of afflict, each
        -- triggers separately." Asked of the mint rather than of a board, as
        -- bushido's and prowess' are: no card in this pool prints afflict twice
        -- and nothing here grants it.
        --
        -- The inequality is the second half of the case and a separate claim: N
        -- reaches the minted ability at all. Afflict 1 is the only N a board in
        -- this pool can show, so nothing above would go red if the mint hardcoded
        -- its 1.
        Spec.it s "CR 702.130b each instance of afflict is its own ability, and N reaches it" $ do
          Spec.assertEqWith s "afflict 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 1) 2)) [Keyword.afflict 1, Keyword.afflict 1]
          Spec.assertEqWith s "and afflict 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afflict 3) 1)) [Keyword.afflict 3]
          Spec.assertBool s (Keyword.afflict 1 /= Keyword.afflict 3) "and the two differ, so N is in the ability"

-- CR 702.121 melee, whose rule text is a triggered ability,
-- and the first whose payload is a number read off game state rather than a
-- literal, with Wings of the Guard ({1}{W} Creature -- Bird 1/1, flying and
-- melee, and nothing else).
--
-- THREE SEATS throughout, and here that is load-bearing rather than tidy: at two
-- players "each opponent you attacked" and "each opponent" are the same number,
-- so a bonus that ignored the combat record entirely would pass every case.
--
-- CR 802 is unavailable (#175), so one combat phase has ONE defending player and
-- the bonus pawl can reach is 0 or 1. What separates the two is CR 506.3's other
-- attackable permanents: a creature that attacked only a planeswalker attacked no
-- opponent.
meleeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
meleeSpec s registry =
  let -- Attacks `who` with everything, aiming every attack at the player.
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      -- The same, with `these` creatures aimed at a planeswalker instead (CR
      -- 508.1b). Falls back to the head, so a board with no planeswalker offered
      -- runs exactly as the answerer above.
      aimingAtJace :: [ObjectId.ObjectId] -> PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace these who p = case p of
        Prompt.ChooseAttackTarget _ _ oid options
          | elem oid these -> case filter isPlaneswalker (NonEmpty.toList options) of
              target : _ -> target
              [] -> NonEmpty.head options
        _ -> attacking who p
      -- alice fields the Bird (plus whatever else `mine` names); bob and carol
      -- each field a Goblin Piker, so either is a legal defending player.
      board mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat (wings : mine) [piker] [piker])
      -- The same, with bob fielding Jace Beleren at loyalty 3 as well -- the one
      -- planeswalker in the pool, and the only way to attack something that is
      -- not an opponent.
      jaceBoard mine = do
        wings <- S.printingOf s registry "Wings of the Guard"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat (wings : mine) [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Melee" $ do
        -- The proving test. One opponent attacked, so +1/+1 on a 1/1. The
        -- falsifier three seats buy: a bonus counting alice's OPPONENTS rather
        -- than the ones she attacked reads 2 here and cannot pass.
        Spec.it s "CR 702.121a whole card: Wings of the Guard attacking one of two opponents is 2/2" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> Spec.assertEqWith s "1/1 plus one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- Rule 702.121a counts OPPONENTS, not creatures: a second attacker at the
        -- same seat adds nothing. The falsifier is a bonus read off the size of
        -- the declaration, which reads 2 here and 1 above.
        Spec.it s "CR 702.121a a second attacker at the same opponent does not raise the bonus" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- board [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "still one opponent attacked" (S.powerToughnessOf wings (atBlockers (attacking S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
        -- CR 508.4's sibling reading, from the other side: attacking an
        -- opponent's PLANESWALKER is not attacking that opponent, so the bonus is
        -- 0 and the Bird stays a 1/1. The attack record is asserted first, so a
        -- run where the Bird failed to attack at all fails there rather than
        -- passing this vacuously.
        Spec.it s "CR 506.3 a creature that attacked only a planeswalker gets +0/+0" $ do
          (gs, ours, theirs, _) <- jaceBoard []
          case (ours, theirs) of
            ([wings], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace [wings] S.bob) gs
              Spec.assertEqWith s "CR 508.1b the Bird really did attack Jace" (Map.lookup wings (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no opponent was attacked, so it is still a 1/1" (S.powerToughnessOf wings after) (Just (1, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and bob a Jace"
        -- Melee still TRIGGERS when its bearer attacks a planeswalker -- what the
        -- planeswalker changes is the bonus. Same board as above plus a Piker
        -- sent at bob, so the record holds one opponent and the Bird is pumped
        -- although it attacked nobody.
        Spec.it s "CR 702.121a the bearer's own attack need not be the one that counts" $ do
          piker <- S.printingOf s registry "Goblin Piker"
          (gs, ours, _, _) <- jaceBoard [piker]
          case ours of
            [wings, _] -> Spec.assertEqWith s "the Piker's attack on bob is the +1/+1" (S.powerToughnessOf wings (atBlockers (aimingAtJace [wings] S.bob) gs)) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice a Bird and a Piker"
        -- CR 611.2d: the bonus is fixed as the ability resolves, and CR 511.3
        -- clears the combat record at end of combat -- so a pump that re-read the
        -- record live would shrink back to +0/+0 the moment combat ended, while
        -- the printed duration runs to end of turn.
        Spec.it s "CR 611.2d the +1/+1 outlives the combat record it was computed from" $ do
          (gs, ours, _, _) <- board []
          case ours of
            [wings] -> do
              let after = S.runToStep Phase.PostcombatMain (attacking S.bob) gs
              Spec.assertEqWith s "CR 511.3 the record is cleared" (Combat.Type.declaredAttacked (GameState.combat after)) Set.empty
              Spec.assertEqWith s "and the Bird is still a 2/2" (S.powerToughnessOf wings after) (Just (2, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Bird"
        -- CR 702.121b: two instances are two abilities, so two triggers and two
        -- bonuses. Asserted at the mint, no card in the pool having melee twice.
        Spec.it s "CR 702.121b each instance triggers separately" $ do
          Spec.assertEqWith s "melee held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 2)) [Keyword.melee, Keyword.melee]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Melee 1)) [Keyword.melee]

-- CR 702.105 dethrone, whose CONDITION is the whole keyword -- the first minted
-- trigger narrowed by a fact about the game rather than about the declaration --
-- with Enraged Revolutionary ({2}{R} Creature -- Human Warrior 2/1, dethrone and
-- nothing else). The counter is read as power and toughness, so 2/1 is "did not
-- trigger" and 3/2 is "did".
--
-- THREE SEATS throughout, and load-bearing twice over: at two players "the player
-- with the most life" and "the defending player" coincide whenever the attacker's
-- controller is behind, and there is no second opponent to be the wrong one.
--
-- Life totals are all distinct except where a tie is the point, so no reading of
-- the rule produces the same board twice.
dethroneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
dethroneSpec s registry =
  let at pid n gs = gs {GameState.players = Map.adjust (\pl -> pl {Player.life = n}) pid (GameState.players gs)}
      lives a b c gs = at S.alice a (at S.bob b (at S.carol c gs))
      -- alice fields the Revolutionary; bob and carol each field a Piker, so
      -- either is a legal defending player.
      board = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        pure (S.threePlayerCombat [rev] [piker] [piker])
      -- The same with bob fielding Jace Beleren at loyalty 3, the pool's one
      -- planeswalker and so the only attackable permanent that is not a player.
      jaceBoard = do
        rev <- S.printingOf s registry "Enraged Revolutionary"
        piker <- S.printingOf s registry "Goblin Piker"
        jace <- S.printingOf s registry "Jace Beleren"
        pure $ case S.threePlayerCombat [rev] [piker, jace] [piker] of
          (gs, ours, theirs@(_ : jaceId : _), others) -> (S.addCounter CounterKind.Loyalty 3 jaceId gs, ours, theirs, others)
          done -> done
      isPlaneswalker target = case target of
        AttackTarget.OfPlaneswalker _ -> True
        AttackTarget.OfPlayer _ -> False
        AttackTarget.OfBattle _ -> False
      aimingAtJace :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimingAtJace who p = case p of
        Prompt.ChooseAttackTarget _ _ _ options -> case filter isPlaneswalker (NonEmpty.toList options) of
          target : _ -> target
          [] -> NonEmpty.head options
        _ -> attacking who p
      attacking :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      attacking who p = case p of
        Prompt.ChooseDefender {} -> who
        _ -> S.aggressiveAnswer p
      atBlockers = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "Dethrone" $ do
        -- The proving test. bob is on 20 against alice's 15 and carol's 10, so the
        -- creature grows.
        Spec.it s "CR 702.105a whole card: attacking the player with the most life is a +1/+1 counter" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "2/1 plus one counter" (S.powerToughnessOf rev (atBlockers (attacking S.bob) (lives 15 20 10 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- The same board attacked the other way. carol is the LOWEST, so nothing
        -- triggers -- the falsifier for a condition that fired on any attack.
        Spec.it s "CR 702.105a attacking a player who is not on the most life does nothing" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.carol) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack carol" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.carol))
              Spec.assertEqWith s "and it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- Rule 702.105a says "the player with the most life", not "the opponent
        -- with the most life", so the attacker's OWN controller is compared too:
        -- alice on 25 makes bob's 20 not the most, and the same attack that grew
        -- the creature above now does nothing.
        Spec.it s "CR 702.105a the attacking creature's controller counts as a player" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> do
              let after = atBlockers (attacking S.bob) (lives 25 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack bob" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlayer S.bob))
              Spec.assertEqWith s "and alice is ahead, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- "Or tied for most life": bob and carol both on 20, and attacking either
        -- triggers. The falsifier is a strict comparison, which fires on neither.
        Spec.it s "CR 702.105a a tie for most life still triggers" $ do
          (gs, ours, _, _) <- board
          case ours of
            [rev] -> Spec.assertEqWith s "tied at 20 against alice's 15" (S.powerToughnessOf rev (atBlockers (attacking S.carol) (lives 15 20 20 gs))) (Just (3, 2))
            _ -> Spec.assertFailure s "fixture should give alice one Revolutionary"
        -- CR 702.105a names THE PLAYER, and CR 508.1b lets a creature attack a
        -- planeswalker instead. The defending player is bob either way, so this is
        -- the case that separates reading Combat.attackers from reading the
        -- declaration event's CR 508.5 field.
        Spec.it s "CR 508.1b attacking the leader's planeswalker is not attacking the leader" $ do
          (gs, ours, theirs, _) <- jaceBoard
          case (ours, theirs) of
            ([rev], [_, jaceId]) -> do
              let after = atBlockers (aimingAtJace S.bob) (lives 15 20 10 gs)
              Spec.assertEqWith s "CR 508.1b the Revolutionary really did attack Jace" (Map.lookup rev (Combat.Type.attackers (GameState.combat after))) (Just (AttackTarget.OfPlaneswalker jaceId))
              Spec.assertEqWith s "and no player was attacked, so it is still a 2/1" (S.powerToughnessOf rev after) (Just (2, 1))
            _ -> Spec.assertFailure s "fixture should give alice a Revolutionary and bob a Jace"
        -- CR 702.105b: two instances are two abilities, so two triggers and two
        -- counters. Asserted at the mint, no card in the pool having dethrone twice.
        Spec.it s "CR 702.105b each instance triggers separately" $ do
          Spec.assertEqWith s "dethrone held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 2)) [Keyword.dethrone, Keyword.dethrone]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton Keyword.Type.Dethrone 1)) [Keyword.dethrone]

-- CR 702.23 rampage, whose rule text is a triggered ability,
-- and the first whose bonus multiplies a printed N by a number read off the
-- board.
--
-- Wolverine Pack {2}{G}{G} Creature -- Wolverine 2/4 is the card: rampage 2 and
-- nothing else. Its numbers are chosen so no two readings of rule 702.23a agree
-- -- an asymmetric 2/4 base, and N = 2 rather than 1, so "+N per blocker" (6/8 at
-- two blockers), "+1 per blocker beyond the first" (3/5) and the rule's own
-- reading (4/6) are three different pairs.
--
-- Horrible Hordes {3} Artifact Creature -- Spirit 2/2, rampage 1, is the second
-- producer and is what pins N: the same three blockers give it +2/+2 where the
-- Pack gets +4/+4.
--
-- Every reading but the last is taken at the COMBAT DAMAGE step, before damage is
-- dealt -- CR 509.2a puts the trigger on the stack in the declare blockers step,
-- so the bonus is already applied there.
rampageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rampageSpec s registry =
  let board mine theirs = do
        ours <- mapM (S.printingOf s registry) mine
        yours <- mapM (S.printingOf s registry) theirs
        pure (S.combatBoardOf ours yours)
      noBlocks :: Prompt.Prompt r -> r
      noBlocks p = case p of
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
      atDamage = S.runToStep (Phase.Combat CombatStep.CombatDamage) S.aggressiveAnswer
   in Spec.describe s "Rampage" $ do
        -- The proving test: two blockers is one beyond the first, so rampage 2 is
        -- +2/+2 and the 2/4 Pack is a 4/6.
        Spec.it s "CR 702.23a whole card: Wolverine Pack blocked by two creatures is 4/6" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus one blocker beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (4, 6))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- A THIRD blocker is a second creature beyond the first, so the bonus
        -- doubles rather than growing by one. The falsifier is a bonus that adds 1
        -- per creature beyond the first instead of N, which reads 4/6 here.
        Spec.it s "CR 702.23a a third blocker is a second +2/+2" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> Spec.assertEqWith s "2/4 plus two beyond the first, twice" (S.powerToughnessOf pack (atDamage gs)) (Just (6, 8))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- "BEYOND THE FIRST": one blocker is a trigger with a bonus of 0, not a
        -- bonus of N. The falsifier is a bonus counting blockers outright, which
        -- reads 4/6 here.
        Spec.it s "CR 702.23a one blocker leaves the Pack a 2/4" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> Spec.assertEqWith s "the first blocker is not beyond the first" (S.powerToughnessOf pack (atDamage gs)) (Just (2, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- CR 509.3c is the event, so an UNBLOCKED attacker never triggers at all.
        -- Asserted on the LOG and not on power and toughness, which cannot tell
        -- the two apart: a trigger that fired with no blockers would be +0/+0 and
        -- leave the same 2/4. The blocked leg is the same board with the block
        -- taken, so the pair differs in nothing but CR 509.1's declaration.
        Spec.it s "CR 509.3c an unblocked Pack never triggers" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker"]
          case mine of
            [pack] -> do
              let fired after = elem (GameEvent.AbilityTriggered pack S.alice TriggerCondition.SelfBecomesBlocked) (S.eventsOf after)
              Spec.assertBool s (not (fired (S.runToStep (Phase.Combat CombatStep.CombatDamage) noBlocks gs))) "nothing blocked, so nothing triggered"
              Spec.assertBool s (fired (atDamage gs)) "and the same board with the block taken does trigger"
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- N is the card's, not the engine's: rampage 1 against the same three
        -- blockers is +2/+2 where rampage 2 was +4/+4.
        Spec.it s "CR 702.23a rampage 1 on the same board is half the bonus" $ do
          (gs, mine, _) <- board ["Horrible Hordes"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [hordes] -> Spec.assertEqWith s "2/2 plus two beyond the first, once" (S.powerToughnessOf hordes (atDamage gs)) (Just (4, 4))
            _ -> Spec.assertFailure s "fixture should give alice one Hordes"
        -- CR 702.23b: the bonus is calculated as the ability RESOLVES and does not
        -- move afterwards. CR 511.3 clears Combat.blockers at end of combat, so a
        -- bonus that re-read the declaration live would fall back to +0/+0 the
        -- moment combat ended, while the printed duration runs to end of turn. The
        -- Pack is a 6/8 taking 7, so it survives to be read.
        Spec.it s "CR 702.23b the bonus outlives the blockers it was counted from" $ do
          (gs, mine, _) <- board ["Wolverine Pack"] ["Goblin Piker", "Hill Giant", "Icehide Golem"]
          case mine of
            [pack] -> do
              let after = S.runToStep Phase.PostcombatMain S.aggressiveAnswer gs
              Spec.assertEqWith s "CR 511.3 the declaration is cleared" (Combat.Type.blockers (GameState.combat after)) Map.empty
              Spec.assertEqWith s "and the Pack is still a 6/8" (S.powerToughnessOf pack after) (Just (6, 8))
            _ -> Spec.assertFailure s "fixture should give alice one Pack"
        -- CR 702.23c: each instance triggers separately. Asserted at the MINT, no
        -- printing in the pool carrying rampage twice, and the second assertion is
        -- what puts N inside the ability rather than beside it.
        Spec.it s "CR 702.23c each instance triggers separately" $ do
          Spec.assertEqWith s "rampage 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 2)) [Keyword.rampage 2, Keyword.rampage 2]
          Spec.assertEqWith s "and once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Rampage 2) 1)) [Keyword.rampage 2]
          Spec.assertBool s (Keyword.rampage 1 /= Keyword.rampage 2) "and the two differ, so N is in the ability"

-- CR 603.6a's SECOND written form -- "Whenever a [type] enters, . . ." -- and
-- Soul Warden {W} Creature -- Human Cleric 1/1, "Whenever another creature
-- enters, you gain 1 life", the card that proves it. Its effect names nothing
-- about the entering creature, so these cases isolate the trigger CONDITION;
-- its "another" is Filter.Not Filter.IsSource inside the condition's own
-- Filter, never a second exclusion mechanism (#163).
permanentEntersSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentEntersSpec s registry =
  let anyCreature = Filter.Type.HasCardType CardType.Creature
      anotherCreature = Filter.Type.And [anyCreature, Filter.Type.Not Filter.Type.IsSource]
      enters oid = GameEvent.Moved (ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield) S.emptyCharacteristics
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
   in Spec.describe s "PermanentEnters" $ do
        -- The gameplay-level proof, cast to resolution: alice's second Soul
        -- Warden enters and the FIRST one's trigger resolves for exactly 1
        -- life. Exactly one life, not two, is the "another" falsifier -- the
        -- newcomer is checked against its own entry (the case below proves
        -- the scan does check it) and its own Filter is what declines.
        Spec.it s "CR 603.6a whole cards: a second Soul Warden entering gains alice exactly 1 life" $ do
          plains <- S.printingOf s registry "Plains"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (_, base) = S.addCreature soulWarden S.alice (S.landsInPlay plains 1)
              (gs, spellId) = S.handOne soulWarden base
              cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
              settled = snd (Engine.runGamePure S.identityAnswer cast Engine.priorityLoop)
          Spec.assertEqWith s "both Wardens are on the battlefield" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Soul Warden") S.alice settled) 2
          Spec.assertEqWith s "alice gained exactly 1" (S.lifeOf S.alice settled) (fmap (+ 1) (S.lifeOf S.alice gs))
          Spec.assertEqWith s "bob gained nothing" (S.lifeOf S.bob settled) (S.lifeOf S.bob gs)
        -- CR 603.6a: "all permanents on the battlefield (INCLUDING THE
        -- NEWCOMERS) are checked for any enters-the-battlefield triggers that
        -- match the event." The newcomer really is offered its own entry; a
        -- bare "a creature enters" admits it, and only Soul Warden's printed
        -- "another" turns it away. This is why the constructor is not called
        -- OtherEnters.
        Spec.it s "CR 603.6a including the newcomers: a permanent is checked against its own entry" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (oid, gs) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
          Spec.assertBool s (Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anyCreature) (enters oid)) "\"a creature enters\" admits the newcomer itself"
          Spec.assertBool s (not (Event.matchesTrigger gs oid S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters oid))) "\"another creature enters\" does not"
        -- The live-reading falsifier. `enters` above hands the event a
        -- deliberately EMPTY ProjectedCharacteristics -- no card types at all
        -- -- because that snapshot is CR 608.2h last known information for the
        -- zone the object LEFT, and matching against it would answer CR 603.6b
        -- backwards: "continuous effects that modify characteristics of a
        -- permanent do so the moment the permanent is on the battlefield (and
        -- not before then)". CR 603.10 says the same of an event's objects.
        -- The Piker is a Creature only in the live projection, so a matcher
        -- reading the snapshot fires nothing here.
        Spec.it s "CR 603.6b/603.10 the entrant is read live, not from the Moved event's snapshot" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs1) = S.addCreature piker S.bob gs0
          Spec.assertEqWith s "no card types in the snapshot" (PC.cardTypes S.emptyCharacteristics) Set.empty
          Spec.assertBool s (Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) (enters pikerId)) "and the trigger still fires"
        -- CR 608.2h: an entrant that is already gone by the CR 117.5 boundary
        -- -- here moved straight on to the graveyard -- is read from last known
        -- information, which for a permanent that left the battlefield is the
        -- battlefield reading. The event happened; the trigger is not lost with
        -- the object. Same fallback eventTriggers' own group-scoped unions take for
        -- the bearer side.
        Spec.it s "CR 608.2h a creature that enters and leaves again still fires the trigger" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (handCard, gs1) = S.addHandCard piker S.bob gs0
              entered = S.runPure S.identityAnswer gs1 (Event.changeZone handCard Zone.Battlefield)
              newIds = fmap ZoneChange.object (filter ((==) Zone.Battlefield . ZoneChange.to) (S.zoneChangesOf entered))
          case newIds of
            [newId] -> do
              let gone = S.runPure S.identityAnswer entered (Event.changeZone newId Zone.Graveyard)
              Spec.assertEqWith s "the entrant is off the battlefield" (Game.lookupObject newId gone) Nothing
              Spec.assertEqWith s "the Warden still triggered, once" (sourcesOf gone) [TriggerSource.OfObject warden]
            other -> Spec.assertFailure s ("expected exactly one battlefield entry, got " <> show (length other))
        -- The type half of the Filter: a LAND entering is not a creature
        -- entering. Plains has no ability of its own, so nothing else can
        -- stand in for the silence.
        Spec.it s "CR 603.6a a noncreature permanent entering fires nothing" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          plains <- S.printingOf s registry "Plains"
          let (_, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (landId, gs1) = S.addCreature plains S.alice gs0
              gs2 = S.withEvents [GameEvent.Moved (ZoneChange.MkZoneChange landId landId Zone.Stack Zone.Battlefield) (Projection.project landId gs1)] gs1
          Spec.assertEqWith s "no trigger" (sourcesOf gs2) []
        -- The destination half: CR 603.6a is an ENTERS-THE-BATTLEFIELD
        -- ability, so a creature card moving to a graveyard is not it.
        Spec.it s "CR 603.6a only a battlefield destination fires it" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs1) = S.addCreature piker S.bob gs0
              toGrave = GameEvent.Moved (ZoneChange.MkZoneChange pikerId pikerId Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics
          Spec.assertBool s (not (Event.matchesTrigger gs1 warden S.alice (TriggerCondition.PermanentEnters anotherCreature) toGrave)) "a graveyard-bound move does not match"
        -- CR 603.6a: "EACH TIME an event puts one or more permanents onto the
        -- battlefield" -- one bearer, two entering creatures, two triggers. A
        -- count, not a boolean, so "fires once per entrant" is distinguishable
        -- from "fires once per batch".
        Spec.it s "CR 603.6a one Soul Warden fires once per entering creature" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (warden, gs0) = S.addCreature soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              (first, gs1) = S.addCreature piker S.bob gs0
              (second, gs2) = S.addCreature piker S.bob gs1
              gs3 =
                S.withEvents
                  [ GameEvent.Moved (ZoneChange.MkZoneChange first first Zone.Stack Zone.Battlefield) (Projection.project first gs2),
                    GameEvent.Moved (ZoneChange.MkZoneChange second second Zone.Stack Zone.Battlefield) (Projection.project second gs2)
                  ]
                  gs2
          Spec.assertEqWith s "twice, both from the one Warden" (sourcesOf gs3) (replicate 2 (TriggerSource.OfObject warden))

-- CR 113.6k: "A trigger condition that can't trigger from the battlefield
-- functions in all zones it can trigger from." Narcomoeba's "When this card is
-- put into your graveyard from your library" is such a condition -- the bearer
-- is in a graveyard when it fires, never on the battlefield -- so the scan has
-- to look somewhere other than the battlefield to find it.
--
-- The proving pair is Tome Scour ("target player mills five cards") and
-- Narcomoeba; Soul Warden rides along in the same graveyard as the control,
-- because its CR 603.6a trigger functions ONLY on the battlefield and so must
-- stay silent even when a creature enters right in front of it.
graveyardTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardTriggerSpec s registry =
  let -- alice: one Island in play (Tome Scour's {U}), Tome Scour in hand, and a
      -- three-card library of Narcomoeba, Soul Warden and a Goblin Piker. Five
      -- mills a three-card library empty (CR 701.17b), so every one of them
      -- lands in the graveyard in one event batch and the scan has to pick the
      -- one card whose ability functions there.
      milledBoard = do
        island <- S.printingOf s registry "Island"
        tomeScour <- S.printingOf s registry "Tome Scour"
        narcomoeba <- S.printingOf s registry "Narcomoeba"
        soulWarden <- S.printingOf s registry "Soul Warden"
        piker <- S.printingOf s registry "Goblin Piker"
        let base = S.landsInPlay island 1
            (_, g1) = S.addLibraryCard narcomoeba S.alice base
            (_, g2) = S.addLibraryCard soulWarden S.alice g1
            (_, g3) = S.addLibraryCard piker S.alice g2
            (g4, spellId) = S.handOne tomeScour g3
        pure (g4 {GameState.priority = Just S.alice}, spellId)
      -- Takes every "may". There is exactly one in this scenario -- Narcomoeba's
      -- -- so this is not a blanket yes standing in for a specific answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- Cast Tome Scour at alice herself (S.identityAnswer's ChooseTargets picks
      -- the least id in each set, and alice is player 0), resolve it, settle so
      -- any trigger reaches the stack, then resolve that trigger.
      millSelf :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      millSelf answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            milled = S.runPure answer cast Stack.resolveTop
            placed = S.runPure answer milled Engine.settleForPriority
         in (placed, S.runPure answer placed Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      narcomoebaName = CardName.MkCardName $ Text.pack "Narcomoeba"
   in Spec.describe s "GraveyardTrigger" $ do
        -- The gameplay-level proof, cast to resolution.
        Spec.it s "CR 113.6k whole card: Tome Scour mills Narcomoeba and its trigger puts it onto the battlefield" $ do
          board <- milledBoard
          let (placed, after) = millSelf takeOptional board
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after)) "Narcomoeba is on the battlefield"
          Spec.assertBool s (not (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
          -- The control, in the same graveyard: Soul Warden's "whenever
          -- another creature enters" functions only on the battlefield (CR
          -- 113.6's default), and a creature entered right in front of it.
          Spec.assertEqWith s "the Soul Warden in the graveyard gained nothing" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.5: the "may" is a real choice, and declining is the other
        -- half of it. The trigger still went on the stack and still resolved.
        Spec.it s "CR 603.5 declining the may leaves Narcomoeba in the graveyard" $ do
          board <- milledBoard
          let (placed, after) = millSelf S.identityAnswer board
          Spec.assertEqWith s "the trigger reached the stack anyway" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice after)) "Narcomoeba is still in the graveyard"
          Spec.assertBool s (not (Set.member narcomoebaName (namesIn Zone.Battlefield S.alice after))) "and not on the battlefield"
          Spec.assertEqWith s "and the ability left the stack -- a declined may is not a fizzle" (length (GameState.stack after)) 0
        -- "from your library" doing real work, half one: the same card moved
        -- out of a HAND reaches the same graveyard and must not trigger.
        Spec.it s "CR 113.6k Narcomoeba put into the graveyard from the HAND does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (handCard, gs) = S.addHandCard narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              buried = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped buried) buried))) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice buried)) "it is in the graveyard"
        -- "from your library" doing real work, half two: dying is a move to
        -- the same graveyard from the battlefield, and is not this trigger.
        Spec.it s "CR 113.6k Narcomoeba dying from the BATTLEFIELD does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (creature, gs) = S.addCreature narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice died)) "it is in the graveyard"
        -- The zone half, isolated from the mill: a graveyard card whose only
        -- trigger functions on the battlefield (CR 113.6's default) is not
        -- scanned into firing by an event it would have seen from play.
        Spec.it s "CR 113.6 a battlefield-only trigger in a graveyard is not scanned" $ do
          soulWarden <- S.printingOf s registry "Soul Warden"
          piker <- S.printingOf s registry "Goblin Piker"
          let (wardenCard, gs0) = S.addLibraryCard soulWarden S.alice (Setup.emptyGame S.bothPlayers)
              buried = S.runPure S.identityAnswer gs0 (Event.changeZone wardenCard Zone.Graveyard)
              (pikerCard, gs1) = S.addHandCard piker S.alice buried
              entered = S.runPure S.identityAnswer gs1 (Event.changeZone pikerCard Zone.Battlefield)
          Spec.assertBool s (Set.member (CardName.MkCardName $ Text.pack "Soul Warden") (namesIn Zone.Graveyard S.alice entered)) "the Warden is in the graveyard"
          Spec.assertEqWith s "and a creature entering fires nothing" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped entered) entered))) []

-- Gaea's Blessing {1}{G} Sorcery, "Target player shuffles up to three target
-- cards from their graveyard into their library. Draw a card. When this card is
-- put into your graveyard from your library, shuffle your graveyard into your
-- library." (name, cost, type line and oracle text checked against Scryfall.)
--
-- Narcomoeba's CR 113.6k condition carrying rule 701.24's OTHER shape: a set
-- rather than named objects (Effect.ShuffleIntoLibrary over
-- ObjectRef.EachCardInGraveyard), which is CR 701.24d -- "if an effect would
-- cause a player to shuffle a set of objects into a library, that library is
-- shuffled even if there are no objects in that set". The set is empty when the
-- graveyard is emptied in response, and the library named by the effect is then
-- the only thing left saying which library to shuffle (#558).
gaeasBlessingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gaeasBlessingSpec s registry =
  let namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- CR 701.24a leaves a shuffle observable only through the order it
      -- produces, so the interpreter REVERSES it (TargetSpec's Riftsweeper cases,
      -- for the same reason). What order a shuffle leaves is not asserted
      -- anywhere -- a real one has none.
      reversing :: Prompt.Prompt r -> r
      reversing p = case p of
        Prompt.Shuffle ids -> reverse ids
        _ -> S.identityAnswer p
   in Spec.describe s "GaeasBlessing" $ do
        -- The whole card through a real mill: Tome Scour empties alice's
        -- three-card library, so every card lands in her graveyard at once and
        -- the trigger scan has to find the ability on a SORCERY card there. Tome
        -- Scour itself joins them on the way out (CR 608.2n), which is why the
        -- library the trigger refills is four cards and not three.
        Spec.it s "CR 113.6k whole card: milled, Gaea's Blessing shuffles the whole graveyard back into the library" $ do
          island <- S.printingOf s registry "Island"
          tomeScour <- S.printingOf s registry "Tome Scour"
          blessing <- S.printingOf s registry "Gaea's Blessing"
          piker <- S.printingOf s registry "Goblin Piker"
          bolt <- S.printingOf s registry "Lightning Bolt"
          let base = S.landsInPlay island 1
              (_, g1) = S.addLibraryCard blessing S.alice base
              (_, g2) = S.addLibraryCard piker S.alice g1
              (_, g3) = S.addLibraryCard bolt S.alice g2
              (g4, spellId) = S.handOne tomeScour g3
              board = g4 {GameState.priority = Just S.alice}
              cast = S.runPure reversing board (S.cast S.alice spellId)
              milled = S.runPure reversing cast Stack.resolveTop
              placed = S.runPure reversing milled Engine.settleForPriority
              after = S.runPure reversing placed Stack.resolveTop
          Spec.assertEqWith s "the mill emptied the library" (length (Game.zoneMembers Zone.Library S.alice milled)) 0
          Spec.assertEqWith s "and its trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "which empties the graveyard" (Game.zoneMembers Zone.Graveyard S.alice after) []
          Spec.assertEqWith s "into a library of four -- the three milled cards and Tome Scour" (length (Game.zoneMembers Zone.Library S.alice after)) 4
          Spec.assertEqWith
            s
            "each of them by name"
            (namesIn Zone.Library S.alice after)
            (Set.fromList (fmap (CardName.MkCardName . Text.pack) ["Gaea's Blessing", "Goblin Piker", "Lightning Bolt", "Tome Scour"]))
        -- CR 701.24d, off a PAIR of boards differing in exactly one thing:
        -- whether the graveyard was emptied between the trigger going on the
        -- stack and its resolution. With the set empty there is no object left to
        -- read an owner off, so only the library the effect NAMES can be shuffled
        -- -- and the reversal is what shows it was.
        --
        -- THREE SEATS: bob's library is seeded and untouched, so "your library"
        -- is told apart from every library. carol holds the third seat so that
        -- bob is not simply "the other player".
        Spec.it s "CR 701.24d the library is shuffled even when the graveyard has been emptied in response" $ do
          blessing <- S.printingOf s registry "Gaea's Blessing"
          piker <- S.printingOf s registry "Goblin Piker"
          bolt <- S.printingOf s registry "Lightning Bolt"
          -- S.addLibraryCard puts each card ON TOP, so the second of each pair
          -- heads the library and the first sits under it.
          let (blessingId, g1) = S.addLibraryCard blessing S.alice S.threePlayerGame
              (herDeeperId, g2) = S.addLibraryCard piker S.alice g1
              (herTopId, g3) = S.addLibraryCard bolt S.alice g2
              (hisDeeperId, g4) = S.addLibraryCard bolt S.bob g3
              (hisTopId, board) = S.addLibraryCard piker S.bob g4
              buried = S.runPure S.identityAnswer board (Event.changeZone blessingId Zone.Graveyard)
              placed = S.runPure S.identityAnswer buried Engine.settleForPriority
              emptied = case Game.zoneMembers Zone.Graveyard S.alice placed of
                -- CR 400.7 minted a fresh id when the card arrived, so the
                -- graveyard's own member is the one to exile.
                [buriedId] -> S.runPure S.identityAnswer placed (Event.changeZone buriedId Zone.Exile)
                _ -> placed
              withCard = S.runPure reversing placed Stack.resolveTop
              withoutCard = S.runPure reversing emptied Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "the control: it shuffles the graveyard's one card in, leaving the graveyard empty" (Game.zoneMembers Zone.Graveyard S.alice withCard) []
          Spec.assertEqWith s "and her library three" (length (Game.zoneMembers Zone.Library S.alice withCard)) 3
          Spec.assertEqWith s "exiled in response, the graveyard is empty before the trigger resolves" (Game.zoneMembers Zone.Graveyard S.alice emptied) []
          Spec.assertEqWith
            s
            "CR 701.24d: her library is shuffled all the same, and gains nothing -- the reversal shows through"
            (Game.zoneMembers Zone.Library S.alice withoutCard)
            [herDeeperId, herTopId]
          Spec.assertEqWith s "with bob's library neither shuffled nor added to" (Game.zoneMembers Zone.Library S.bob withoutCard) [hisTopId, hisDeeperId]
          Spec.assertEqWith s "and the trigger off the stack" (length (GameState.stack withoutCard)) 0

-- CR 113.6m for a TRIGGERED ability: "an ability whose cost or effect specifies
-- that it moves the object it's on out of a particular zone functions only in
-- that zone". The rule says "an ability", not "an activated ability", and
-- Pawl.ActivateSpec's Reassembling Skeleton is the same sentence read off an
-- activated one.
--
-- Squee, Goblin Nabob {2}{R} Legendary Creature -- Goblin 1/1, "At the beginning
-- of your upkeep, you may return this card from your graveyard to your hand."
-- (name, cost, type line, P/T and oracle text checked against Scryfall.) The
-- printing that makes the rule bite, because CR 113.6k cannot reach it: "at the
-- beginning of your upkeep" is a condition that triggers perfectly well from the
-- battlefield, so the only thing that says "graveyard" is the effect's own
-- words.
--
-- The controls are built to leave the zone derivation as the sole difference:
--
--   * Bitterblossom sits in the SAME graveyard, with the SAME condition, and its
--     effect names no zone. CR 113.6's default keeps it on the battlefield, so
--     the upkeep that fires Squee must pass it over. Without it, a scan that
--     simply offered every graveyard card's every ability would pass.
--   * Squee ON the battlefield at the same upkeep fires nothing, which is the
--     "functions ONLY in that zone" half.
-- CR 114.4: "abilities of emblems function in the command zone" -- the third zone
-- Pawl.Engine.Event.eventTriggers scans, and the only one whose membership is
-- decided by the OBJECT rather than by the trigger condition (CR 113.6p).
--
-- Ajani, Adversary of Tyrants {2}{W}{W} Legendary Planeswalker -- Ajani, loyalty
-- 4. "-7: You get an emblem with \"At the beginning of your end step, create three
-- 1/1 white Cat creature tokens with lifelink.\"" (Name, cost, type line, loyalty
-- and oracle text checked against Scryfall.) The pool's first emblem with a
-- triggered ability, so before this the ability was authorable and inert.
--
-- The board holds the vacuity traps down:
--
--   * The emblem is the ONLY bearer, and CR 114.1 keeps it in the command zone
--     for its whole existence -- there is no battlefield reading of this ability
--     for the assertion to be passing on instead.
--   * Three seats, and the two boards differ in exactly one thing: WHOSE end step
--     began. "At the beginning of YOUR end step" is CR 114.2's controller, and a
--     scan that took the active player, or the owner of some other object, would
--     fire on bob's.
commandZoneTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
commandZoneTriggerSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      -- One seat's end step, everything else held equal.
      beginEndStepOf pid gs =
        Event.recordEvent
          (GameEvent.StepBegan endStep pid)
          gs {GameState.phase = endStep, GameState.activePlayer = pid}
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      catName = CardName.MkCardName (Text.pack "Cat Token")
      cats gs = length (filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just catName) (Set.toList (GameState.battlefield gs)))
      ultimate = 2 :: Int
      -- alice's Ajani at seven loyalty, its ultimate activated and resolved, at
      -- three seats. The loyalty is a fixture rather than seven turns of +1: CR
      -- 306.5b's counters are what the ability's cost pays, and how they got there
      -- is no part of what this asks.
      emblemBoard = do
        ajani <- S.printingOf s registry "Ajani, Adversary of Tyrants"
        let (ajaniId, g1) = S.addCreature ajani S.alice S.threePlayerGame
            armed = S.addCounter CounterKind.Loyalty 7 ajaniId g1
            used = case drop ultimate (Face.activatedAbilities (S.combinedFace ajani)) of
              ability : _ -> S.runPure S.identityAnswer armed (do Activate.activateAbility S.alice ajaniId ability; Stack.resolveTop)
              [] -> armed
        pure (Set.toList (GameState.command used), used)
   in Spec.describe s "CommandZoneTrigger" $ do
        -- The premise, asserted rather than assumed: CR 114.2 put one emblem in
        -- the command zone, and it is on nobody's battlefield.
        Spec.it s "CR 114.2 the ultimate puts one emblem in the command zone" $ do
          (emblems, gs) <- emblemBoard
          Spec.assertEqWith s "one emblem" (length emblems) 1
          Spec.assertEqWith s "no Cats yet" (cats gs) 0
        -- The gathering itself, at the narrowest path: one trigger, borne by the
        -- emblem, from a zone no other source reads.
        Spec.it s "CR 114.4 the emblem's trigger is gathered from the command zone" $ do
          (emblems, gs) <- emblemBoard
          let atEnd = beginEndStepOf S.alice gs
          Spec.assertEqWith
            s
            "exactly one trigger, borne by the emblem"
            (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped atEnd) atEnd)))
            (fmap TriggerSource.OfObject emblems)
        -- End to end through the real engine: placed, resolved, three Cats.
        Spec.it s "CR 114.4 whole card: three Cat tokens arrive at its controller's end step" $ do
          (_, gs) <- emblemBoard
          let placed = settle (beginEndStepOf S.alice gs)
              after = resolveAll placed
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "three Cats" (cats after) 3
        -- The negative, on the same board with one thing changed: CR 114.2 makes
        -- the emblem alice's, and bob's end step is not hers.
        Spec.it s "CR 114.2 another seat's end step fires nothing" $ do
          (_, gs) <- emblemBoard
          let atBobs = beginEndStepOf S.bob gs
              after = resolveAll (settle atBobs)
          Spec.assertEqWith s "no trigger gathered" (length (fst (Event.gatherTriggers (Event.unscannedGrouped atBobs) atBobs))) 0
          Spec.assertEqWith s "no Cats" (cats after) 0

graveyardEffectZoneTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
graveyardEffectZoneTriggerSpec s registry =
  let squeeName = CardName.MkCardName (Text.pack "Squee, Goblin Nabob")
      upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      settle answer gs = S.runPure answer gs Engine.settleForPriority
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Takes every "may". Squee's is the only one `buriedBoard` can raise --
      -- Bitterblossom's mode is mandatory -- so this is not a blanket yes
      -- standing in for a specific answer.
      takeOptional :: Prompt.Prompt r -> r
      takeOptional p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      -- alice's graveyard: Squee, and Bitterblossom as the control. Returns
      -- Squee's graveyard id and the board with alice's upkeep begun.
      buriedBoard = do
        squee <- S.printingOf s registry "Squee, Goblin Nabob"
        bitterblossom <- S.printingOf s registry "Bitterblossom"
        let (_, g1) = S.addGraveyardCard bitterblossom S.alice (Setup.emptyGame S.bothPlayers)
            (squeeId, g2) = S.addGraveyardCard squee S.alice g1
        pure (squeeId, beginUpkeep g2)
   in Spec.describe s "GraveyardEffectZoneTrigger" $ do
        -- The gathering itself: one trigger, and it is Squee's. The count is what
        -- the Bitterblossom control turns on -- two would mean the scan read the
        -- graveyard indiscriminately.
        Spec.it s "CR 113.6m Squee's upkeep trigger is gathered from the graveyard, on its effect's word alone" $ do
          (squeeId, gs) <- buriedBoard
          Spec.assertEqWith
            s
            "exactly one trigger, from Squee"
            (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs)))
            [TriggerSource.OfObject squeeId]
        -- End to end through the real engine: the trigger is placed, resolves,
        -- and CR 400.7's funnel moves the card to alice's hand.
        Spec.it s "CR 113.6m whole card: it resolves and Squee returns to its owner's hand" $ do
          (_, gs) <- buriedBoard
          let placed = settle takeOptional gs
              after = S.runPure takeOptional placed Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member squeeName (namesIn Zone.Hand S.alice after)) "Squee is in hand"
          Spec.assertBool s (not (Set.member squeeName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
          -- The control, in the same graveyard and under the same condition:
          -- Bitterblossom's effect names no zone, so CR 113.6's default leaves it
          -- on the battlefield and its "you lose 1 life" never runs.
          Spec.assertEqWith s "the Bitterblossom in the graveyard cost alice nothing" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.5: the "may" is a real choice. The trigger is placed either way,
        -- so declining tells the zone gate apart from the mode gate.
        Spec.it s "CR 603.5 declining the may leaves Squee in the graveyard" $ do
          (_, gs) <- buriedBoard
          let placed = settle S.identityAnswer gs
              after = S.runPure S.identityAnswer placed Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack anyway" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member squeeName (namesIn Zone.Graveyard S.alice after)) "Squee is still in the graveyard"
          Spec.assertBool s (not (Set.member squeeName (namesIn Zone.Hand S.alice after))) "and not in hand"
        -- "Functions ONLY in that zone", the other direction: the same card, the
        -- same upkeep, one zone away. Nothing but CR 113.6m can withhold it --
        -- the condition matches a battlefield permanent perfectly well, which is
        -- exactly what Bitterblossom does from there.
        Spec.it s "CR 113.6m the same card on the battlefield triggers for nobody" $ do
          squee <- S.printingOf s registry "Squee, Goblin Nabob"
          let (_, gs) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
              begun = beginUpkeep gs
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped begun) begun))) []

-- Serra Avatar ({4}{W}{W}{W} Creature -- Avatar, printed */*), second line: "When
-- Serra Avatar is put into a graveyard from anywhere, shuffle it into its
-- owner's library." Oracle text verified against Scryfall. Its first line, the
-- CR 604.3 life-total P/T, is Pawl.PowerToughnessSpec's half.
--
-- CR 603.6c's LAST sentence is the whole reason this is a condition of its own:
-- "An ability that triggers when a card is put into a certain zone 'from
-- anywhere' is never treated as a leaves-the-battlefield ability, even if an
-- object is put into that zone from the battlefield." Two things follow that
-- SelfDies gets the other way round, and the tests below are built to tell them
-- apart:
--
--   * a NON-battlefield origin fires it. A discarded or milled Serra Avatar
--     never left the battlefield, and encoding this trigger as SelfDies would
--     leave it in the graveyard.
--   * no CR 603.10a look-back. Not being a leaves-the-battlefield ability, this
--     condition is absent from that rule's list of exceptions, so CR 603.10's
--     normal reading applies and the bearer is the CR 400.7 incarnation that
--     ARRIVED in the graveyard -- which is also the card the shuffle has to move,
--     so "self" names it directly and no `became` slot is needed.
--
-- CR 113.6k puts the ability in the graveyard, exactly as it does Narcomoeba's
-- above, and for a nearer reason: this condition can never trigger with its
-- bearer on the battlefield, however the card got to the graveyard.
serraAvatarSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
serraAvatarSpec s registry =
  let avatarName = CardName.MkCardName (Text.pack "Serra Avatar")
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Settle so any trigger reaches the stack, then resolve it.
      fireTrigger gs =
        let placed = S.runPure S.identityAnswer gs Engine.settleForPriority
         in (placed, S.runPure S.identityAnswer placed Stack.resolveTop)
      -- One Serra Avatar in a zone of alice's, with priority so
      -- Engine.settleForPriority has somewhere to give it.
      cardIn place = do
        avatar <- S.printingOf s registry "Serra Avatar"
        let (oid, gs) = place avatar S.alice (Setup.emptyGame S.bothPlayers)
        pure (oid, gs {GameState.priority = Just S.alice})
   in Spec.describe s "Serra Avatar" $ do
        -- The gameplay-level proof, cast to resolution: alice Murders her own
        -- Avatar. S.identityAnswer targets the least Recipient and
        -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
        -- creature on the board is the target without a bespoke interpreter.
        Spec.it s "CR 603.6 whole card: a Murdered Serra Avatar shuffles itself into its owner's library" $ do
          swamp <- S.printingOf s registry "Swamp"
          murder <- S.printingOf s registry "Murder"
          avatar <- S.printingOf s registry "Serra Avatar"
          let (gs0, spellId) = S.handOne murder (S.landsInPlay swamp 3)
              (_, board) = S.addCreature avatar S.alice gs0
              cast = S.runPure S.identityAnswer board (S.cast S.alice spellId)
              died = S.runPure S.identityAnswer cast Stack.resolveTop
              (placed, after) = fireTrigger died
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Graveyard S.alice died)) "it died into the graveyard"
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "and CR 701.24 put it into its owner's library"
          Spec.assertBool s (not (Set.member avatarName (namesIn Zone.Graveyard S.alice after))) "leaving the graveyard"
        -- "FROM ANYWHERE" doing real work, half one, and the falsifier for
        -- encoding this trigger as SelfDies: a discarded Serra Avatar never
        -- touched the battlefield, and CR 700.4's "dies" would have nothing to
        -- say about it.
        Spec.it s "CR 603.6 a Serra Avatar put into the graveyard from the HAND triggers" $ do
          (handCard, gs) <- cardIn S.addHandCard
          let discarded = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
              (placed, after) = fireTrigger discarded
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "it is in its owner's library"
          Spec.assertBool s (not (Set.member avatarName (namesIn Zone.Graveyard S.alice after))) "and no longer in the graveyard"
        -- Half two, and the falsifier for the other direction -- collapsing this
        -- condition into Narcomoeba's SelfPutIntoGraveyardFromLibrary. A mill
        -- fires BOTH conditions; the hand case above is what only this one sees.
        Spec.it s "CR 603.6 a Serra Avatar milled from the LIBRARY triggers" $ do
          (libraryCard, gs) <- cardIn S.addLibraryCard
          let milled = S.runPure S.identityAnswer gs (Event.changeZone libraryCard Zone.Graveyard)
              (placed, after) = fireTrigger milled
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack placed)) 1
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Library S.alice after)) "it is back in its owner's library"
        -- The DESTINATION is the whole condition, so it has to be load-bearing:
        -- an Avatar exiled off the battlefield left the battlefield just as
        -- surely and reached no graveyard, so this must stay silent where
        -- SelfLeavesTheBattlefield would fire.
        --
        -- Asserted against Event.matchesTrigger DIRECTLY, and deliberately not
        -- through a gathered scan: an exiled card is not among the graveyard
        -- candidates eventTriggers offers, so a scan-level assertion would pass
        -- whether or not this condition reads the destination at all. Both
        -- destinations are asserted from the one event shape, so the True side is
        -- what makes the False side mean something.
        Spec.it s "CR 603.6 only a GRAVEYARD destination matches: exile does not" $ do
          (creature, gs) <- cardIn S.addCreature
          let moveTo to = GameEvent.Moved (ZoneChange.MkZoneChange creature creature Zone.Battlefield to) S.emptyCharacteristics
              matches = Event.matchesTrigger gs creature S.alice TriggerCondition.SelfPutIntoGraveyardFromAnywhere
          Spec.assertBool s (matches (moveTo Zone.Graveyard)) "a graveyard-bound move matches"
          Spec.assertBool s (not (matches (moveTo Zone.Exile))) "an exile-bound move does not"
        -- The gameplay-level companion to the pair above: an Avatar exiled off
        -- the battlefield really does leave nothing on the stack.
        Spec.it s "CR 603.6 a Serra Avatar EXILED from the battlefield triggers nothing" $ do
          (creature, gs) <- cardIn S.addCreature
          let exiled = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped exiled) exiled))) []
          Spec.assertBool s (Set.member avatarName (namesIn Zone.Exile S.alice exiled)) "it is in exile"

-- CR 603.6c: leaves-the-battlefield abilities "trigger when a permanent moves
-- from the battlefield to another zone ... written as, but aren't limited to,
-- 'When [this object] leaves the battlefield, . . .' or 'Whenever [something]
-- is put into a graveyard from the battlefield, . . . .'" Doomed Traveler
-- prints the second of those in its abbreviated form: CR 700.4 says "the term
-- dies means 'is put into a graveyard from the battlefield.'"
--
-- CR 603.10a is what makes it more than a tenth condition: "Some zone-change
-- triggers look back in time. These are leaves-the-battlefield abilities ...",
-- so the match is against the game as it was IMMEDIATELY BEFORE the event. By
-- the time the scan runs, the Traveler is a card in a graveyard with a fresh id
-- (CR 400.7) and nothing is on the battlefield to find -- which is what makes
-- the token appearing at all the discriminating assertion here.
diesTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
diesTriggerSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), a Doomed Traveler in play,
      -- and the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      boltBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        doomedTraveler <- S.printingOf s registry "Doomed Traveler"
        let (_, withTraveler) = S.addCreature doomedTraveler S.alice (S.landsInPlay mountain 1)
        pure (S.handOne lightningBolt withTraveler)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g's state-based action destroys it and the CR 117.5 settle's OWN
      -- trigger scan must see that death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      spiritsOf pid gs =
        filter
          -- CR 111.4: Doomed Traveler does not specify the token's name, so the
          -- name is its subtype plus the word "Token".
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Spirit Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
      travelerName = CardName.MkCardName $ Text.pack "Doomed Traveler"
   in Spec.describe s "DiesTrigger" $ do
        -- The gameplay-level proof, cast to resolution, through a real
        -- removal spell and the state-based action it sets up.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Doomed Traveler and its dies trigger makes a flying Spirit" $ do
          board <- boltBoard
          let (settled, after) = boltIt board
          -- The trigger was gathered in the SAME settle that ran the SBA
          -- (Engine.settleForPriority: performStateBasedActions, then
          -- placePendingTriggers, then loop).
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          -- And it did so with the Traveler already gone: an implementation
          -- matching against the live battlefield would find nothing here.
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Graveyard S.alice settled)) "the Traveler is in the graveyard by then"
          Spec.assertBool s (not (Set.member travelerName (namesIn Zone.Battlefield S.alice settled))) "and not on the battlefield"
          case spiritsOf S.alice after of
            [spirit] -> do
              Spec.assertEqWith s "power" (Projection.powerOf spirit after) (Just 1)
              Spec.assertEqWith s "toughness" (Projection.toughnessOf spirit after) (Just 1)
              Spec.assertEqWith s "white" (Projection.colorsOf spirit after) (Set.singleton Color.White)
              Spec.assertEqWith s "Spirit" (Projection.subtypesOf spirit after) (Set.singleton Subtype.Spirit)
              Spec.assertBool s (Projection.hasKeyword Keyword.Type.Flying spirit after) "with flying"
            other -> Spec.assertFailure s ("expected exactly one Spirit token, got " <> show (length other))
        -- CR 700.4 doing real work: "dies" is NARROWER than CR 603.6c's
        -- leaves-the-battlefield. The same permanent moved from the
        -- battlefield to EXILE has left the battlefield and has not died.
        Spec.it s "CR 700.4 a Traveler exiled from the battlefield does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              exiled = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped exiled) exiled))) []
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Exile S.alice exiled)) "it is in exile"
        -- The other half of "from the battlefield": the same card discarded
        -- reaches the same graveyard and has not died (CR 700.4).
        Spec.it s "CR 700.4 a Traveler discarded from the HAND does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addHandCard doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              discarded = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped discarded) discarded))) []
        -- Self-scoped: SOME OTHER creature dying is not this Traveler's
        -- death, even though the Traveler is right there to see it.
        Spec.it s "CR 603.6c another creature dying does not fire the Traveler's trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withTraveler) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs) = S.addCreature piker S.alice withTraveler
              died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) []
        -- CR 603.3a through CR 603.10a's look-back: "the player who controlled
        -- the ability's source at the time it triggered" is read from the game
        -- as it was immediately BEFORE the death, so a Traveler bob owns but
        -- alice has stolen with Control Magic hands ALICE the Spirit. Reading
        -- the graveyard card's owner instead would answer bob.
        Spec.it s "CR 603.3a the trigger is controlled by whoever controlled the Traveler as it died" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          controlMagic <- S.printingOf s registry "Control Magic"
          let (traveler, withTraveler) = S.addCreature doomedTraveler S.bob (Setup.emptyGame S.bothPlayers)
              (aura, withAura) = S.addCreature controlMagic S.alice withTraveler
              stolen = S.attach aura traveler withAura
              died = S.runPure S.identityAnswer stolen (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "alice controlled it as it died" (Projection.controllerOf traveler stolen) (Just S.alice)
          Spec.assertEqWith s "so the trigger is hers, not its owner's" (fmap PendingTrigger.controller (fst (Event.gatherTriggers (Event.unscannedGrouped died) died))) [S.alice]

-- CR 603.6c's SECOND written form with a BYSTANDER bearer -- "Whenever
-- [something] is put into a graveyard from the battlefield", narrowed by CR
-- 700.4's "dies" -- and Meren of Clan Nel Toth {2}{B}{G} Legendary Creature --
-- Human Shaman 3/4, "Whenever another creature you control dies, you get an
-- experience counter", the card that proves it.
--
-- diesTriggerSpec above is the SELF-scoped half of the same rule: there the
-- bearer IS the permanent that died. Here the bearer watches, so the three
-- printed words that narrow the watching are three arms of the condition's own
-- Filter, exactly as Soul Warden's "another" is (#163) -- "creature" is
-- HasCardType, "you control" is ControlledBy You read against CR 109.5's you
-- (the ability's controller, CR 603.3a), and "another" is Not IsSource. Each
-- gets a falsifier below, because a condition that ignored any one of them
-- would still pass the whole-card case.
--
-- The dying permanent is read from CR 608.2h last known information, which is
-- not an implementation convenience but CR 603.10a ("some zone-change triggers
-- look back in time. These are leaves-the-battlefield abilities ...") read
-- through CR 603.10's own definition of looking back: "using the existence of
-- those abilities and the appearance of objects immediately prior to the
-- event." By the CR 117.5 boundary the creature is a card in a graveyard,
-- CR 108.4 says "a card doesn't have a controller unless that card represents a
-- permanent or spell", and CR 108.4a would hand a matcher reading the graveyard
-- card its OWNER instead -- so a creature its controller had stolen would be
-- credited back to the player who no longer had it. The Control Magic case at
-- the end is that falsifier.
--
-- Meren's SECOND ability -- "At the beginning of your end step, choose target
-- creature card in your graveyard. If that card's mana value is less than or
-- equal to the number of experience counters you have, return it to the
-- battlefield. Otherwise, put it into your hand." -- is not transcribed (#614).
--
-- Two cases here kill the bearer and another creature in one batch, and they are
-- a PAIR: CR 704.3 makes Day of Judgment's deaths one event, so Meren sees the
-- Piker that died alongside her, while a Salt Road Skirmish that destroys her and
-- then buries two tokens later in the same batch is a sequence, and she sees
-- neither. Both are needed -- admitting the whole batch passes the first and
-- answers the second 2 -- and the first is asserted for BOTH object-id orders,
-- since that is what makes it a test of the rule rather than of the id minting.
permanentDiesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentDiesSpec s registry =
  let anotherCreatureYouControl =
        Filter.Type.And
          [ Filter.Type.HasCardType CardType.Creature,
            Filter.Type.ControlledBy PlayerRelation.You,
            Filter.Type.Not Filter.Type.IsSource
          ]
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped gs) gs))
      experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      -- alice's Meren beside one creature of `victim`'s printing, controlled by
      -- `owner`.
      merenBeside victim owner gs0 = do
        meren <- S.printingOf s registry "Meren of Clan Nel Toth"
        printing <- S.printingOf s registry victim
        let (merenId, withMeren) = S.addCreature meren S.alice gs0
            (victimId, gs) = S.addCreature printing owner withMeren
        pure (merenId, victimId, gs)
   in Spec.describe s "PermanentDies" $ do
        -- The gameplay-level proof, cast to resolution: alice's Lightning Bolt
        -- kills her own Goblin Piker, CR 704.5g's state-based action moves it
        -- to the graveyard, and the CR 117.5 settle's trigger scan sees the
        -- death. One experience counter, from a card that started with none.
        Spec.it s "CR 700.4 whole cards: alice's Piker dies and her Meren gets an experience counter" $ do
          mountain <- S.printingOf s registry "Mountain"
          lightningBolt <- S.printingOf s registry "Lightning Bolt"
          (_, pikerId, board) <- merenBeside "Goblin Piker" S.alice (S.landsInPlay mountain 1)
          let (gs, spellId) = S.handOne lightningBolt board
              -- Bolt the Piker by id rather than by S.identityAnswer's least
              -- Recipient, which would aim at whichever creature sorts first.
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature pikerId))) sets
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (S.cast S.alice spellId)
              damaged = S.runPure answer cast Stack.resolveTop
              settled = S.runPure answer damaged Engine.settleForPriority
              after = S.runPure answer settled Stack.resolveTop
          Spec.assertEqWith s "alice starts with no experience" (experienceOf S.alice gs) 0
          Spec.assertEqWith s "the Piker is gone" (Game.lookupObject pikerId settled) Nothing
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and alice has exactly one experience counter" (experienceOf S.alice after) 1
          Spec.assertEqWith s "bob has none" (experienceOf S.bob after) 0
        -- CR 603.10a's own Example, played out: "Two creatures are on the
        -- battlefield along with an artifact that has the ability 'Whenever a
        -- creature dies, you gain 1 life.' Someone casts a spell that destroys
        -- all artifacts, creatures, and enchantments. The artifact's ability
        -- triggers twice, EVEN THOUGH THE ARTIFACT GOES TO ITS OWNER'S
        -- GRAVEYARD AT THE SAME TIME AS THE CREATURES." Meren is that artifact
        -- and the Piker is one of those creatures: CR 704.3 / CR 608.2f make
        -- Day of Judgment's two deaths ONE event, so the look-back reads a board
        -- on which Meren and the Piker were both still there.
        --
        -- BOTH ID ORDERS, which is the whole point rather than belt and braces:
        -- the two boards differ in nothing a rule can see, so an engine that
        -- answered by the order the ids were minted in would answer them
        -- differently. The Meren-first board is the one that used to answer 0.
        --
        -- Exactly ONE counter, not two: Meren's own death is excluded by the
        -- printed "another" (the falsifier below), so the count discriminates
        -- between seeing her group-mate and seeing her whole group.
        Spec.it s "CR 603.10a Meren sees the Piker that died alongside her, in either id order" $ do
          plains <- S.printingOf s registry "Plains"
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          dayOfJudgment <- S.printingOf s registry "Day of Judgment"
          let -- The two boards, differing only in which of the two creatures was
              -- minted first and so which one Event.destroyIn reaches first.
              board merenFirst =
                let base = Setup.emptyGame S.bothPlayers
                 in if merenFirst
                      then snd (S.addCreature piker S.alice (snd (S.addCreature meren S.alice base)))
                      else snd (S.addCreature meren S.alice (snd (S.addCreature piker S.alice base)))
              run merenFirst =
                let withLands = List.foldl' (\gs _ -> snd (S.addCreature plains S.alice gs)) (board merenFirst) [1 :: Int .. 4]
                    (withSpell, spell) = S.handOne dayOfJudgment withLands
                    afterCast = S.runPure S.identityAnswer withSpell (S.cast S.alice spell)
                    swept = S.runPure S.identityAnswer afterCast Stack.resolveTop
                    settled = S.runPure S.identityAnswer swept Engine.settleForPriority
                 in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
              (merenFirstSettled, merenFirstAfter) = run True
              (pikerFirstSettled, pikerFirstAfter) = run False
              creaturesLeft gs = Set.size (Set.filter (`Projection.isCreatureOf` gs) (GameState.battlefield gs))
          Spec.assertEqWith s "the sweep left no creatures either way" (fmap creaturesLeft [merenFirstSettled, pikerFirstSettled]) [0, 0]
          Spec.assertEqWith s "one trigger reached the stack with Meren minted first" (length (GameState.stack merenFirstSettled)) 1
          Spec.assertEqWith s "and one with the Piker minted first" (length (GameState.stack pikerFirstSettled)) 1
          Spec.assertEqWith s "alice has exactly one experience counter with Meren minted first" (experienceOf S.alice merenFirstAfter) 1
          Spec.assertEqWith s "and exactly one with the Piker minted first" (experienceOf S.alice pikerFirstAfter) 1
          Spec.assertEqWith s "bob has none either way" (fmap (experienceOf S.bob) [merenFirstAfter, pikerFirstAfter]) [0, 0]
        -- The control that keeps the case above from being answered by simply
        -- admitting everything in the batch. Three groups, ONE batch: alice's
        -- Salt Road Skirmish destroys her own Meren (CR 701.8), then creates two
        -- 1/1 Warrior tokens later in that same resolution, and the CR 117.5
        -- settle's first state-based-action pass buries both as 0/0 under Night
        -- of Souls' Betrayal (CR 704.5f). GameState.scannedThrough is not bumped
        -- until the trigger scan, so all three share a batch -- and none of them
        -- shares Meren's event.
        --
        -- ZERO counters is the rules answer: two creatures alice controls die
        -- STRICTLY AFTER Meren left, so CR 603.10a's "immediately prior to the
        -- event" reads a board she is not on. A look-back that took the whole
        -- batch rather than the event's own group would answer 2.
        --
        -- The target is pinned by ObjectId rather than left to
        -- S.identityAnswer's least Recipient, the way the Bolt above is.
        Spec.it s "CR 603.10a a Meren who died earlier in the batch sees neither token buried later in it" $ do
          swamp <- S.printingOf s registry "Swamp"
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          night <- S.printingOf s registry "Night of Souls' Betrayal"
          skirmish <- S.printingOf s registry "Salt Road Skirmish"
          let (merenId, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (_, withNight) = S.addCreature night S.alice withMeren
              withLands = List.foldl' (\gs _ -> snd (S.addCreature swamp S.alice gs)) withNight [1 :: Int .. 4]
              (withSpell, spell) = S.handOne skirmish withLands
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature merenId))) sets
                _ -> S.identityAnswer p
              afterCast = S.runPure answer withSpell (S.cast S.alice spell)
              resolved = S.runPure answer afterCast Stack.resolveTop
              settled = S.runPure answer resolved Engine.settleForPriority
          Spec.assertEqWith s "Meren was destroyed by her controller's own spell" (Game.lookupObject merenId settled) Nothing
          Spec.assertEqWith s "the two tokens entered and were buried as 0/0" (length (filter (\zc -> ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard) (S.zoneChangesOf settled))) 3
          Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack settled)) 0
          Spec.assertEqWith s "and alice has no experience counters at all" (experienceOf S.alice settled) 0
        -- "ANOTHER", the Filter's Not IsSource arm. Meren's own death IS a
        -- creature alice controls dying, so the silence has to come from the
        -- exclusion rather than from the condition failing to see the death at
        -- all -- and the second pair of assertions is what tells those apart:
        -- the same bearer, the same event, and a Filter differing only in the
        -- exclusion fires. That also exercises the CR 608.2h read on a bearer
        -- and a candidate that are one departed object.
        Spec.it s "CR 603.6c another: Meren's own death does not give her controller a counter" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          let anyCreatureYouControl =
                Filter.Type.And
                  [ Filter.Type.HasCardType CardType.Creature,
                    Filter.Type.ControlledBy PlayerRelation.You
                  ]
              (merenId, gs) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone merenId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
          case Event.unscannedEvents died of
            [death] -> do
              Spec.assertBool s (Event.matchesTrigger died merenId S.alice (TriggerCondition.PermanentDies anyCreatureYouControl) death) "a Filter without the exclusion admits Meren's own death"
              Spec.assertBool s (not (Event.matchesTrigger died merenId S.alice (TriggerCondition.PermanentDies anotherCreatureYouControl) death)) "so the printed \"another\" is the only thing declining it"
            other -> Spec.assertFailure s ("expected exactly one event, got " <> show (length other))
        -- "YOU CONTROL", the ControlledBy arm, read through CR 109.5 against
        -- the ability's controller (CR 603.3a). bob's creature dying in front
        -- of alice's Meren is not it.
        Spec.it s "CR 109.5 you control: an opponent's creature dying fires nothing" $ do
          (_, pikerId, gs) <- merenBeside "Goblin Piker" S.bob (Setup.emptyGame S.bothPlayers)
          let died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
        -- "CREATURE", the HasCardType arm. A land alice controls reaching the
        -- same graveyard from the same battlefield is not a creature dying.
        Spec.it s "CR 205.2a creature: a land of alice's dying fires nothing" $ do
          (_, landId, gs) <- merenBeside "Mountain" S.alice (Setup.emptyGame S.bothPlayers)
          let died = S.runPure S.identityAnswer gs (Event.changeZone landId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf died) []
        -- CR 700.4 doing the same work it does for SelfDies: "dies" is
        -- narrower than CR 603.6c's leaves-the-battlefield. alice's own
        -- creature EXILED off the battlefield has left it without dying.
        Spec.it s "CR 700.4 a creature of alice's exiled from the battlefield fires nothing" $ do
          (_, pikerId, gs) <- merenBeside "Goblin Piker" S.alice (Setup.emptyGame S.bothPlayers)
          let exiled = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Exile)
          Spec.assertEqWith s "nothing triggered" (sourcesOf exiled) []
        -- The same card discarded from a HAND reaches the same graveyard and
        -- has not died: the `from` half of CR 700.4.
        Spec.it s "CR 700.4 a creature card discarded from alice's hand fires nothing" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (handCard, gs) = S.addHandCard piker S.alice withMeren
              discarded = S.runPure S.identityAnswer gs (Event.changeZone handCard Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (sourcesOf discarded) []
        -- CR 603.10a / CR 608.2h: the dying creature is read as it was
        -- IMMEDIATELY PRIOR to the event. bob owns the Piker, alice has stolen
        -- it with Control Magic, and it dies -- so "a creature YOU control"
        -- holds for alice. A matcher reading the card that landed in the
        -- graveyard would take CR 108.4a's substitute for CR 108.4's missing
        -- controller -- its owner, bob -- and answer no.
        Spec.it s "CR 608.2h a stolen creature dying is read with the controller it had as it left" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          piker <- S.printingOf s registry "Goblin Piker"
          controlMagic <- S.printingOf s registry "Control Magic"
          let (merenId, withMeren) = S.addCreature meren S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, withPiker) = S.addCreature piker S.bob withMeren
              (aura, withAura) = S.addCreature controlMagic S.alice withPiker
              stolen = S.attach aura pikerId withAura
              died = S.runPure S.identityAnswer stolen (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "alice controlled it as it died" (Projection.controllerOf pikerId stolen) (Just S.alice)
          Spec.assertEqWith s "so her Meren triggered" (sourcesOf died) [TriggerSource.OfObject merenId]
        -- The condition is the card's, not this spec's: the printed Filter is
        -- what the matcher is asked about everywhere above.
        Spec.it s "Meren's printed condition is PermanentDies over another creature you control" $ do
          meren <- S.printingOf s registry "Meren of Clan Nel Toth"
          Spec.assertEqWith
            s
            "one triggered ability, with that condition"
            (fmap TriggeredAbility.condition (Face.triggeredAbilities (S.combinedFace meren)))
            [TriggerCondition.PermanentDies anotherCreatureYouControl]

-- CR 603.6c's FIRST written form, and the whole of its first clause:
-- "Leaves-the-battlefield abilities trigger when a permanent moves from the
-- battlefield to another zone ... written as, but aren't limited to, 'When
-- [this object] leaves the battlefield, . . . .'" ANY other zone -- which is the
-- whole of what separates it from the SECOND written form, the one CR 700.4
-- abbreviates as "dies" and diesTriggerSpec above covers.
--
-- Thragtusk, {4}{G} Creature -- Beast 5/3: "When this creature enters, you gain
-- 5 life. When this creature leaves the battlefield, create a 3/3 green Beast
-- creature token." The two halves of the same card are here because the enters
-- trigger is what proves the leaves trigger is not merely firing on every zone
-- change the permanent is party to.
--
-- Unsummon is what makes CR 400.7e's public-zone proviso a real test rather
-- than one satisfied by construction, which is all it could be while SelfDies
-- was the only look-back condition: a bounced Thragtusk HAS left the
-- battlefield, so the ability triggers, but it went to a hand, which CR 400.2
-- makes a hidden zone -- so "can find the new object that it became in the zone
-- it moved to when the ability triggered, IF THAT ZONE IS A PUBLIC ZONE"
-- withholds Pawl.Engine.Binding.became.
--
-- Doomed Traveler bounced is the regression guard on the other side: the two
-- conditions must not be conflated, so the card that prints "dies" must stay
-- silent for exactly the event that fires the card that prints "leaves the
-- battlefield".
leavesBattlefieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
leavesBattlefieldSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), a Thragtusk in play, and the
      -- Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      -- Three damage is lethal to a 5/3 (CR 704.5g).
      boltBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        thragtusk <- S.printingOf s registry "Thragtusk"
        let (tusk, withTusk) = S.addCreature thragtusk S.alice (S.landsInPlay mountain 1)
        pure (tusk, S.handOne lightningBolt withTusk)
      -- The same board with a bounce spell instead of a burn spell, for the
      -- creature the caller names. One Island pays Unsummon's {U}.
      bounceBoard printing = do
        island <- S.printingOf s registry "Island"
        unsummon <- S.printingOf s registry "Unsummon"
        victim <- S.printingOf s registry printing
        let (oid, withVictim) = S.addCreature victim S.alice (S.landsInPlay island 1)
        pure (oid, S.handOne unsummon withVictim)
      -- Cast the one spell in hand, resolve it, then settle -- CR 117.5's
      -- boundary is where state-based actions run and the trigger scan sees the
      -- departure -- and hand back both the settled state (the trigger on the
      -- stack) and the state after the trigger itself resolves.
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            resolved = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer resolved Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      beastsOf pid gs =
        filter
          -- CR 111.4: Thragtusk does not specify the token's name, so the name
          -- is its subtype plus the word "Token".
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Beast Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
      assertOneBeast after =
        case beastsOf S.alice after of
          [beast] -> do
            Spec.assertEqWith s "3/3" (Projection.powerOf beast after, Projection.toughnessOf beast after) (Just 3, Just 3)
            -- CR 202.2b/202.2e: a token has no mana cost, so the colour
            -- indicator is the only thing making it green.
            Spec.assertEqWith s "green" (Projection.colorsOf beast after) (Set.singleton Color.Green)
            Spec.assertEqWith s "Beast" (Projection.subtypesOf beast after) (Set.singleton Subtype.Beast)
          other -> Spec.assertFailure s ("expected exactly one Beast token, got " <> show (length other))
      tuskName = CardName.MkCardName $ Text.pack "Thragtusk"
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      -- Every slot stamped on every object currently on the stack, which for
      -- these boards is the one placed trigger.
      stackSlots gs =
        concatMap (Map.toList . Map.mapMaybe Binding.onlyOne . Binding.targetsOf . maybe Map.empty Object.bindings . flip Game.lookupObject gs) (GameState.stack gs)
   in Spec.describe s "LeavesTheBattlefield" $ do
        -- The destination this condition SHARES with "dies", so the wider
        -- condition is not merely the narrower one's complement: a Thragtusk
        -- that dies has also left the battlefield.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Thragtusk and its leaves-the-battlefield trigger makes a 3/3 Beast" $ do
          (_, board) <- boltBoard
          let (settled, after) = castIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member tuskName (namesIn Zone.Graveyard S.alice settled)) "the Thragtusk is in the graveyard by then"
          assertOneBeast after
        -- The destination "dies" does NOT reach, and the reason this condition
        -- has to exist at all (CR 700.4 is a graveyard, CR 603.6c is any zone).
        -- The hand is also a HIDDEN zone (CR 400.2), which is what makes the
        -- next case a real branch rather than a proviso.
        Spec.it s "CR 603.6c whole card: Unsummon bounces Thragtusk and the leaves-the-battlefield trigger still makes a 3/3 Beast" $ do
          (_, board) <- bounceBoard "Thragtusk"
          let (settled, after) = castIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member tuskName (namesIn Zone.Hand S.alice settled)) "the Thragtusk is in its owner's hand, not a graveyard"
          Spec.assertBool s (not (Set.member tuskName (namesIn Zone.Graveyard S.alice settled))) "and nowhere near a graveyard"
          assertOneBeast after
        -- CR 400.7e's proviso, which is the new plumbing this condition needed:
        -- "can find the new object that it became in the zone it moved to when
        -- the ability triggered, IF THAT ZONE IS A PUBLIC ZONE." A hand is not
        -- one (CR 400.2), so the slot must be ABSENT -- naming the card in hand
        -- would hand the ability an object the rule does not let it find.
        --
        -- CR 113.7a's source slot is still stamped, and is asserted here so
        -- that the absence above is read as a decision about `became` rather
        -- than as a trigger that was placed with no bindings at all.
        Spec.it s "CR 400.7e a bounce to a HIDDEN zone binds no became slot, though the source slot is still stamped" $ do
          (tusk, board) <- bounceBoard "Thragtusk"
          let (settled, _) = castIt board
              slots = stackSlots settled
          Spec.assertEqWith s "the departed permanent is CR 113.7a's source" (lookup Binding.triggerSource slots) (Just (Recipient.ToObject tusk))
          Spec.assertEqWith s "and CR 400.7e's became is absent for a hidden destination" (lookup Binding.became slots) Nothing
        -- The public destination, side by side with the hidden one: the same
        -- condition, the same card, and the slot IS bound -- so its absence
        -- above is CR 400.7e's proviso doing work rather than the condition
        -- simply never binding anything.
        Spec.it s "CR 400.7e a death to a PUBLIC zone does bind became, for the same condition" $ do
          (tusk, board) <- boltBoard
          let (settled, _) = castIt board
              slots = stackSlots settled
          Spec.assertEqWith s "the departed permanent is still CR 113.7a's source" (lookup Binding.triggerSource slots) (Just (Recipient.ToObject tusk))
          case lookup Binding.became slots of
            Just (Recipient.ToObject graveyardId) -> do
              Spec.assertBool s (graveyardId /= tusk) "became is the CR 400.7 incarnation, a different id"
              Spec.assertEqWith s "and it is the graveyard card" (fmap Face.name (Game.faceOf graveyardId settled)) (Just tuskName)
            other -> Spec.assertFailure s ("expected became to name an object, got " <> show other)
        -- THE REGRESSION GUARD. Doomed Traveler prints "dies", not "leaves the
        -- battlefield", and CR 700.4 makes that a graveyard and nothing else. A
        -- bounce is the event that fires Thragtusk two cases up, so conflating
        -- the two conditions would show up here as a Spirit that should not
        -- exist.
        Spec.it s "CR 700.4 a Doomed Traveler bounced by Unsummon does NOT fire its dies trigger" $ do
          (_, board) <- bounceBoard "Doomed Traveler"
          let (settled, _) = castIt board
          Spec.assertBool s (Set.member (CardName.MkCardName $ Text.pack "Doomed Traveler") (namesIn Zone.Hand S.alice settled)) "the Traveler left the battlefield for a hand"
          Spec.assertEqWith s "and nothing triggered" (length (GameState.stack settled)) 0
          Spec.assertEqWith s "so no Spirit was made" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Spirit Token") S.alice settled) 0
        -- CR 400.7e's proviso in isolation, one case per zone CR 400.2
        -- classifies, so the branch is pinned to the RULE rather than to the two
        -- destinations the boards above happen to reach. "Graveyard,
        -- battlefield, stack, exile, ante, and command are public zones ...
        -- Library and hand are hidden zones."
        Spec.it s "CR 400.2 eventBindings binds became for every PUBLIC destination and for no hidden one" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              leftFor to = Event.eventBindings TriggerCondition.SelfLeavesTheBattlefield (GameEvent.Moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield to) S.emptyCharacteristics)
              bound = Map.singleton Binding.became (Binding.toObject arrived)
          Spec.assertEqWith s "a graveyard is public" (leftFor Zone.Graveyard) bound
          Spec.assertEqWith s "exile is public" (leftFor Zone.Exile) bound
          Spec.assertEqWith s "the stack is public" (leftFor Zone.Stack) bound
          Spec.assertEqWith s "the command zone is public" (leftFor Zone.Command) bound
          Spec.assertEqWith s "a hand is hidden" (leftFor Zone.Hand) Map.empty
          Spec.assertEqWith s "a library is hidden" (leftFor Zone.Library) Map.empty
        -- CR 603.6c's "to ANOTHER zone", which is the one destination this
        -- condition rejects. Pawl.Engine.Event.recordTokenEntry files a
        -- battlefield-to-battlefield pseudo-move whose departed id is the new
        -- token's own, so a token bearing this ability would fire on its own
        -- creation if the guard were dropped -- and no card in the pool makes
        -- such a token, which is exactly why the guard needs a test of its own.
        Spec.it s "CR 603.6c a battlefield-to-battlefield pseudo-move is not a departure" $ do
          let token = ObjectId.MkObjectId 1
              entry = GameEvent.Moved (ZoneChange.MkZoneChange token token Zone.Battlefield Zone.Battlefield) S.emptyCharacteristics
              gone = GameEvent.Moved (ZoneChange.MkZoneChange token (ObjectId.MkObjectId 2) Zone.Battlefield Zone.Exile) S.emptyCharacteristics
              matches = Event.matchesTrigger (Setup.emptyGame S.bothPlayers) token S.alice TriggerCondition.SelfLeavesTheBattlefield
          Spec.assertBool s (not (matches entry)) "a token's own entry is not a departure"
          Spec.assertBool s (matches gone) "but the same token being exiled is"
        -- The card's other half, and the proof that the leaves trigger is
        -- scoped to DEPARTURES: Thragtusk arriving on the battlefield is a zone
        -- change involving the same permanent, and only the enters trigger may
        -- see it.
        Spec.it s "CR 603.6a whole card: casting Thragtusk gains 5 life, and its leaves trigger stays silent on the way in" $ do
          forest <- S.printingOf s registry "Forest"
          thragtusk <- S.printingOf s registry "Thragtusk"
          let (settled, after) = castIt (S.handOne thragtusk (S.landsInPlay forest 5))
          Spec.assertEqWith s "exactly one trigger, the enters one" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "alice is still at 20 until it resolves" (S.lifeOf S.alice settled) (Just 20)
          Spec.assertEqWith s "and at 25 once it does" (S.lifeOf S.alice after) (Just 25)
          Spec.assertEqWith s "with no Beast token anywhere" (length (beastsOf S.alice after)) 0

-- CR 601.2i's second sentence -- "any abilities that trigger when a spell is
-- cast or put onto the stack trigger at this time" -- which is the whole trigger
-- event TriggerCondition.SpellCast matches.
--
-- Young Pyromancer, {1}{R} Creature -- Human Shaman 2/1: "Whenever you cast an
-- instant or sorcery spell, create a 1/1 red Elemental creature token." Two
-- narrowings in one printed sentence, and the Filter carries both -- "you cast"
-- is Filter.ControlledBy You against CR 109.5's "you" (CR 603.3a), "an instant
-- or sorcery spell" a disjunction of Filter.HasCardType -- so a board that moved
-- only one of them at a time could not tell a working Filter from one that
-- always passes. Each case below moves exactly one.
--
-- Boil, {3}{R} Instant "Destroy all Islands", is the spell cast: it TARGETS
-- NOTHING, so no answerer choice enters the fixture, and no player here controls
-- an Island, so its resolution changes nothing that an assertion reads. The
-- Elemental token is therefore the only thing the cast can put on the
-- battlefield.
--
-- THREE seats. At two players every board has exactly one non-controller, so
-- "the caster is not you" and "the caster is that one opponent" are the same
-- sentence and a Filter that confused them would still answer right. carol is
-- the seat that is neither the caster nor the ability's controller, and the
-- opponent case below names all three players in its assertions.
youngPyromancerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
youngPyromancerSpec s registry =
  let elemental = CardName.MkCardName (Text.pack "Elemental Token")
      elementalsOf = S.countOnBattlefieldByName elemental
      -- alice has Young Pyromancer and four Mountains, bob four Mountains, carol
      -- nothing at all. Four each is Boil's {3}{R}, and covers Goblin Piker's
      -- {2}{R} with one to spare.
      board mountain pyromancer =
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature mountain pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 4 S.threePlayerGame)
            (_, withPyromancer) = S.addCreature pyromancer S.alice withLands
         in withPyromancer
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
   in Spec.describe s "SpellCast" $ do
        -- THE case: the trigger fires at all, and the token it makes is the one
        -- the ability names rather than merely something arriving on the stack.
        Spec.it s "CR 601.2i casting an instant fires Young Pyromancer" $ do
          mountain <- S.printingOf s registry "Mountain"
          pyromancer <- S.printingOf s registry "Young Pyromancer"
          boil <- S.printingOf s registry "Boil"
          let (boilId, gs) = S.addHandCard boil S.alice (board mountain pyromancer)
              after = castAndResolve S.alice boilId gs
          Spec.assertEqWith s "no Elemental before the cast" (elementalsOf S.alice gs) 0
          Spec.assertEqWith s "exactly one Elemental token afterwards" (elementalsOf S.alice after) 1
        -- The card-type half of the Filter, moved on its own: alice still casts,
        -- and only what she casts changes. A Filter that admitted everything and
        -- one that read the type correctly are indistinguishable without this.
        Spec.it s "CR 601.2i a CREATURE spell fires nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          pyromancer <- S.printingOf s registry "Young Pyromancer"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = S.addHandCard piker S.alice (board mountain pyromancer)
              after = castAndResolve S.alice pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer and not a fixture that
          -- never cast anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and no Elemental token" (elementalsOf S.alice after) 0
        -- The "you" half, moved on its own: the same instant, cast from the seat
        -- to alice's left instead of hers. carol makes the board three-handed,
        -- so "bob cast it" is not the same statement as "an opponent cast it".
        Spec.it s "CR 109.5 'you cast': an OPPONENT's instant fires nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          pyromancer <- S.printingOf s registry "Young Pyromancer"
          boil <- S.printingOf s registry "Boil"
          let base = board mountain pyromancer
              (bobsBoil, withBobs) = S.addHandCard boil S.bob base
              (alicesBoil, gs) = S.addHandCard boil S.alice withBobs
              byBob = castAndResolve S.bob bobsBoil gs
              byAlice = castAndResolve S.alice alicesBoil gs
          Spec.assertEqWith s "alice gets no Elemental from bob's cast" (elementalsOf S.alice byBob) 0
          Spec.assertEqWith s "and neither does bob" (elementalsOf S.bob byBob) 0
          Spec.assertEqWith s "and neither does carol" (elementalsOf S.carol byBob) 0
          -- The same board, one caster apart: alice casting her own copy is what
          -- proves the seat is the only thing the silence above turns on.
          Spec.assertEqWith s "the same board fires for alice's own cast" (elementalsOf S.alice byAlice) 1

-- CR 113.6k: the first ability in the pool that functions from the STACK. The
-- same rule that put Narcomoeba's in a graveyard, one zone over.
--
-- Desolation Twin, {10} Creature -- Eldrazi 10/10: "When you cast this spell,
-- create a 10/10 colorless Eldrazi creature token." Chosen from the cast-trigger
-- family because it is the one member whose WHOLE printed text pawl can write:
-- every other printing in that family wants CR 707.10's copy-a-spell or CR
-- 118.12's positive half (#701). Nothing of this card is omitted.
--
-- The bearer is the SPELL, which is what makes this a zone test rather than
-- another SpellCast case: at CR 601.2i the Twin is on nobody's battlefield and in
-- nobody's graveyard, so every candidate source but Event.eventTriggers'
-- `spellCast` misses it entirely, and the token below never appears.
desolationTwinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
desolationTwinSpec s registry =
  let eldrazi = CardName.MkCardName (Text.pack "Eldrazi Token")
      eldraziOf = S.countOnBattlefieldByName eldrazi
      -- Ten Mountains, which is the Twin's {10} exactly and Goblin Piker's
      -- {1}{R} with plenty to spare -- the negative case below casts on the same
      -- board, so mana can never be what separates the two.
      board mountain =
        let withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) (Setup.emptyGame S.bothPlayers) [1 .. (10 :: Int)]
         in withLands
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
   in Spec.describe s "SelfCast" $ do
        -- THE case: an ability borne by an object on the stack fires at all.
        Spec.it s "CR 113.6k Desolation Twin's cast trigger fires from the stack" $ do
          mountain <- S.printingOf s registry "Mountain"
          twin <- S.printingOf s registry "Desolation Twin"
          let (twinId, gs) = S.addHandCard twin S.alice (board mountain)
              after = castAndResolve S.alice twinId gs
          Spec.assertEqWith s "no Eldrazi token before the cast" (eldraziOf S.alice gs) 0
          -- Positive control: the spell really resolved, so the token below is
          -- the trigger's and not a fixture that never cast anything.
          Spec.assertEqWith s "the Twin itself resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName twin) S.alice after) 1
          Spec.assertEqWith s "and its cast trigger made exactly one token" (eldraziOf S.alice after) 1
        -- The same board and the same caster, one spell apart. A fence on the
        -- candidate source's SCOPE rather than on the condition: `spellCast`
        -- offers the cast spell alone, so a source that reached into the hand or
        -- swept the whole stack would make a token here. No mutation of the code
        -- as it stands turns this red.
        Spec.it s "CR 601.2i a different card's cast fires nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          twin <- S.printingOf s registry "Desolation Twin"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withTwin) = S.addHandCard twin S.alice (board mountain)
              (pikerId, gs) = S.addHandCard piker S.alice withTwin
              after = castAndResolve S.alice pikerId gs
          Spec.assertEqWith s "the Piker resolved" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and the Twin in hand made no token" (eldraziOf S.alice after) 0

-- CR 601.2i's trigger reading back the spell it watched: the reserved slot
-- Event.eventBindings stamps for that condition (Binding.castSpell), and the
-- first payload that acts on the WATCHED OBJECT rather than merely counting the
-- event.
--
-- Presence of the Master, {3}{W} Enchantment: "Whenever a player casts an
-- enchantment spell, counter it." Chosen over Thousand-Year Storm's "copy it for
-- each other instant and sorcery spell you've cast before it this turn" because
-- the payload is a rule 701 keyword action pawl already has (Effect.Counter, CR
-- 701.6a) rather than CR 707.10's copy-a-spell, and the printed "it" is the bound
-- spell with nothing else attached -- no count, no new targets.
--
-- WHAT THE BOARD KEEPS APART. The bearer and the watched spell must be
-- observably different objects, or a payload that acted on its own source would
-- pass: alice's Presence sits on the BATTLEFIELD while the spell it counters is
-- bob's, on the STACK, and the assertions name Presence's survival alongside the
-- spell's removal. Countering the bearer is not merely wrong here, it is
-- impossible -- CR 701.6a acts on the stack -- so a bearer-bound slot leaves the
-- enchantment spell to resolve and the first case below fails.
--
-- THREE SEATS, and the printed subject is why: "a player casts" is not "you
-- cast" and not "an opponent casts", and at two players those three readings all
-- coincide on any single cast. bob's cast rules out ControlledBy You, alice's own
-- cast rules out ControlledBy Opponent, and carol is the seat that makes
-- "opponent" more than a synonym for "the other player".
presenceOfTheMasterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
presenceOfTheMasterSpec s registry =
  let graveyardOf pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      -- alice bears Presence; alice and bob each get three Swamps and three
      -- Mountains, which is Bad Moon's {1}{B} and Goblin Piker's {1}{R} with
      -- room to spare. carol gets nothing: she is the third seat, not a caster.
      board swamp mountain presence =
        let addLands pid n printing g = List.foldl' (\g' _ -> snd (S.addCreature printing pid g')) g [1 .. (n :: Int)]
            withLands =
              addLands S.bob 3 mountain
                . addLands S.bob 3 swamp
                . addLands S.alice 3 mountain
                $ addLands S.alice 3 swamp S.threePlayerGame
            (_, withPresence) = S.addCreature presence S.alice withLands
         in withPresence
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
   in Spec.describe s "SpellCast binds the spell" $ do
        -- THE case: the trigger reaches the object the event named. Bad Moon is
        -- an inert static enchantment, so nothing but the counter can move it.
        Spec.it s "CR 701.6a Presence of the Master counters the enchantment spell it watched" $ do
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          presence <- S.printingOf s registry "Presence of the Master"
          badMoon <- S.printingOf s registry "Bad Moon"
          let (moonId, gs) = S.addHandCard badMoon S.bob (board swamp mountain presence)
              after = castAndResolve S.bob moonId gs
          Spec.assertEqWith s "nothing in bob's graveyard before the cast" (graveyardOf S.bob gs) 0
          Spec.assertEqWith s "Bad Moon never reaches the battlefield" (S.countOnBattlefieldByName (S.printingName badMoon) S.bob after) 0
          Spec.assertEqWith s "CR 701.6a puts it in its owner's graveyard" (graveyardOf S.bob after) 1
          -- The bearer, unharmed: the slot named the spell and not the source.
          Spec.assertEqWith s "and Presence of the Master is still on the battlefield" (S.countOnBattlefieldByName (S.printingName presence) S.alice after) 1
        -- The Filter half, moved on its own: the same caster, a spell of the
        -- wrong card type. Without it a condition that admitted every cast and
        -- one that read the type would be indistinguishable.
        Spec.it s "CR 601.2i a CREATURE spell is not countered" $ do
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          presence <- S.printingOf s registry "Presence of the Master"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = S.addHandCard piker S.bob (board swamp mountain presence)
              after = castAndResolve S.bob pikerId gs
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.bob after) 1
          Spec.assertEqWith s "and nothing went to bob's graveyard" (graveyardOf S.bob after) 0
        -- "A player", not "you" and not "an opponent": the bearer's own
        -- controller is a player too, so alice's enchantment dies to her own
        -- Presence. The case bob's cast above cannot make.
        Spec.it s "CR 601.2i 'a player casts' includes the bearer's controller" $ do
          swamp <- S.printingOf s registry "Swamp"
          mountain <- S.printingOf s registry "Mountain"
          presence <- S.printingOf s registry "Presence of the Master"
          badMoon <- S.printingOf s registry "Bad Moon"
          let (moonId, gs) = S.addHandCard badMoon S.alice (board swamp mountain presence)
              after = castAndResolve S.alice moonId gs
          Spec.assertEqWith s "alice's own Bad Moon never reaches the battlefield" (S.countOnBattlefieldByName (S.printingName badMoon) S.alice after) 0
          Spec.assertEqWith s "it is in alice's graveyard" (graveyardOf S.alice after) 1
          Spec.assertEqWith s "bob's graveyard is untouched" (graveyardOf S.bob after) 0
          Spec.assertEqWith s "and carol's" (graveyardOf S.carol after) 0

-- CR 601.2i's trigger reading back the PLAYER it watched, which is the other
-- half of the event: Binding.triggerPlayer stamped off GameEvent.SpellCast's
-- PlayerId, alongside the spell Binding.castSpell already holds.
--
-- Kambal, Consul of Allocation, {1}{W}{B} Legendary Creature -- Human Advisor
-- 2/3: "Whenever an opponent casts a noncreature spell, that player loses 2 life
-- and you gain 2 life." The plainest printing that names the caster and reaches
-- them through the EVENT rather than through the spell -- CR 112.2 makes the
-- spell's controller derivable from the spell, but CR 608.2h leaves the spell
-- possibly gone by the time the ability resolves, so the player is bound in its
-- own right.
--
-- "An opponent casts" needs nothing bound: Event.matchesTrigger's SpellCast arm
-- hands the event's caster to Projection.viewOfSpell as the spell's controller
-- (CR 601.2a), so Filter.ControlledBy Opponent answers the printed relation
-- against CR 109.5's "you" (CR 603.3a). It is the PAYLOAD's "that player" that
-- needs the slot.
--
-- THREE SEATS, and this is the test that needs them most: at two players "that
-- player" and "each opponent" name the same person, so a two-handed board cannot
-- tell Kambal's PlayerRef.InSlot thatPlayer from a wrong PlayerRef.Relative
-- Opponent. carol is the opponent who is NOT the caster, and her life total is
-- what separates the two authorings.
--
-- ONE TUPLE, not three assertions: the card prints 2 for both halves, so alice's
-- +2 and bob's -2 are the same magnitude and separate checks could agree for the
-- wrong reason. CR 119.3 is what moves each total.
--
-- Boil, {3}{R} Instant "Destroy all Islands", is the noncreature spell: it
-- TARGETS NOTHING, so no answerer choice enters the fixture, and no player here
-- controls an Island, so its resolution moves nothing an assertion reads.
kambalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kambalSpec s registry =
  let -- alice bears Kambal and nothing else; bob gets four Mountains, which is
      -- Boil's {3}{R} and Goblin Piker's {2}{R}. carol gets nothing at all: she
      -- is the third seat, not a caster.
      board mountain kambal =
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature mountain pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 S.threePlayerGame
            (_, withKambal) = S.addCreature kambal S.alice withLands
         in withKambal
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "SpellCast binds the caster" $ do
        -- THE case: the payload reaches the player the EVENT named, and not the
        -- other opponent. A wrong PlayerRef.Relative Opponent authoring drops
        -- carol to 18 as well, which this tuple sees.
        Spec.it s "CR 112.2 Kambal's 'that player' is the opponent who cast it" $ do
          mountain <- S.printingOf s registry "Mountain"
          kambal <- S.printingOf s registry "Kambal, Consul of Allocation"
          boil <- S.printingOf s registry "Boil"
          let (boilId, gs) = S.addHandCard boil S.bob (board mountain kambal)
              after = castAndResolve S.bob boilId gs
          Spec.assertEqWith s "everyone starts at 20" (lives gs) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "CR 119.3: bob loses 2, alice gains 2, carol is untouched" (lives after) (Just 22, Just 18, Just 20)
        -- The "noncreature" half of the Filter, moved on its own: the same
        -- caster, a spell of the wrong card type. Without it a condition that
        -- admitted every opponent's cast would be indistinguishable.
        Spec.it s "CR 601.2i a CREATURE spell fires nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          kambal <- S.printingOf s registry "Kambal, Consul of Allocation"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = S.addHandCard piker S.bob (board mountain kambal)
              after = castAndResolve S.bob pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer rather than a fixture that
          -- never cast anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.bob after) 1
          Spec.assertEqWith s "and nobody's life total moved" (lives after) (Just 20, Just 20, Just 20)

-- CR 601.2i's trigger narrowed by WHOSE TURN the cast happened on, which is a
-- second axis beside the Filter: CR 601.2i says nothing about the turn, and CR
-- 117.1a lets an instant be cast on anybody's, so the restriction has to come
-- from the condition. Pawl.Types.TurnScope is the type that says it, the same
-- one TriggerCondition.StepBegins carries.
--
-- Brineborn Cutthroat, {1}{U} Creature -- Merfolk Pirate 2/1: "Flash. Whenever
-- you cast a spell during an opponent's turn, put a +1/+1 counter on this
-- creature." Two narrowings again, on two different axes -- "you cast" is
-- Filter.ControlledBy You against CR 109.5's "you" (CR 603.3a), and "during an
-- opponent's turn" is TurnScope.OpponentsTurn read against the same player --
-- and only the second is new here.
--
-- Fog, {G} Instant "Prevent all combat damage that would be dealt this turn", is
-- the spell cast: it TARGETS NOTHING, so no answerer choice enters the fixture,
-- and no combat happens here, so its resolution moves nothing an assertion
-- reads.
--
-- THREE SEATS, and this is what earns the third: at two players "the active
-- player is not you" and "the active player is bob" are the same sentence, so a
-- scope that had hard-coded the one other seat would still answer right. carol's
-- turn is the case only a third seat can make.
--
-- THE TURNS ARE SET ON THE FIXTURE rather than played out. Whose turn it is
-- reaches the condition as GameState.activePlayer and nothing else, so three
-- assignments say exactly what three turn cycles would -- and CR 104.3c stays
-- out of it, three untap/draw steps at three seats being three chances to deck a
-- fixture library.
--
-- BOTH the counter and the projected power are asserted, because CR 122.1a is
-- what makes the counter mean anything: a counter that landed but never reached
-- the CR 613.4c layer would leave the count right and the creature a 2/1.
brinebornCutthroatSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
brinebornCutthroatSpec s registry =
  let -- alice bears the Cutthroat and three Forests, one per Fog: no untap step
      -- runs between the casts below, so the lands are not reused. bob and carol
      -- get nothing at all -- they are turns here, not casters.
      board forest cutthroat =
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature forest pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.alice 3 S.threePlayerGame
            (cutthroatId, withCutthroat) = S.addCreature cutthroat S.alice withLands
         in ( cutthroatId,
              withCutthroat
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      -- alice keeps priority throughout: CR 117.1a lets her cast an instant on
      -- anybody's turn, which is the whole premise of the card.
      onTurnOf pid gs = gs {GameState.activePlayer = pid, GameState.priority = Just S.alice}
      countersOn = S.counterOf CounterKind.PlusOnePlusOne
   in Spec.describe s "SpellCast during an opponent's turn" $ do
        -- THE case, in one run so the counts accumulate: the same caster and the
        -- same spell three times over, one turn apart each.
        Spec.it s "CR 601.2i Brineborn Cutthroat counts only the casts on another player's turn" $ do
          forest <- S.printingOf s registry "Forest"
          fog <- S.printingOf s registry "Fog"
          cutthroat <- S.printingOf s registry "Brineborn Cutthroat"
          let (cutthroatId, base) = board forest cutthroat
              (fog1, g1) = S.addHandCard fog S.alice base
              (fog2, g2) = S.addHandCard fog S.alice g1
              (fog3, g3) = S.addHandCard fog S.alice g2
              afterAlice = castAndResolve S.alice fog1 (onTurnOf S.alice g3)
              afterBob = castAndResolve S.alice fog2 (onTurnOf S.bob afterAlice)
              afterCarol = castAndResolve S.alice fog3 (onTurnOf S.carol afterBob)
              graveyardOf gs = length (Game.zoneMembers Zone.Graveyard S.alice gs)
          Spec.assertEqWith s "no counter before anything is cast" (countersOn cutthroatId g3) 0
          -- Positive control: all three casts really happened and really
          -- resolved, so any silence below is the scope's answer rather than a
          -- fixture that ran out of mana on the second Fog.
          Spec.assertEqWith s "each Fog resolved into alice's graveyard in turn" (graveyardOf afterAlice, graveyardOf afterBob, graveyardOf afterCarol) (1, 2, 3)
          -- ONE TUPLE over the three turns rather than three assertions, so a
          -- scope read the wrong way round shows its whole trajectory at once:
          -- alice's own turn is the seat that must NOT count, bob's is the first
          -- that must, and carol's is the seat that is neither the caster nor the
          -- one other player -- which is what "an opponent's" has to mean (CR
          -- 102.2, CR 806.1).
          Spec.assertEqWith
            s
            "only bob's and carol's turns put a counter on"
            (countersOn cutthroatId afterAlice, countersOn cutthroatId afterBob, countersOn cutthroatId afterCarol)
            (0, 1, 2)
          -- And the same three states read through the CR 613.4c layer, so a
          -- counter that landed without reaching the projected P/T is caught.
          Spec.assertEqWith
            s
            "CR 122.1a moves the printed 2/1 with them"
            (S.powerToughnessOf cutthroatId afterAlice, S.powerToughnessOf cutthroatId afterBob, S.powerToughnessOf cutthroatId afterCarol)
            (Just (2, 1), Just (3, 2), Just (4, 3))
        -- CR 702.8a's flash, which the trigger above does not touch: casting an
        -- INSTANT on an opponent's turn is CR 117.1a and says nothing about the
        -- Cutthroat's own keyword. Goblin Piker is the control -- an ordinary
        -- creature spell, in the same hand on the same turn with its mana paid
        -- for -- so the only difference between the two answers is the keyword.
        Spec.it s "CR 702.8a flash lets the Cutthroat itself be cast on an opponent's turn" $ do
          island <- S.printingOf s registry "Island"
          mountain <- S.printingOf s registry "Mountain"
          cutthroat <- S.printingOf s registry "Brineborn Cutthroat"
          piker <- S.printingOf s registry "Goblin Piker"
          let addLands printing pid n g = List.foldl' (\g' _ -> snd (S.addCreature printing pid g')) g [1 .. (n :: Int)]
              lands = addLands mountain S.alice 3 (addLands island S.alice 2 S.threePlayerGame)
              (cutthroatId, withCutthroat) = S.addHandCard cutthroat S.alice lands
              (pikerId, gs) = S.addHandCard piker S.alice withCutthroat
              bobsTurn = (onTurnOf S.bob gs) {GameState.phase = Phase.PrecombatMain}
          Spec.assertBool s (S.castable S.alice cutthroatId bobsTurn) "flash makes the Cutthroat castable on bob's turn"
          Spec.assertBool s (not (S.castable S.alice pikerId bobsTurn)) "and a creature without it is not"

-- The events a trigger condition GENUINELY fires on (Event.matchesTrigger's own
-- arms are the spec), so eventBindings is exercised through its matching arm
-- rather than through its `_ -> Map.empty` fallthrough. A pair that did not
-- match would pin nothing: both sides would read empty for every condition.
--
-- The INHERENT conditions are the exception, and it is the rules' rather than an
-- oversight: CR 725.2's and CR 702.179d's abilities hang on no card, so
-- Event.matchesTrigger answers False for them whatever the event and their real
-- matchers live in Pawl.Engine.Monarch and Pawl.Engine.Speed. Their arms below
-- name the event the RULE names anyway. What the pin says of those two is
-- therefore weaker but not vacuous -- that neither claims a slot the log could
-- never bind -- and it is what fails if either grows a binding arm here without
-- eventBindingSlots being told.
--
-- A NON-EMPTY LIST rather than one event, because Event.eventBindingSlots
-- answers the guaranteed FLOOR -- the slots bound for every event a condition
-- admits -- and a condition that binds a slot for some of its events and not
-- others cannot be pinned by any single one of them. Every condition but one is
-- represented by a one-element list, for which the floor is that event's exact
-- keyset. SelfLeavesTheBattlefield is the exception, and the two
-- destinations below are why: CR 400.7e binds `became` for the public one and
-- withholds it for the hidden one (CR 400.2).
--
-- Exhaustive with no wildcard, which is half of what keeps the pin honest -- a
-- new TriggerCondition fails to compile here. The other half, the list below, is
-- hand-kept and cannot be forced; add the new constructor there too.
representativeEvents :: TriggerCondition.TriggerCondition -> NonEmpty.NonEmpty GameEvent.GameEvent
representativeEvents cond =
  let departed = ObjectId.MkObjectId 1
      arrived = ObjectId.MkObjectId 2
      moved from to = GameEvent.Moved (ZoneChange.MkZoneChange departed arrived from to) S.emptyCharacteristics
      combatDamage =
        GameEvent.DamageDealt
          (DamageEvent.MkDamageEvent departed (Recipient.ToPlayer S.bob) 2 False False False 0 Nothing DamageKind.Combat)
      one e = e NonEmpty.:| []
   in case cond of
        TriggerCondition.SelfEnters -> one (moved Zone.Stack Zone.Battlefield)
        TriggerCondition.PermanentEnters _ -> one (moved Zone.Stack Zone.Battlefield)
        TriggerCondition.StepBegins phase _ -> one (GameEvent.StepBegan phase S.alice)
        -- CR 603.8: a state trigger matches a game STATE, so no log entry fires
        -- it at all (Event.matchesTrigger's StateIs arm answers False for every
        -- event). Any event is therefore as representative as any other.
        TriggerCondition.StateIs _ -> one (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
        TriggerCondition.SelfDealsCombatDamageToPlayer -> one combatDamage
        -- The same event read by a bystander, and the only one this condition
        -- admits.
        TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> one combatDamage
        TriggerCondition.CreatureDealtCombatDamageToMonarch -> one combatDamage
        -- CR 702.179d's own event. Like the monarch's condition above it, this
        -- one is matched by Pawl.Engine.Speed.inherentPending rather than by
        -- Event.matchesTrigger, which answers False for it whatever the event --
        -- so the pin here is that an inherent condition binds nothing from the
        -- log, which is what Event.eventBindingSlots claims for it.
        TriggerCondition.OpponentLostLifeDuringYourTurn -> one (GameEvent.LifeLost S.bob 2)
        TriggerCondition.SelfCycled -> one (GameEvent.Discarded S.alice departed DiscardCause.ToPayCyclingCost)
        TriggerCondition.PlayerDiscards _ -> one (GameEvent.Discarded S.alice departed DiscardCause.Ordinary)
        -- CR 508.5's defending player, and deliberately NOT the attacker's own
        -- controller: eventBindings binds this field under `thatPlayer`, so an
        -- arm that bound the attacking side instead would still agree with
        -- eventBindingSlots here if the two coincided.
        TriggerCondition.SelfAttacks _ -> one (GameEvent.AttackerDeclared departed S.carol 1)
        -- The same declaration event. This one binds NOTHING off it, which is
        -- what eventBindingSlots claims and what the pin here checks -- the
        -- defending player the event carries is not rule 702.149a's to read.
        TriggerCondition.SelfAttacksWithAnother _ -> one (GameEvent.AttackerDeclared departed S.carol 1)
        -- The same declaration event, with the count that makes it CR 506.5's
        -- alone -- and the ATTACKER is what this one binds, where SelfAttacks
        -- above binds the defending player off the very same event.
        TriggerCondition.CreatureAttacksAlone _ -> one (GameEvent.AttackerDeclared departed S.carol 1)
        -- The same declaration event once more. Rule 702.105a binds NOTHING off
        -- it, SelfAttacksWithAnother's case: the player it compares is read from
        -- Combat.attackers and then never named again.
        TriggerCondition.SelfAttacksPlayerWithMostLife -> one (GameEvent.AttackerDeclared departed S.carol 1)
        -- The GROUPED blocking event, which is CR 509.3a's arity: one per blocking
        -- creature, whatever it was declared against.
        TriggerCondition.SelfBlocks -> one (GameEvent.BlocksDeclared departed 1)
        -- The same event with the count read, which is all CR 509.3e adds.
        TriggerCondition.SelfBlocksAtLeast _ -> one (GameEvent.BlocksDeclared departed 2)
        -- The same grouped event once more, with the count IGNORED: CR 509.3e's
        -- filtered form reads the attackers off Combat.blockers instead, and
        -- binds nothing off the log.
        TriggerCondition.SelfBlocksOneOrMore _ -> one (GameEvent.BlocksDeclared departed 1)
        -- The PAIRWISE event instead: CR 509.3b's bearer is the BLOCKER too, and
        -- the attacker beside it is what this one binds.
        TriggerCondition.SelfBlocksCreature -> one (GameEvent.BlockerDeclared departed (ObjectId.MkObjectId 41))
        -- CR 508.5's defending player again, and carol for SelfAttacks' reason
        -- above: eventBindings binds this field under `thatPlayer`.
        TriggerCondition.SelfBecomesBlocked -> one (GameEvent.AttackerBlocked departed S.carol)
        -- The same declaration event SelfBlocks names, with the ids the other way
        -- round: this condition's bearer is the ATTACKER, and the blocker is what
        -- it binds.
        TriggerCondition.SelfBecomesBlockedBy _ -> one (GameEvent.BlockerDeclared (ObjectId.MkObjectId 41) departed)
        -- The GROUPED attacking-side event, which is what makes this one fire
        -- once where the arm above fires per blocker. carol on SelfBecomesBlocked's
        -- reasoning -- and this one binds that player nothing, which is the
        -- difference the pin catches.
        TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> one (GameEvent.AttackerBlocked departed S.carol)
        TriggerCondition.SelfPutIntoGraveyardFromLibrary -> one (moved Zone.Library Zone.Graveyard)
        -- Every origin zone is admitted, but the floor is the same for all of
        -- them: the destination is always a graveyard, which CR 400.2 makes
        -- public, so CR 400.7e never withholds anything and one event says as
        -- much as any list would.
        TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> one (moved Zone.Hand Zone.Graveyard)
        TriggerCondition.SelfDies -> one (moved Zone.Battlefield Zone.Graveyard)
        TriggerCondition.PermanentDies _ -> one (moved Zone.Battlefield Zone.Graveyard)
        -- CR 603.6c admits every destination, and CR 400.2 splits them into
        -- public and hidden, so both sides of CR 400.7e's proviso have to be
        -- here for the floor to be the honest answer.
        TriggerCondition.SelfLeavesTheBattlefield ->
          moved Zone.Battlefield Zone.Graveyard NonEmpty.:| [moved Zone.Battlefield Zone.Hand]
        TriggerCondition.SpellOrAbilityCounters _ ->
          one (GameEvent.SpellCountered (Countering.MkCountering departed arrived S.alice))
        -- CR 615.13: the recipient has to be a PLAYER, this condition being
        -- scoped to damage that would be dealt to one -- an event naming a
        -- creature matches nothing and would pin the floor at empty.
        TriggerCondition.DamageToPlayerPrevented _ -> one (GameEvent.DamagePrevented (Recipient.ToPlayer S.bob) 2)
        -- CR 119.9's own event, and the only one this condition admits: the
        -- payload is a player and an amount, and the amount is the floor.
        TriggerCondition.PlayerGainsLife _ -> one (GameEvent.LifeGained S.bob 2)
        -- The loss condition's own event, and the only one it admits, on the
        -- gain arm's reasoning: same payload shape, same amount floor.
        TriggerCondition.PlayerLosesLife _ -> one (GameEvent.LifeLost S.bob 2)
        -- CR 714.2b: a placement on the BEARER that crosses the chapter. The
        -- bearer here is `departed`, the id Event.matchesTrigger is asked about
        -- below, and the counts straddle N so the event really matches.
        TriggerCondition.SelfCountersReached kind n -> one (GameEvent.CountersPut departed kind 0 n)
        -- CR 310.11b: a removal on the BEARER that took the last counter, so the
        -- event really matches the condition Event.matchesTrigger is asked about.
        TriggerCondition.SelfLastCounterRemoved kind -> one (GameEvent.CountersRemoved departed kind 1 0)
        -- CR 601.2i's own event, and the only one this condition admits. Both
        -- halves are bound whichever ids the event names -- the spell under
        -- `thatSpell`, the caster under `thatPlayer` -- so the two sides agree
        -- on the pair.
        TriggerCondition.SpellCast _ _ -> one (GameEvent.SpellCast S.alice arrived S.emptyCharacteristics)
        -- The same event, and the only one this condition admits either. It binds
        -- nothing whichever ids the event names, since the spell IS the bearer.
        TriggerCondition.SelfCast -> one (GameEvent.SpellCast S.alice arrived S.emptyCharacteristics)
        -- CR 709.5h's own event, on the BEARER and naming the same door the
        -- condition does, so the pair really matches -- the door below is the one
        -- everyTriggerCondition names.
        TriggerCondition.SelfHalfUnlocked half -> one (GameEvent.HalfUnlocked departed half False)
        -- CR 709.5i's own event, with the flag SET -- an unset one matches
        -- nothing, and would pin the floor against an event this condition does
        -- not admit.
        TriggerCondition.RoomFullyUnlocked _ -> one (GameEvent.HalfUnlocked departed (CardName.MkCardName (Text.pack "Steaming Sauna")) True)
        -- EVERY event any branch admits, concatenated, which is what makes the
        -- intersection below the honest floor for an AnyOf: a slot one branch
        -- binds and another does not must not be claimed.
        TriggerCondition.AnyOf conditions -> case conditions of
          [] -> one (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)
          c : cs -> Foldable.foldr1 (<>) (fmap representativeEvents (c NonEmpty.:| cs))
        -- CR 708.7's own event, and the only one this condition admits, on the
        -- BEARER -- so the pair really matches.
        TriggerCondition.SelfTurnedFaceUp -> one (GameEvent.TurnedFaceUp departed)
        -- The same event for the watcher-scoped form, and the only one it admits.
        -- `departed` again, so the pair really matches: the Filter this condition
        -- is instantiated with below is the trivial one, which admits whatever the
        -- id resolves to.
        TriggerCondition.PermanentTurnedFaceUp _ -> one (GameEvent.TurnedFaceUp departed)
        -- CR 702.112b's own event, and the only one this condition admits, on
        -- `departed` for the arm above's reason.
        TriggerCondition.PermanentBecomesDesignated d _ -> one (GameEvent.BecameDesignated d departed)
        -- CR 702.100b's own event, and the only one this condition admits. The
        -- pair does NOT match -- the condition is self-scoped and `departed` is
        -- not the bearer -- which pins the floor for a matching pair too, since
        -- this arm binds nothing either way.
        TriggerCondition.SelfEvolves -> one (GameEvent.Evolved departed)
        -- CR 702.134c's own event, and the only one this condition admits. TWO
        -- distinct ids, which is what the pin needs here: eventBindings stamps the
        -- SECOND under `thatMentoredCreature`, so an arm that bound the mentor
        -- instead would still agree with eventBindingSlots if the two coincided.
        -- Whether the pair matches on the board below does not matter, eventBindings
        -- reading the event rather than the attachment.
        TriggerCondition.AttachedCreatureMentors -> one (GameEvent.Mentored departed arrived)
        -- CR 701.21a's own event, and the only one this condition admits. The
        -- payload is arbitrary: the condition compares nothing, so any sacrifice
        -- matches and the floor is the same for all of them.
        TriggerCondition.PermanentSacrificed -> one (GameEvent.PermanentSacrificed S.alice departed)
        -- CR 603.3b's own event, and the only one this condition admits. The
        -- pair does NOT actually match here -- `departed` projects as no Saga on
        -- the empty board Event.matchesTrigger is asked about -- which is fine
        -- for what this pins: eventBindings contributes nothing for this
        -- condition under any event, so the floor is empty either way.
        TriggerCondition.SagaFinalChapterTriggers _ ->
          one (GameEvent.AbilityTriggered departed S.alice (TriggerCondition.SelfCountersReached CounterKind.Lore 3))
        -- CR 725.1's own event, and the only one this condition admits. CR 725.3
        -- makes it name exactly one player, so there is no second shape of the
        -- event for the floor to differ on. bob rather than the perspective
        -- player, on the SelfAttacks arm's reasoning: an arm that stamped CR
        -- 109.5's "you" instead of the crowned player would still agree with
        -- eventBindingSlots here if the two coincided.
        TriggerCondition.PlayerBecomesMonarch _ -> one (GameEvent.BecameMonarch S.bob)
        -- CR 603.7's own event, and the only shape of it: Engine.sampleControl mints
        -- a ControlChanged only where the two players differ, so there is no
        -- same-player shape for the floor to come apart on. The ids and seats are
        -- arbitrary -- this condition binds nothing from the log, which is what
        -- Event.eventBindingSlots claims for it.
        TriggerCondition.LoseControlOfBound _ -> one (GameEvent.ControlChanged departed S.alice S.bob)
        -- CR 309.4c's own event. The dungeon id and the room are arbitrary: this
        -- condition binds nothing from the log, which is what
        -- Event.eventBindingSlots claims for it.
        TriggerCondition.RoomEntered _ -> one (GameEvent.VentureMarkerEntered S.alice departed RoomIndex.topmost)

-- Every TriggerCondition, one inhabitant each. The payloads are arbitrary:
-- eventBindings and eventBindingSlots both ignore them, which is itself part of
-- what the pin asserts.
everyTriggerCondition :: [TriggerCondition.TriggerCondition]
everyTriggerCondition =
  [ TriggerCondition.SelfEnters,
    TriggerCondition.PermanentEnters Filter.Type.IsSource,
    TriggerCondition.PermanentDies Filter.Type.IsSource,
    TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn,
    TriggerCondition.StateIs (Condition.Type.Compares (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 0)),
    TriggerCondition.SelfDealsCombatDamageToPlayer,
    TriggerCondition.PermanentDealsCombatDamageToPlayer (Filter.Type.And []),
    TriggerCondition.CreatureDealtCombatDamageToMonarch,
    TriggerCondition.OpponentLostLifeDuringYourTurn,
    TriggerCondition.SelfCycled,
    TriggerCondition.PlayerDiscards PlayerRelation.Opponent,
    TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
    TriggerCondition.SelfAttacksWithAnother (Filter.Type.And []),
    TriggerCondition.CreatureAttacksAlone (Filter.Type.And []),
    TriggerCondition.SelfAttacksPlayerWithMostLife,
    TriggerCondition.SelfBlocks,
    TriggerCondition.SelfBlocksAtLeast 2,
    TriggerCondition.SelfBlocksCreature,
    TriggerCondition.SelfBecomesBlocked,
    TriggerCondition.SelfBlocksOneOrMore (Filter.Type.And []),
    TriggerCondition.SelfBecomesBlockedBy (Filter.Type.And []),
    TriggerCondition.SelfBecomesBlockedByOneOrMore (Filter.Type.And []),
    TriggerCondition.SelfPutIntoGraveyardFromLibrary,
    TriggerCondition.SelfPutIntoGraveyardFromAnywhere,
    TriggerCondition.SelfDies,
    TriggerCondition.SelfLeavesTheBattlefield,
    TriggerCondition.SpellOrAbilityCounters PlayerRelation.You,
    TriggerCondition.DamageToPlayerPrevented PlayerRelation.You,
    TriggerCondition.PlayerGainsLife PlayerRelation.You,
    TriggerCondition.PlayerLosesLife PlayerRelation.Opponent,
    TriggerCondition.SelfCountersReached CounterKind.Lore 1,
    TriggerCondition.SelfLastCounterRemoved CounterKind.Defense,
    -- BOTH scopes, unlike StepBegins' one above: the TurnScope is new on this
    -- condition, and the pin below asserts eventBindingSlots against what
    -- eventBindings stamps for every event -- so an arm that had cased on the
    -- scope and stamped nothing under one of them would go unseen if only one
    -- were listed.
    TriggerCondition.SpellCast Filter.Type.IsSource TurnScope.EachTurn,
    TriggerCondition.SpellCast Filter.Type.IsSource TurnScope.OpponentsTurn,
    TriggerCondition.SelfCast,
    TriggerCondition.SelfHalfUnlocked (CardName.MkCardName (Text.pack "Steaming Sauna")),
    TriggerCondition.RoomFullyUnlocked PlayerRelation.You,
    -- Balemurk Leech's own pair, and not an arbitrary one: PermanentEnters binds
    -- `became` while RoomFullyUnlocked binds nothing, so the intersection is
    -- EMPTY -- which is the case the union-versus-intersection call in
    -- Event.eventBindingSlots turns on. A union would claim `became` here and the
    -- pin below would catch it.
    TriggerCondition.AnyOf [TriggerCondition.PermanentEnters Filter.Type.IsSource, TriggerCondition.RoomFullyUnlocked PlayerRelation.You],
    TriggerCondition.SelfTurnedFaceUp,
    TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And []),
    TriggerCondition.PermanentBecomesDesignated Designation.Renowned (Filter.Type.And []),
    TriggerCondition.SelfEvolves,
    TriggerCondition.AttachedCreatureMentors,
    TriggerCondition.PermanentSacrificed,
    TriggerCondition.SagaFinalChapterTriggers PlayerRelation.You,
    -- BOTH relations, on the SpellCast pair's reasoning above: an eventBindings
    -- arm that had cased on the relation and stamped nothing under one of them
    -- would go unseen if only one were listed. Custodi Lich prints the You form
    -- and Garland, Royal Kidnapper the Opponent one, and both stamp the crowned
    -- player -- which is the claim this list exists to keep honest.
    TriggerCondition.PlayerBecomesMonarch PlayerRelation.You,
    TriggerCondition.PlayerBecomesMonarch PlayerRelation.Opponent,
    TriggerCondition.LoseControlOfBound (SlotName.MkSlotName (Text.pack "target")),
    TriggerCondition.RoomEntered RoomIndex.topmost
  ]

-- CR 603.6c's penultimate sentence -- "An ability that attempts to do something
-- to the card that left the battlefield checks for it only in the first zone
-- that it went to" -- said positively by CR 400.7e: "Abilities that trigger when
-- an object moves from one zone to another ... can find the new object that it
-- became in the zone it moved to when the ability triggered, if that zone is a
-- public zone."
--
-- Endless Cockroaches, {1}{B}{B} Creature -- Insect 1/1, "When this creature
-- dies, return it to its owner's hand." Two different objects hide inside that
-- one printed word "it": the ability's SOURCE (CR 113.7a -- the permanent that
-- died, which CR 603.10a's look-back reads from CR 608.2h last known
-- information) and the CARD it became in the graveyard, which is what the
-- effect has to move. CR 400.7 minted a fresh id for the second, so the two are
-- not interchangeable and they are not one slot.
--
-- Also the home of the pin on Event.eventBindingSlots (the last case): that
-- classification restates in one dimension what eventBindings says in two, so
-- the two are compared here rather than trusted to agree.
--
-- The structural twin is Narcomoeba's `MoveToZone "self" Battlefield` in
-- graveyardTriggerSpec: same opcode, same slot shape. There "self" IS the
-- arriving card, because SelfPutIntoGraveyardFromLibrary matches on the
-- ARRIVING incarnation; here it is not, because CR 603.10a makes this condition
-- match on the DEPARTING one. That contrast is why there are two slots.
becameSlotSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becameSlotSpec s registry =
  let -- alice: one Mountain (Lightning Bolt's {R}), the Cockroaches in play, and
      -- the Bolt in hand. S.identityAnswer targets the least Recipient, and
      -- Recipient.ToCreature sorts before Recipient.ToPlayer, so the one
      -- creature on the board is the target without a bespoke interpreter.
      roachBoard = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        cockroaches <- S.printingOf s registry "Endless Cockroaches"
        let (roachId, withRoaches) = S.addCreature cockroaches S.alice (S.landsInPlay mountain 1)
        pure (roachId, S.handOne lightningBolt withRoaches)
      -- Cast the Bolt, resolve it (3 damage marked on a 1/1), settle -- CR
      -- 704.5g destroys it and the same CR 117.5 settle's trigger scan sees the
      -- death -- then resolve the trigger.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        Set.fromList (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers zone pid gs))
      roachName = CardName.MkCardName $ Text.pack "Endless Cockroaches"
   in Spec.describe s "CR 400.7e the card it became" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the HAND: an effect reading the trigger's source would
        -- name the dead battlefield id and move nothing, leaving the card in
        -- the graveyard where the state-based action put it.
        Spec.it s "CR 603.6c whole card: Lightning Bolt kills Endless Cockroaches and its dies trigger returns the card to hand" $ do
          (_, board) <- roachBoard
          let (settled, after) = boltIt board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Set.member roachName (namesIn Zone.Graveyard S.alice settled)) "the card is in the graveyard when the trigger is placed"
          Spec.assertBool s (Set.member roachName (namesIn Zone.Hand S.alice after)) "and in hand once it resolves"
          Spec.assertBool s (not (Set.member roachName (namesIn Zone.Graveyard S.alice after))) "no longer in the graveyard"
          -- The Bolt itself is the graveyard's only remaining tenant, so the
          -- assertion above cannot be passing because the graveyard is read
          -- from the wrong player's zone.
          Spec.assertEqWith s "only the Bolt is left there" (namesIn Zone.Graveyard S.alice after) (Set.singleton (CardName.MkCardName $ Text.pack "Lightning Bolt"))
        -- The two slots, side by side on the placed trigger. CR 113.7a's
        -- source is the id that DIED and no longer resolves; CR 400.7e's
        -- "became" is the graveyard card, which does.
        Spec.it s "CR 113.7a the self slot keeps the departed id while became names the graveyard card" $ do
          (roachId, board) <- roachBoard
          let (settled, _) = boltIt board
              bindingsOn oid = maybe Map.empty Object.bindings (Game.lookupObject oid settled)
              slots = concatMap (Map.toList . Map.mapMaybe Binding.onlyOne . Binding.targetsOf . bindingsOn) (GameState.stack settled)
              slotFor name = lookup name slots
          Spec.assertEqWith s "self is the permanent that died" (slotFor Binding.triggerSource) (Just (Recipient.ToObject roachId))
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject roachId settled)) "and that id is gone (CR 400.7)"
          case slotFor Binding.became of
            Just (Recipient.ToObject graveyardId) -> do
              Spec.assertBool s (graveyardId /= roachId) "became is a different id"
              Spec.assertEqWith s "and it is the graveyard card" (fmap Face.name (Game.faceOf graveyardId settled)) (Just roachName)
              -- The spent Bolt is in that graveyard too, so membership is the
              -- assertion rather than the whole zone.
              Spec.assertBool s (elem graveyardId (Game.zoneMembers Zone.Graveyard S.alice settled)) "in alice's graveyard"
            other -> Spec.assertFailure s ("expected became to name an object, got " <> show other)
        -- eventBindings in isolation, so the binding is pinned to the rule
        -- rather than to one card's payload. CR 400.7e's "the new object that
        -- it became in the zone it moved to" is ZoneChange.object, never
        -- ZoneChange.departed, which is what matchesTrigger matched on.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id, not the departed one" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics
          Spec.assertEqWith s "became names the graveyard incarnation" (Event.eventBindings TriggerCondition.SelfDies died) (Map.singleton Binding.became (Binding.toObject arrived))
        -- A condition that is not a look-back gets no such slot: Narcomoeba's
        -- bearer IS the arriving card, so binding it again would be a second
        -- name for the same object.
        Spec.it s "CR 113.6k a library-to-graveyard trigger binds nothing" $ do
          let oid = ObjectId.MkObjectId 1
              milled = GameEvent.Moved (ZoneChange.MkZoneChange oid oid Zone.Library Zone.Graveyard) S.emptyCharacteristics
          Spec.assertEqWith s "no became slot" (Event.eventBindings TriggerCondition.SelfPutIntoGraveyardFromLibrary milled) Map.empty
        -- The pin on Event.eventBindingSlots, the per-CONDITION slot set the
        -- card lint asks (CardSpec's "every slot a triggered ability reads is
        -- bound for its condition"). That function is a second statement of
        -- what eventBindings already says, and eventBindings cases on
        -- (condition, event) PAIRS, so nothing in the types keeps the two
        -- agreeing: a new binding arm added there and forgotten here would
        -- silently un-lint the new slot. Comparing the keys eventBindings
        -- actually produces against what the classification claims is what
        -- makes the drift a failing test.
        --
        -- The INTERSECTION over the events a condition admits, because the
        -- classification answers the guaranteed floor rather than the union: a
        -- slot the lint says is available must be bound for every event that
        -- could have placed the trigger. For every condition but
        -- SelfLeavesTheBattlefield that list has one element, so the
        -- intersection is exactly that event's keyset.
        Spec.it s "CR 603.2 eventBindingSlots names exactly the keys eventBindings stamps for EVERY event a condition admits" $ do
          mapM_
            ( \cond ->
                let stamped = fmap (Map.keysSet . Event.eventBindings cond) (representativeEvents cond)
                 in Spec.assertEqWith s ("the slots bound for " <> show cond) (Event.eventBindingSlots cond) (foldr Set.intersection (NonEmpty.head stamped) (NonEmpty.tail stamped))
            )
            everyTriggerCondition

-- CR 400.7e's slot under a BYSTANDER's dies trigger, the last of the four
-- conditions that bind it (SelfDies in becameSlotSpec above,
-- SelfLeavesTheBattlefield in leavesBattlefieldSpec, PermanentEnters in
-- aetherFlashSpec below, and this): "Abilities that trigger when an object moves
-- from one zone to another ... can find the new object that it became in the
-- zone it moved to when the ability triggered, if that zone is a public zone."
--
-- Promise of Tomorrow, {2}{W} Enchantment, "Whenever a creature you control
-- dies, exile it." The bearer is a THIRD object -- neither the creature that
-- died nor its graveyard incarnation -- which is what makes "it" unambiguous
-- here where becameSlotSpec's Endless Cockroaches had to keep two incarnations
-- of one card apart. Not transcribed: the second ability, "at the beginning of
-- each end step, if you control no creatures, sacrifice this enchantment and
-- return all cards exiled with it to the battlefield under your control"
-- (#968).
--
-- The discriminating assertion is WHICH id the payload moves. CR 603.10a makes
-- Event.matchesTrigger's PermanentDies arm match on ZoneChange.departed, so
-- "you control" is answerable from CR 608.2h last known information; but CR
-- 400.7 deleted that id when the creature died, so the effect has to be handed
-- ZoneChange.object -- the card now in the graveyard -- instead. Reading
-- `departed` here would leave the creature sitting in the graveyard, which is
-- exactly what assertions (a) and (b) below rule out.
--
-- CR 400.7e's public-zone proviso needs no guard: the PermanentDies arm has
-- already required battlefield-to-graveyard, and CR 400.2 lists the graveyard
-- among the public zones. SelfLeavesTheBattlefield is the condition where the
-- proviso does real work, and it is guarded there.
--
-- Goblin Piker is the victim rather than a token, deliberately: CR 111.7 makes
-- a token in a zone other than the battlefield cease to exist, so a token
-- exiled out of a graveyard would leave nothing to observe and (a) would pass
-- for the wrong reason.
--
-- Two seats, because "a creature YOU control" needs a creature somebody else
-- controls to be separated from. Bob's Ogre Sentry standing untouched is that
-- separation.
promiseOfTomorrowSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
promiseOfTomorrowSpec s registry =
  let promiseBoard = do
        promise <- S.printingOf s registry "Promise of Tomorrow"
        piker <- S.printingOf s registry "Goblin Piker"
        sentry <- S.printingOf s registry "Ogre Sentry"
        let empty = Setup.emptyGame S.bothPlayers
            (_, withPromise) = S.addCreature promise S.alice empty
            (pikerId, withPiker) = S.addCreature piker S.alice withPromise
            (sentryId, withSentry) = S.addCreature sentry S.bob withPiker
        pure (pikerId, sentryId, withSentry)
      -- Destroy alice's creature outright (CR 701.8a), settle so the CR 117.5
      -- boundary scans the death and places the trigger, then resolve it.
      killIt oid gs =
        let killed = S.runPure S.identityAnswer gs (Event.destroy Regenerability.Regenerable [oid])
            settled = S.runPure S.identityAnswer killed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
   in Spec.describe s "CR 400.7e the card a BYSTANDER's dies trigger names" $ do
        Spec.it s "CR 700.4 whole card: alice's Goblin Piker dies and Promise of Tomorrow exiles the graveyard card" $ do
          (pikerId, sentryId, board) <- promiseBoard
          let (settled, after) = killIt pikerId board
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId settled)) "the battlefield id is gone (CR 400.7)"
          Spec.assertEqWith s "and the card is in the graveyard when the trigger is placed" (namesIn Zone.Graveyard S.alice settled) [pikerName]
          -- (a), (b) and (c) as one tuple. (a) and (b) are the same fact from
          -- both sides: an effect handed ZoneChange.departed would move an id
          -- CR 400.7 deleted, so the exile would be empty and the graveyard
          -- would still hold the Piker. (c) is the "you control" separation.
          Spec.assertEqWith
            s
            "exiled, out of the graveyard, and bob's creature untouched"
            (namesIn Zone.Exile S.alice after, namesIn Zone.Graveyard S.alice after, Set.member sentryId (GameState.battlefield after))
            ([pikerName], [], True)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Promise of Tomorrow's payload. The contrast with the
        -- SelfDies arm above is only in which object the BEARER is; the slot
        -- names ZoneChange.object either way.
        Spec.it s "CR 400.7e eventBindings binds the ARRIVING id for PermanentDies too" $ do
          let departed = ObjectId.MkObjectId 1
              arrived = ObjectId.MkObjectId 2
              died = GameEvent.Moved (ZoneChange.MkZoneChange departed arrived Zone.Battlefield Zone.Graveyard) S.emptyCharacteristics
          Spec.assertEqWith s "became names the graveyard incarnation" (Event.eventBindings (TriggerCondition.PermanentDies (Filter.Type.HasCardType CardType.Creature)) died) (Map.singleton Binding.became (Binding.toObject arrived))

-- CR 603.4's intervening "if" on a LOOK-BACK trigger, which is the one shape
-- where the clause has to be read against an object that no longer exists:
-- "When the trigger event occurs, the ability checks whether the stated
-- condition is true. The ability triggers only if it is." CR 608.2a repeats the
-- check as the ability resolves.
--
-- Deathknell Berserker, {1}{B} Creature -- Elf Berserker 2/2: "When this
-- creature dies, if its power was 3 or greater, create a 2/2 black Zombie
-- Berserker creature token." Both readings of "its power" that are available
-- without CR 608.2h are wrong -- the id is gone, so a live projection describes
-- nothing at all, and the card sitting in the graveyard has its printed 2.
-- Only last known information answers 3.
--
-- Bad Moon supplies the third power, and it does so through the LAYERS
-- (CR 613.4c, layer 7c), which is what makes the graveyard card's printed value
-- visibly the wrong answer rather than merely a different route to the same one.
lookBackInterveningSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lookBackInterveningSpec s registry =
  let berserkerBoard withBadMoon = do
        mountain <- S.printingOf s registry "Mountain"
        lightningBolt <- S.printingOf s registry "Lightning Bolt"
        berserker <- S.printingOf s registry "Deathknell Berserker"
        badMoon <- S.printingOf s registry "Bad Moon"
        let lands = S.landsInPlay mountain 1
            moonAdded = if withBadMoon then snd (S.addCreature badMoon S.alice lands) else lands
            (berserkerId, withBerserker) = S.addCreature berserker S.alice moonAdded
        pure (berserkerId, S.handOne lightningBolt withBerserker)
      -- The Bolt targets the least Recipient, and S.addCreature hands out
      -- ascending ids, so Bad Moon (added first) would sort before the
      -- Berserker if it were a legal target -- it is an enchantment, and
      -- Lightning Bolt's pool is AnyTarget, so the Berserker is the only
      -- creature and the Bolt finds it.
      boltIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            damaged = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer damaged Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      tokensOf pid gs =
        filter
          -- CR 111.4: the name is BOTH subtypes plus "Token", which is exactly
          -- the rule's own Dwarven Reinforcements example.
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName $ Text.pack "Zombie Berserker Token"))
          (Game.zoneMembers Zone.Battlefield pid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known information" $ do
        Spec.it s "CR 603.4 with Bad Moon the Berserker died at power 3 and its trigger fires" $ do
          (berserkerId, board) <- berserkerBoard True
          let (settled, after) = boltIt board
          Spec.assertEqWith s "it was a 3/3 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 3)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          case tokensOf S.alice after of
            [token] -> do
              -- Printed 2/2, and 3/3 on this board: the token is black, so
              -- the same Bad Moon that made its maker a 3/3 pumps it in turn
              -- (CR 613.4c, layer 7c). Asserting the projection rather than
              -- the printed pair is what keeps the two facts from being
              -- confused for one another.
              Spec.assertEqWith s "printed 2/2" (maybe (Nothing, Nothing) (\f -> (Face.power f, Face.toughness f)) (Game.faceOf token after)) (Just (Power.MkPower (Quantity.Type.Literal 2)), Just (Toughness.MkToughness (Quantity.Type.Literal 2)))
              Spec.assertEqWith s "3/3 under Bad Moon" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 3, Just 3)
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Zombie Berserker" (Projection.subtypesOf token after) (Set.fromList [Subtype.Zombie, Subtype.Berserker])
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
        -- CR 603.4's "otherwise it does nothing" -- and "does nothing" means
        -- the ability never reaches the stack at all, not that it resolves to
        -- no effect.
        Spec.it s "CR 603.4 without Bad Moon it died at power 2 and does not trigger at all" $ do
          (berserkerId, board) <- berserkerBoard False
          let (settled, after) = boltIt board
          Spec.assertEqWith s "a 2/2 while it lived" (Projection.powerOf berserkerId (fst board)) (Just 2)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "and no token was made" (tokensOf S.alice after) []

-- The same rule read against what CR 613 does NOT leave behind. CR 122.1a folds
-- a +1/+1 counter into the object's power and toughness at layer 7c, so a
-- projection taken as the creature died records the RESULT of the counter and
-- not the counter -- and "if it had a +1/+1 counter on it" is a question about
-- the counter. LastKnown.counters is where the counter itself is filed, and
-- Quantity.ObjectCounters is what reads it back.
--
-- Promising Duskmage, {2}{B} Creature -- Human Warlock 2/3: "When this creature
-- dies, if it had a +1/+1 counter on it, draw a card."
--
-- Murder rather than a damage spell does the killing on purpose: the counter
-- moves the Duskmage from 2/3 to 3/4, so any lethal-damage removal would need a
-- different amount per leg and the two legs would stop being one fixture. CR
-- 701.8a's destroy does not care what the toughness is.
--
-- Both of CR 603.4's reads are covered, and separately:
--
--   * the GATHER read (Event.interveningHolds) by the two legs -- the trigger
--     reaches the stack with the counter and does not reach it without.
--   * the RESOLUTION read (CR 608.2a, Pawl.Engine.Stack's OfTrigger arm) by the
--     third case, which lets the trigger onto the stack and then empties the
--     record it reads, so a wired re-check removes the ability and an unwired
--     one draws anyway.
counterLookBackSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
counterLookBackSpec s registry =
  let duskmageBoard withCounter = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        duskmage <- S.printingOf s registry "Promising Duskmage"
        let lands = S.landsInPlay swamp 3
            (duskmageId, withDuskmage) = S.addCreature duskmage S.alice lands
            countered =
              if withCounter
                then S.addCounter CounterKind.PlusOnePlusOne 1 duskmageId withDuskmage
                else withDuskmage
            -- CR 104.3c: five cards is more library than any leg can draw
            -- through, so nothing here loses the game before the assertion runs.
            -- No leg advances a turn either, so no draw step spends one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) countered [1 .. 5 :: Int]
        pure (duskmageId, S.handOne murder stocked)
      -- Murder's pool is Pool.Creatures and the Duskmage is the only creature on
      -- the board, so identityAnswer's least Recipient is it.
      murderIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- The COUNTER, not the power it produces. CR 122.1a makes 3/4 the visible
      -- consequence of the +1/+1 counter and this trigger asks about neither the
      -- 3 nor the 4, so the two facts are asserted apart.
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
   in Spec.describe s "CR 603.4 an intervening if over last known COUNTERS" $ do
        Spec.it s "CR 122.1 with a +1/+1 counter the Duskmage's death trigger draws a card" $ do
          (duskmageId, board) <- duskmageBoard True
          let (settled, after) = murderIt board
          Spec.assertEqWith s "it had one +1/+1 counter while it lived" (Map.lookup CounterKind.PlusOnePlusOne (countersOn duskmageId (fst board))) (Just 1)
          Spec.assertEqWith s "and CR 122.1a made it a 3/4, which is a DIFFERENT fact" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 3, Just 4)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "the trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "the Duskmage is in the graveyard" (Set.member duskmageId (GameState.battlefield after)) False
          -- The DELTA: the Murder left the hand and one card arrived from the
          -- library. Zero in hand and five in library is the other leg's answer,
          -- so the two cannot be satisfied by one implementation.
          Spec.assertEqWith s "one card drawn" (S.handSize S.alice after, librarySize S.alice after) (1, 4)
        -- CR 603.4's "otherwise it does nothing": without the counter the
        -- ability never triggers at all.
        Spec.it s "CR 603.4 with no counter the same death draws nothing" $ do
          (duskmageId, board) <- duskmageBoard False
          let (settled, after) = murderIt board
          Spec.assertEqWith s "no counters on it at all" (countersOn duskmageId (fst board)) Map.empty
          Spec.assertEqWith s "a plain 2/3" (Projection.powerOf duskmageId (fst board), Projection.toughnessOf duskmageId (fst board)) (Just 2, Just 3)
          Spec.assertEqWith s "one card in hand and five in library to start" (S.handSize S.alice (fst board), librarySize S.alice (fst board)) (1, 5)
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack settled) []
          Spec.assertEqWith s "the Duskmage is in the graveyard just the same" (Set.member duskmageId (GameState.battlefield after)) False
          Spec.assertEqWith s "no card drawn" (S.handSize S.alice after, librarySize S.alice after) (0, 5)

        -- CR 608.2a on its own. The record the resolution re-check reads is
        -- emptied while the trigger sits on the stack -- something no rule can
        -- do to last known information, which is exactly why it isolates the
        -- second read: only a wired re-check can notice.
        Spec.it s "CR 608.2a the intervening if is checked AGAIN as the ability resolves" $ do
          (duskmageId, board) <- duskmageBoard True
          let cast = S.runPure S.identityAnswer (fst board) (S.cast S.alice (snd board))
              destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
              settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
              forgotten =
                settled
                  { GameState.lastKnown =
                      Map.adjust
                        (\lk -> lk {LastKnown.counters = Map.empty})
                        duskmageId
                        (GameState.lastKnown settled)
                  }
              after = S.runPure S.identityAnswer forgotten Stack.resolveTop
          Spec.assertEqWith s "the trigger reached the stack on the gather read" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and was removed on the resolution read, drawing nothing" (S.handSize S.alice after, librarySize S.alice after) (0, 5)
          Spec.assertEqWith s "the stack is empty either way" (GameState.stack after) []

-- CR 702.93a undying and CR 702.79a persist: one sentence in two counter kinds,
-- minted by Pawl.Engine.Keyword.returns.
--
-- Young Wolf, {G} Creature -- Wolf 1/1, and Putrid Goblin, {1}{B} Creature --
-- Zombie Goblin 2/2. Each keyword is the card's whole text box, so nothing else
-- printed there can be producing the return.
--
-- KILLED TWICE, rather than the counter being placed by hand as
-- counterLookBackSpec's Duskmage does: the second death is what proves the
-- counter actually arrived, since CR 603.4's "if it had no counters" reads it off
-- CR 608.2h last known information. Murder rather than damage, for that spec's
-- reason -- the two deaths are at different toughnesses.
--
-- The two keywords are one group because rules 702.79a and 702.93a are one
-- sentence: what separates the legs is the counter kind, and persist's is the one
-- that makes the returned permanent SMALLER (CR 122.1a).
undyingSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
undyingSpec s registry =
  let -- The named creature on alice's board with two Murders in hand and six
      -- Swamps, which is {1}{B}{B} twice with nothing left over. No untap step
      -- runs between the two casts.
      board name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 6)
            (gs1, firstMurder) = S.handOne murder withCreature
            (secondMurder, gs2) = S.addHandCard murder S.alice gs1
        pure (oid, gs2, firstMurder, secondMurder)
      -- Cast the Murder, resolve it, settle -- CR 704.5g buries the creature and
      -- the same CR 117.5 scan sees the death -- then resolve what it placed.
      murderWith spellId gs =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
         in (settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      inGraveyard name pid gs = elem (CardName.MkCardName (Text.pack name)) (Maybe.mapMaybe (\oid -> fmap Face.name (Game.faceOf oid gs)) (Game.zoneMembers Zone.Graveyard pid gs))
      countersOn oid gs = maybe Map.empty Object.counters (Game.lookupObject oid gs)
      -- One leg per keyword: die, come back with the counter at the stated size,
      -- die again, stay dead.
      twiceKilled name kind power toughness =
        Spec.it s ("CR 702 " <> name <> " returns once, with its counter, and not a second time") $ do
          (firstId, gs, m1, m2) <- board name
          let (settled, after) = murderWith m1 gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject firstId after)) "CR 400.7: the permanent that died is a spent id"
          case named name after of
            [backId] -> do
              Spec.assertEqWith s "it entered with exactly one counter, of that kind" (countersOn backId after) (Map.singleton kind 1)
              Spec.assertEqWith s "CR 122.1a resizes it" (Projection.powerOf backId after, Projection.toughnessOf backId after) (Just power, Just toughness)
              Spec.assertEqWith s "CR 110.2a: under its owner's control" (Projection.controllerOf backId after) (Just S.alice)
              Spec.assertBool s (not (inGraveyard name S.alice after)) "and no longer in the graveyard"
              let (settled2, after2) = murderWith m2 after
              Spec.assertEqWith s "CR 603.4: with the counter on it, the second death triggers nothing" (GameState.stack settled2) []
              Spec.assertEqWith s "so nothing comes back" (named name after2) []
              Spec.assertBool s (inGraveyard name S.alice after2) "and it stays in the graveyard"
            other -> Spec.assertFailure s ("expected exactly one " <> name <> " back on the battlefield, got " <> show other)
   in Spec.describe s "CR 702.93 undying and CR 702.79 persist" $ do
        twiceKilled "Young Wolf" CounterKind.PlusOnePlusOne 2 2
        twiceKilled "Putrid Goblin" CounterKind.MinusOneMinusOne 1 1
        -- CR 110.2a's "unless the effect states otherwise". Alice steals bob's
        -- Wolf and kills it, so CR 603.3a hands ALICE the dies trigger -- which
        -- is what makes this discriminating, since a return under the ability's
        -- controller and a return under the owner are the same board at one
        -- seat. Lightning Bolt does the killing so the whole fixture is red.
        Spec.it s "CR 110.2a a stolen Young Wolf comes back under its OWNER's control" $ do
          mountain <- S.printingOf s registry "Mountain"
          treason <- S.printingOf s registry "Act of Treason"
          bolt <- S.printingOf s registry "Lightning Bolt"
          wolf <- S.printingOf s registry "Young Wolf"
          let (wolfId, withWolf) = S.addCreature wolf S.bob (S.landsInPlay mountain 4)
              (gs1, treasonId) = S.handOne treason withWolf
              (boltId, gs2) = S.addHandCard bolt S.alice gs1
              stolen = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs2 (S.cast S.alice treasonId)) Stack.resolveTop
              burned = S.runPure S.identityAnswer (S.runPure S.identityAnswer stolen (S.cast S.alice boltId)) Stack.resolveTop
              settled = S.runPure S.identityAnswer burned Engine.settleForPriority
              after = S.runPure S.identityAnswer settled Stack.resolveTop
          Spec.assertEqWith s "alice controls the Wolf when it dies" (Projection.controllerOf wolfId stolen) (Just S.alice)
          Spec.assertEqWith s "CR 603.3a: so the dies trigger is alice's" (fmap (\oid -> Projection.controllerOf oid settled) (GameState.stack settled)) [Just S.alice]
          case named "Young Wolf" after of
            [backId] -> do
              Spec.assertEqWith s "and it returns under bob's" (Projection.controllerOf backId after) (Just S.bob)
              Spec.assertEqWith s "with its +1/+1 counter" (countersOn backId after) (Map.singleton CounterKind.PlusOnePlusOne 1)
            other -> Spec.assertFailure s ("expected the Wolf back on the battlefield, got " <> show other)

-- CR 702.135a afterlife N: "When this permanent is put into a graveyard from the
-- battlefield, create N 1/1 white and black Spirit creature tokens with flying."
-- Minted by Pawl.Engine.Keyword.afterlife on the same TriggerCondition.SelfDies
-- undying and persist take, and the FIRST minted keyword ability that creates a
-- token -- so what is under test is a whole card, minted in the engine rather
-- than read from card data.
--
-- Ministrant of Obligation, {2}{W} Creature -- Human Cleric 2/1, whose entire
-- text box is "Afterlife 2". Nothing else printed on it can be making tokens,
-- and N is 2 rather than 1 so a mint that ignored the keyword's payload and
-- created one token would fail.
--
-- Every characteristic rule 702.135a states is asserted, because the mint writes
-- each of them out by hand and a wrong one compiles: 1/1, both colours, the
-- Spirit creature type and flying. Both colours matter most -- the pool's other
-- Spirit token, Doomed Traveler's, is white alone, so a mint copied from it
-- would pass everything else.
afterlifeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
afterlifeSpec s registry =
  let spiritsAfterKilling name = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        creature <- S.printingOf s registry name
        let (oid, withCreature) = S.addCreature creature S.alice (S.landsInPlay swamp 3)
            (gs, spellId) = S.handOne murder withCreature
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
            destroyed = S.runPure S.identityAnswer cast Stack.resolveTop
            settled = S.runPure S.identityAnswer destroyed Engine.settleForPriority
        pure (oid, settled, S.runPure S.identityAnswer settled Stack.resolveTop)
      spirits gs =
        filter
          (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack "Spirit Token")))
          (Set.toList (GameState.battlefield gs))
   in Spec.describe s "CR 702.135 afterlife" $ do
        Spec.it s "CR 702.135a a dying Ministrant of Obligation leaves two 1/1 white and black flying Spirits" $ do
          (ministrantId, settled, after) <- spiritsAfterKilling "Ministrant of Obligation"
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject ministrantId after)) "CR 400.7: the permanent that died is a spent id"
          case spirits after of
            [first, second] -> do
              let describes oid =
                    ( Projection.powerOf oid after,
                      Projection.toughnessOf oid after,
                      Projection.colorsOf oid after,
                      Projection.subtypesOf oid after,
                      Projection.cardTypesOf oid after,
                      Projection.hasKeyword Keyword.Type.Flying oid after,
                      Projection.controllerOf oid after
                    )
                  expected =
                    ( Just (1 :: Integer),
                      Just (1 :: Integer),
                      Set.fromList [Color.White, Color.Black],
                      Set.singleton Subtype.Spirit,
                      Set.singleton CardType.Creature,
                      True,
                      Just S.alice
                    )
              Spec.assertEqWith s "the first is rule 702.135a's token exactly" (describes first) expected
              Spec.assertEqWith s "and so is the second" (describes second) expected
            other -> Spec.assertFailure s ("expected exactly two Spirit tokens, got " <> show other)
        -- The other half of the same board: a creature WITHOUT afterlife dying to
        -- the same Murder leaves nothing behind, so the tokens above are the
        -- keyword's doing and not the fixture's.
        Spec.it s "CR 702.135a a dying creature without afterlife leaves none" $ do
          (pikerId, settled, after) <- spiritsAfterKilling "Goblin Piker"
          Spec.assertBool s (Maybe.isNothing (Game.lookupObject pikerId after)) "the Piker died to the same Murder"
          Spec.assertEqWith s "but nothing triggered" (GameState.stack settled) []
          Spec.assertEqWith s "and no tokens were created" (spirits after) []
        -- CR 702.135b: "if a permanent has multiple instances of afterlife, each
        -- triggers separately". Asserted of the MINT, as renown's multiplicity
        -- is, no card in the pool printing afterlife twice.
        Spec.it s "CR 702.135b each instance of afterlife is its own ability" $ do
          Spec.assertEqWith s "afterlife 2 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 2) 2)) [Keyword.afterlife 2, Keyword.afterlife 2]
          Spec.assertEqWith s "and afterlife 3 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Afterlife 3) 1)) [Keyword.afterlife 3]
          Spec.assertBool s (Keyword.afterlife 2 /= Keyword.afterlife 3) "and the N reaches the minted ability"

-- CR 702.46 soulshift N, the first minted keyword ability that TARGETS A CARD IN
-- A GRAVEYARD -- CR 115.2's clause (a) pool, which until now only card data
-- (Raise Dead) reached. Its dies condition is afterlife's and its optional
-- targeted clause is provoke's; what is new is the pool and the filter.
--
-- Kami of Empty Graves, {3}{B} Creature -- Spirit 4/1, whose entire text box is
-- "Soulshift 3". Murder does the killing, modularSpec's reason.
--
-- Alice's graveyard is seeded so that every way the minted filter could be wrong
-- picks a DIFFERENT card, and each wrong card has a SMALLER ObjectId than the
-- right one -- S.identityAnswer takes the least legal recipient, so a widened
-- filter is not merely permitted to go wrong, it is made to:
--
--   * Goblin Piker -- in the graveyard, not a Spirit. Drops out on the subtype.
--   * Shimatsu the Bloodcloaked -- a Spirit, mana value 4. Drops out on rule
--     702.46a's "N or less", which here is 3.
--   * bob's own Disowned Ancestor -- a Spirit of mana value 1 in the WRONG
--     graveyard (CR 400.1), which is what makes PlayerScope.You load-bearing.
--
-- leaving alice's Disowned Ancestor ({B} Creature -- Spirit Warrior) as the only
-- legal target. The dead Kami's own graveyard incarnation is a Spirit too and is
-- excluded by its mana value of 4 rather than by an "another" the rule does not
-- print.
-- Pays wherever `who` is offered a resolution cost, and answers everything else
-- as S.identityAnswer does -- so a transcript with no ChooseToPay in it says the
-- prompt was never raised rather than that the offer was refused. The Decider is
-- checked alongside the player because CR 723.1 can part the two; nothing in
-- these fixtures controls anybody, so they must agree.
paysFor :: PlayerId.PlayerId -> Prompt.Prompt r -> r
paysFor who p = case p of
  Prompt.ChooseToPay (Decider.MkDecider d) player _ _ _ _
    | d == who && player == who ->
        PaymentDecision.Pays
  _ -> S.identityAnswer p

-- The pay-or-not answers in a transcript, in order.
payResponses :: [Response.Response] -> [Response.Response]
payResponses = filter isPayResponse

isPayResponse :: Response.Response -> Bool
isPayResponse response = case response of
  Response.ChoseToPay _ -> True
  _ -> False

-- CR 702.123 fabricate N: "When this permanent enters, you may put N +1/+1
-- counters on it. If you don't, create N 1/1 colorless Servo artifact creature
-- tokens." Rule 702.123a prints CR 118.12a's rewriting already done, so the
-- minted clause is one UnlessPaid over
-- CostComponent.PutPlusOneCountersOnThis and the tokens are its "if you don't"
-- branch -- afterlife's mint with a gate on it, and the first minted keyword
-- ability that offers a COST at resolution. (Soulshift's and provoke's clauses
-- ask a question there too, but a printed "may" rather than a cost.)
--
-- Glint-Sleeve Artisan, {2}{W} Creature -- Dwarf Artificer 2/2, whose entire
-- text box is "Fabricate 1". Every reading is a different board: 3/3 with the
-- counter, 2/2 plus a Servo without it, 4/4 under Hardened Scales.
--
-- The first two cases start from the SAME board and the SAME settled trigger and
-- differ in NOTHING but alice's answer.
fabricateSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fabricateSpec s registry =
  let named name gs = filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just (CardName.MkCardName (Text.pack name))) (Set.toList (GameState.battlefield gs))
      artisansOn = named "Glint-Sleeve Artisan"
      -- The bearer cast from alice's hand off three lands and resolved, with its
      -- CR 603.6a enters trigger settled onto the stack but NOT resolved.
      -- `others` go onto alice's battlefield first.
      entersOnStack land bearer others =
        let base = List.foldl' (\g p -> snd (S.addCreature p S.alice g)) (S.landsInPlay land 3) others
            (gs, spellId) = S.handOne bearer base
            cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast (Stack.resolveTop >> Engine.settleForPriority)
      boardOf landName bearerName others = do
        land <- S.printingOf s registry landName
        bearer <- S.printingOf s registry bearerName
        rest <- traverse (S.printingOf s registry) others
        pure (entersOnStack land bearer rest)
      board = boardOf "Plains" "Glint-Sleeve Artisan"
   in Spec.describe s "CR 702.123 fabricate" $ do
        Spec.it s "CR 702.123a paying the counter leaves the Artisan a 3/3 and makes no Servo" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              -- The controls: the Artisan really entered, and its trigger really
              -- reached the stack, before anything below is read.
              Spec.assertEqWith s "it entered as a 2/2 with no counters" (S.powerToughnessOf artisanId onStack, S.counterOf CounterKind.PlusOnePlusOne artisanId onStack) (Just (2, 2), 0)
              Spec.assertEqWith s "CR 603.6a: its enters trigger is on the stack" (length (GameState.stack onStack)) 1
              Spec.assertEqWith s "alice was asked exactly once, and paid" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Pays]
              Spec.assertEqWith s "CR 122.6: one +1/+1 counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 1
              Spec.assertEqWith s "CR 613.4c: so it reads 3/3" (S.powerToughnessOf artisanId after) (Just (3, 3))
              Spec.assertEqWith s "CR 118.12a: the paid branch made no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- Every characteristic rule 702.123a states is asserted, because the mint
        -- writes each of them out by hand and a wrong one compiles. Colorless
        -- matters most: the pool's other minted token, afterlife's Spirit, is
        -- white and black, so a mint copied from it would pass everything else.
        Spec.it s "CR 702.123a declining creates a 1/1 colorless Servo artifact creature" $ do
          onStack <- board []
          let ((_, after), transcript) = Replay.record S.identityAnswer onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "alice was asked exactly once, and declined" (payResponses transcript) [Response.ChoseToPay PaymentDecision.Declines]
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 0
              Spec.assertEqWith s "so it is still a 2/2" (S.powerToughnessOf artisanId after) (Just (2, 2))
              case S.tokensOf after of
                [servoId] ->
                  Spec.assertEqWith
                    s
                    "the token is rule 702.123a's exactly"
                    ( fmap Face.name (Game.faceOf servoId after),
                      S.powerToughnessOf servoId after,
                      Projection.colorsOf servoId after,
                      Projection.cardTypesOf servoId after,
                      Projection.subtypesOf servoId after,
                      Projection.controllerOf servoId after
                    )
                    ( -- CR 111.4 names it, CR 105.2 makes an object with no mana
                      -- cost and no colour indicator colorless, and CR 111.2 gives
                      -- it to the ability's controller.
                      Just (CardName.MkCardName (Text.pack "Servo Token")),
                      Just (1, 1),
                      Set.empty,
                      Set.fromList [CardType.Artifact, CardType.Creature],
                      Set.singleton Subtype.Servo,
                      Just S.alice
                    )
                other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- CR 614.16 over a cost paid DURING a resolution: the board differs from
        -- the first case in nothing but the Hardened Scales, and it applies,
        -- because CR 118.12 pays this cost as the ability resolves and CR 609.1
        -- makes what happens then an effect of that ability.
        Spec.it s "CR 614.16 Hardened Scales sees fabricate's counter, so the Artisan reads 4/4" $ do
          onStack <- board ["Hardened Scales"]
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case artisansOn onStack of
            [artisanId] -> do
              Spec.assertEqWith s "it still entered as a 2/2" (S.powerToughnessOf artisanId onStack) (Just (2, 2))
              Spec.assertEqWith s "one counter became two" (S.counterOf CounterKind.PlusOnePlusOne artisanId after) 2
              Spec.assertEqWith s "so it reads 4/4" (S.powerToughnessOf artisanId after) (Just (4, 4))
              Spec.assertEqWith s "and still no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Glint-Sleeve Artisan, got " <> show (length other))
        -- The keyword's N, at gameplay level and on BOTH halves. Weaponcraft
        -- Enthusiast, {2}{B} Creature -- Aetherborn Artificer 0/1, whose entire
        -- text box is "Fabricate 2": a mint that dropped the payload would put
        -- one counter on (1/2, not 2/3) and make one Servo, and 0/1 keeps every
        -- reading a different number from the Artisan's.
        Spec.it s "CR 702.123a fabricate 2 puts two counters on the Enthusiast" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure (paysFor S.alice) onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "it entered as a 0/1" (S.powerToughnessOf enthusiastId onStack) (Just (0, 1))
              Spec.assertEqWith s "two +1/+1 counters went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 2
              Spec.assertEqWith s "so it reads 2/3" (S.powerToughnessOf enthusiastId after) (Just (2, 3))
              Spec.assertEqWith s "and no Servo" (S.tokensOf after) []
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        Spec.it s "CR 702.123a declining fabricate 2 creates two Servos" $ do
          onStack <- boardOf "Swamp" "Weaponcraft Enthusiast" []
          let after = S.runPure S.identityAnswer onStack Stack.resolveTop
          case named "Weaponcraft Enthusiast" onStack of
            [enthusiastId] -> do
              Spec.assertEqWith s "no counter went on" (S.counterOf CounterKind.PlusOnePlusOne enthusiastId after) 0
              Spec.assertEqWith s "so it is still a 0/1" (S.powerToughnessOf enthusiastId after) (Just (0, 1))
              Spec.assertEqWith s "and there are two Servos" (length (S.tokensOf after)) 2
            other -> Spec.assertFailure s ("expected one Weaponcraft Enthusiast, got " <> show (length other))
        -- CR 702.123b: "if a permanent has multiple instances of fabricate, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is, no card in the pool printing fabricate twice.
        Spec.it s "CR 702.123b each instance of fabricate is its own ability" $ do
          Spec.assertEqWith s "fabricate 1 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 1) 2)) [Keyword.fabricate 1, Keyword.fabricate 1]
          Spec.assertEqWith s "and fabricate 2 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Fabricate 2) 1)) [Keyword.fabricate 2]
          Spec.assertBool s (Keyword.fabricate 1 /= Keyword.fabricate 2) "and the N reaches the minted ability"

soulshiftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
soulshiftSpec s registry =
  let ancestorName = CardName.MkCardName (Text.pack "Disowned Ancestor")
      -- Rule 702.46a's "you may", exercised. S.identityAnswer declines it, which
      -- is what the declining leg below rides.
      exercising :: Prompt.Prompt r -> r
      exercising p = case p of
        Prompt.ChooseOptional {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      board = do
        swamp <- S.printingOf s registry "Swamp"
        murder <- S.printingOf s registry "Murder"
        kami <- S.printingOf s registry "Kami of Empty Graves"
        piker <- S.printingOf s registry "Goblin Piker"
        shimatsu <- S.printingOf s registry "Shimatsu the Bloodcloaked"
        ancestor <- S.printingOf s registry "Disowned Ancestor"
        let (kamiId, g1) = S.addCreature kami S.alice (S.landsInPlay swamp 3)
            (pikerId, g2) = S.addGraveyardCard piker S.alice g1
            (shimatsuId, g3) = S.addGraveyardCard shimatsu S.alice g2
            (theirsId, g4) = S.addGraveyardCard ancestor S.bob g3
            (mineId, g5) = S.addGraveyardCard ancestor S.alice g4
            -- CR 104.3c: nothing here draws, but a stocked library keeps a leg
            -- from ending on an empty one.
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g5 [1 .. 5 :: Int]
        pure (kamiId, (pikerId, shimatsuId, theirsId, mineId), S.handOne murder stocked)
      -- Cast the Murder, resolve it (the Kami dies), settle so the death trigger
      -- is gathered and its target chosen (CR 603.3d), then resolve the trigger.
      murderIt :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState, ObjectId.ObjectId) -> (GameState.GameState, GameState.GameState)
      murderIt answer (gs, spellId) =
        let cast = S.runPure answer gs (S.cast S.alice spellId)
            destroyed = S.runPure answer cast Stack.resolveTop
            settled = S.runPure answer destroyed Engine.settleForPriority
         in (settled, S.runPure answer settled Stack.resolveTop)
      graveyardOf = Game.zoneMembers Zone.Graveyard
   in Spec.describe s "Soulshift" $ do
        -- The proving case.
        Spec.it s "CR 702.46a whole card: the dead Kami returns the one Spirit card its N reaches" $ do
          (kamiId, (pikerId, shimatsuId, theirsId, mineId), gs) <- board
          let (settled, after) = murderIt exercising gs
          Spec.assertEqWith s "the dies trigger reached the stack" (length (GameState.stack settled)) 1
          Spec.assertBool s (not (S.onBattlefield kamiId after)) "and the Kami is gone"
          Spec.assertEqWith s "alice's hand holds the Ancestor" (S.countByName ancestorName S.alice after) 1
          Spec.assertBool s (notElem mineId (graveyardOf S.alice after)) "the id it was targeted under has left her graveyard (CR 400.7)"
          Spec.assertBool s (elem pikerId (graveyardOf S.alice after)) "the Piker stayed: not a Spirit"
          Spec.assertBool s (elem shimatsuId (graveyardOf S.alice after)) "Shimatsu stayed: mana value 4 against soulshift 3"
          Spec.assertBool s (elem theirsId (graveyardOf S.bob after)) "bob's identical Ancestor stayed: the wrong graveyard (CR 400.1)"
          Spec.assertEqWith s "so does the dead Kami itself, a Spirit of mana value 4, beside the spent Murder" (length (graveyardOf S.alice after)) 4
        -- CR 603.5's "may" is a real fork, and the control for the case above --
        -- same board, same Murder, and the trigger still reaches the stack.
        Spec.it s "CR 603.5 declining the may returns nothing" $ do
          (_, (_, _, _, mineId), gs) <- board
          let (settled, after) = murderIt S.identityAnswer gs
          Spec.assertEqWith s "the trigger reached the stack all the same" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "but alice's hand is empty" (S.countByName ancestorName S.alice after) 0
          Spec.assertBool s (elem mineId (graveyardOf S.alice after)) "and the Ancestor is where it was"
        -- CR 702.46b: "if a permanent has multiple instances of soulshift, each
        -- triggers separately". Asserted of the MINT, as afterlife's multiplicity
        -- is: Forked-Branch Garami prints "soulshift 4, soulshift 4" and is not in
        -- the pool.
        Spec.it s "CR 702.46b each instance of soulshift is its own ability" $ do
          Spec.assertEqWith s "soulshift 3 held twice is two abilities" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 3) 2)) [Keyword.soulshift 3, Keyword.soulshift 3]
          Spec.assertEqWith s "and soulshift 4 once is one" (Keyword.triggeredAbilitiesOf (Map.singleton (Keyword.Type.Soulshift 4) 1)) [Keyword.soulshift 4]
          Spec.assertBool s (Keyword.soulshift 3 /= Keyword.soulshift 4) "and the N reaches the minted ability"

-- Radiant Fountain, a Land: "When this land enters, you gain 2 life. / {T}: Add
-- {C}." A nonbasic land whose whole text box is one triggered ability and one
-- activated one, which is what makes it the pool's witness for CR 305.7's
-- "It loses all abilities generated from its rules text" reaching a TRIGGER.
--
-- The entry is staged the way Pawl.TriggerSpec's other entry fixtures stage it:
-- the permanent is placed, its Moved event recorded, and the scan run at the next
-- settle. CR 603.6a checks every battlefield permanent against the event, and it
-- reads each one's PROJECTION -- so a Blood Moon that has already made the
-- Fountain a Mountain leaves nothing there to trigger.
strippedTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
strippedTriggerSpec s registry =
  let settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved moved (Projection.project oid gs)] gs))
   in Spec.describe s "CR 305.7 strips a triggered ability" $ do
        Spec.it s "CR 603.6a Radiant Fountain's entry trigger gains its controller 2 life" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          let (fountainId, gs) = S.addCreature radiantFountain S.alice (Setup.emptyGame S.bothPlayers)
              after = entering fountainId gs
          Spec.assertEqWith s "20 + 2" (S.lifeOf S.alice after) (Just 22)
          Spec.assertBool s (ManaType.Colorless `elem` Mana.manaTypesOf fountainId after) "and it taps for colorless"
        Spec.it s "CR 305.7 under Blood Moon the same entry triggers nothing" $ do
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          bloodMoon <- S.printingOf s registry "Blood Moon"
          let (_, withMoon) = S.addCreature bloodMoon S.alice (Setup.emptyGame S.bothPlayers)
              (fountainId, gs) = S.addCreature radiantFountain S.alice withMoon
              after = entering fountainId gs
          Spec.assertBool s (Set.member Subtype.Mountain (Projection.subtypesOf fountainId after)) "it entered as a Mountain"
          Spec.assertEqWith s "nothing reached the stack" (GameState.stack after) []
          Spec.assertEqWith s "and no life was gained" (S.lifeOf S.alice after) (Just 20)
          -- CR 305.7's last clause, on the same board: the printed mana ability
          -- goes and the new basic land type's replaces it.
          Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf fountainId after) "red instead"
          Spec.assertBool s (ManaType.Colorless `notElem` Mana.manaTypesOf fountainId after) "colorless gone"

-- CR 702.19b: assign each blocker exactly its lethal threshold and let the
-- excess trample through to the defending player. S.aggressiveAnswer cannot be
-- used here -- its AssignCombatDamage arm dumps the whole amount onto the first
-- CREATURE recipient it finds, so nothing would ever reach a player and the
-- trigger under test would never have an event to match.
tramplingAnswer :: Prompt.Prompt r -> r
tramplingAnswer p = case p of
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockerEntries = Map.toList (Map.filterWithKey (\r _ -> S.isCreatureRecipient r) thresholds)
        spent = sum (fmap snd blockerEntries)
        leftover = if n >= spent then n - spent else 0
        toBlockers = Map.fromList blockerEntries
     in case filter (not . S.isCreatureRecipient) (Map.keys thresholds) of
          d : _ -> Map.insert d leftover toBlockers
          [] -> toBlockers
  _ -> S.aggressiveAnswer p

-- CR 603.10's FIRST sentence for a BYSTANDER: "objects that exist immediately
-- after an event are checked to see if the event matched any trigger
-- conditions". The scan runs once, at the CR 117.5 boundary, after CR 704.3's
-- state-based actions -- so a permanent that was on the battlefield when some
-- OTHER event in the same batch happened, and is gone by the time the scan
-- looks, has to be recovered from CR 608.2h last known information exactly as
-- CR 603.10a's look-back already recovers a departure event's own permanent.
--
-- Lightning Skelemental is the card: {B}{R}{R} Creature -- Elemental Skeleton
-- 6/1, "Trample, haste / Whenever this creature deals combat damage to a
-- player, that player discards two cards. / At the beginning of the end step,
-- sacrifice this creature." Its 1 toughness and CR 702.19b's trample are what
-- put the trigger's event and the bearer's death in ONE batch: the excess
-- reaches bob while the blocker's damage kills the Skelemental at the very next
-- CR 704.5g check, before any player gets priority.
bystanderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderSpec s registry =
  Spec.describe s "Bystander" $ do
    -- Which side of CR 603.10 each condition falls on, asserted directly.
    -- Event.looksBack is a TOTAL case, so -Werror already forces a new condition
    -- to be classified; what it cannot force is the classification being the
    -- RIGHT one, and the four arms below are the ones a plausible wrong reading
    -- would flip. Each is the rule read against the constructor's own printed
    -- sentence:
    --
    --   * CR 603.10a names leaves-the-battlefield abilities, so "dies" is one
    --     (CR 700.4 narrows the destination, which does not leave the family);
    --   * and it names sacrifice triggers in as many words;
    --   * CR 603.6c's own last sentence puts "put into a graveyard from
    --     anywhere" OUTSIDE that family, which is the arm a wildcard would have
    --     gotten wrong in the expensive direction;
    --   * CR 708.8 leaves a permanent turned face up ON the battlefield, so
    --     there is no departure for a look-back to recover.
    Spec.it s "CR 603.10a the look-back families are the ones that rule lists" $ do
      Spec.assertBool s (Event.looksBack (TriggerCondition.PermanentDies (Filter.Type.And []))) "a dies trigger is a leaves-the-battlefield ability"
      Spec.assertBool s (Event.looksBack TriggerCondition.SelfLeavesTheBattlefield) "and so is the wider written form"
      Spec.assertBool s (Event.looksBack TriggerCondition.PermanentSacrificed) "a sacrifice trigger is named outright"
      Spec.assertBool s (not (Event.looksBack TriggerCondition.SelfPutIntoGraveyardFromAnywhere)) "CR 603.6c says put-into-a-graveyard-from-anywhere is not one"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.PermanentTurnedFaceUp (Filter.Type.And [])))) "and CR 708.8 leaves a turned-up permanent on the battlefield"
      -- CR 603.1b: one ability, several conditions -- it looks back if any of
      -- them does, and the pair below differ only in whether one does.
      Spec.assertBool s (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfDies])) "an AnyOf containing one looks back"
      Spec.assertBool s (not (Event.looksBack (TriggerCondition.AnyOf [TriggerCondition.SelfEnters, TriggerCondition.SelfTurnedFaceUp]))) "and one containing none does not"
    -- The proving test. bob holds THREE cards, so "discarded once" (one left)
    -- is distinguishable from "discarded twice" (none) and from "not at all"
    -- (three).
    Spec.it s "CR 603.10 whole cards: Lightning Skelemental dies to its blocker and STILL makes bob discard two" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [piker] of
        (base, [attacker], [blocker]) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertEqWith s "bob starts with three cards" (S.handSize S.bob gs) 3
          Spec.assertEqWith s "CR 702.19b: five trampled through to bob" (S.lifeOf S.bob after) (Just 15)
          Spec.assertBool s (not (S.onBattlefield attacker after)) "CR 704.5g: the Piker's two killed the 6/1"
          Spec.assertBool s (not (S.onBattlefield blocker after)) "and the Piker died to its one"
          Spec.assertEqWith s "the Skelemental is in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
          Spec.assertEqWith s "and bob discarded two, exactly once" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob one blocker"
    -- The control leg, which passes with or without the bystander recovery:
    -- unblocked, the Skelemental is still on the battlefield at the boundary,
    -- so `onBattlefield` carries it and the same trigger fires from the
    -- ordinary candidate source. It is what makes the card data and the
    -- reserved "that player" slot innocent when the leg above fails.
    Spec.it s "CR 510.1b control: an UNBLOCKED Skelemental survives and makes bob discard two the ordinary way" $ do
      skelemental <- S.printingOf s registry "Lightning Skelemental"
      piker <- S.printingOf s registry "Goblin Piker"
      case S.combatBoardOf [skelemental] [] of
        (base, [attacker], []) -> do
          let gs = List.foldl' (\g _ -> snd (S.addHandCard piker S.bob g)) base [(), (), ()]
              after = S.runCombat tramplingAnswer gs
          Spec.assertBool s (S.onBattlefield attacker after) "the Skelemental is still on the battlefield"
          Spec.assertEqWith s "bob took all six" (S.lifeOf S.bob after) (Just 14)
          Spec.assertEqWith s "and discarded two" (S.handSize S.bob after) 1
        _ -> Spec.assertFailure s "fixture should give alice one attacker and bob no blockers"
    -- The OTHER shape the rule reaches, at the gather rather than through a
    -- whole turn: a CR 603.2b step trigger whose bearer is gone by the
    -- boundary. Khabál Ghoul ("At the beginning of each end step, put a +1/+1
    -- counter on Khabál Ghoul for each creature that died this turn") is the
    -- bearer; the end step's beginning and the Ghoul's own death are two
    -- events in one unscanned batch, and the step event comes FIRST, so
    -- nothing about the Ghoul's own departure event can be what recovers it.
    Spec.it s "CR 603.10 a StepBegins bearer that dies later in the same batch still triggers" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          began = S.withEvents [GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice] gs0
          dead = S.runPure S.identityAnswer began (Event.destroy Regenerability.Regenerable [ghoul])
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped dead) dead)
      Spec.assertEqWith s "the Ghoul really did leave the battlefield" (Game.lookupObject ghoul dead) Nothing
      Spec.assertEqWith s "its step trigger still fired" (fmap PendingTrigger.source triggers) [TriggerSource.OfObject ghoul]
      Spec.assertEqWith s "under alice, who controlled it as it left (CR 603.3a)" (fmap PendingTrigger.controller triggers) [S.alice]
    -- The discriminating twin: a bearer that left the battlefield BEFORE the
    -- step began did not exist immediately after that event, and gets nothing.
    -- Same board, same two events, opposite order.
    Spec.it s "CR 603.10 a bearer that had already left before the event does NOT trigger" $ do
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (ghoul, gs0) = S.addCreature khabalGhoul S.alice (Setup.emptyGame S.bothPlayers)
          dead = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [ghoul])
          began = S.runPure S.identityAnswer dead (State.modify' (Event.recordEvent (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) S.alice)))
          triggers = fst (Event.gatherTriggers (Event.unscannedGrouped began) began)
      Spec.assertEqWith s "the Ghoul is gone" (Game.lookupObject ghoul began) Nothing
      Spec.assertEqWith s "and nothing triggered" (fmap PendingTrigger.source triggers) []

-- CR 113.6m read off a BYSTANDER: the half of CR 603.10's first sentence
-- `bystanderSpec` above recovers, asked of a permanent whose ability functions
-- only in a graveyard.
--
-- The rule the recovery is not allowed to lose: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". A bystander is recovered from CR 608.2h last known
-- information, but what it is recovered AS is a permanent that was ON THE
-- BATTLEFIELD when the event happened -- so one of its abilities that functions
-- only in a graveyard was no more watching then than it would be now.
--
-- CR 603.10a is deliberately NOT this case. There the rule's own "unless its
-- trigger condition ... specifies that the object is put into that zone" arm
-- decides, and it is unimplemented (#819); a bystander carries any condition at
-- all, so nothing about that arm reaches here.
--
-- The pair, chosen so that ONE derivation is the only difference between them:
--
--   * Squee, Goblin Nabob ({2}{R} Legendary Creature -- Goblin 1/1, "At the
--     beginning of your upkeep, you may return this card from your graveyard to
--     your hand"). CR 113.6k cannot reach it -- an upkeep condition triggers
--     perfectly well from the battlefield -- so only the effect's own words say
--     graveyard.
--   * Bitterblossom ({1}{B} Kindred Enchantment -- Faerie, "At the beginning of
--     your upkeep, create a 1/1 black Faerie Rogue creature token with flying and
--     you lose 1 life") as the control: the SAME trigger condition, on the same
--     battlefield, leaving in the same batch, with an effect that names no zone.
--     CR 113.6's default keeps it functioning on the battlefield.
--
-- (Both names, costs, type lines, P/T and oracle texts checked against Scryfall.)
--
-- Both leave the battlefield AFTER the upkeep begins and inside one unscanned
-- batch, which is `bystanderSpec`'s Khabál Ghoul shape: the step event comes
-- first, so nothing about either permanent's own departure event can be what
-- offers it.
bystanderZoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
bystanderZoneSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      -- alice's upkeep begins with Squee and Bitterblossom on her battlefield;
      -- `remove` then takes both off inside the same batch. Answers with the two
      -- battlefield ids and the sources the gather produced.
      board remove = do
        squee <- S.printingOf s registry "Squee, Goblin Nabob"
        bitterblossom <- S.printingOf s registry "Bitterblossom"
        let (squeeId, g1) = S.addCreature squee S.alice (Setup.emptyGame S.bothPlayers)
            (blossomId, g2) = S.addCreature bitterblossom S.alice g1
            began =
              S.withEvents
                [GameEvent.StepBegan upkeep S.alice]
                (g2 {GameState.phase = upkeep, GameState.activePlayer = S.alice})
            after = S.runPure S.identityAnswer began (remove [squeeId, blossomId])
        pure (squeeId, blossomId, after, fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedGrouped after) after)))
   in Spec.describe s "BystanderZone" $ do
        -- The proving leg. EXILE rather than a graveyard on purpose: it leaves
        -- the bystander reading as the only source that could offer Squee's
        -- ability at all, so the assertion cannot pass on the strength of the
        -- graveyard filtering that already landed with CR 113.6m's trigger half.
        Spec.it s "CR 113.6m a bystander's graveyard-functioning trigger does not fire from the battlefield it just left" $ do
          (squeeId, blossomId, after, sources) <- board (mapM_ (\oid -> Event.changeZone oid Zone.Exile))
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertEqWith s "and so did the Bitterblossom" (Game.lookupObject blossomId after) Nothing
          Spec.assertBool s (null (Game.zoneMembers Zone.Graveyard S.alice after)) "neither card is in a graveyard, so no graveyard reading can be doing this"
          Spec.assertEqWith
            s
            "only the Bitterblossom, whose effect names no zone, is recovered as a bystander"
            sources
            [TriggerSource.OfObject blossomId]
        -- The same board with the ordinary destination. The battlefield
        -- incarnation still gets nothing, which is what this change is; the
        -- graveyard incarnation CR 400.7 mints is a different object under a
        -- different id, and whether IT should be offered to an event that
        -- predates its arrival is a separate question this says nothing about
        -- (#824).
        Spec.it s "CR 113.6m the same holds when the bystander dies to a graveyard" $ do
          (squeeId, blossomId, after, sources) <- board (Event.destroy Regenerability.Regenerable)
          Spec.assertEqWith s "Squee really left the battlefield" (Game.lookupObject squeeId after) Nothing
          Spec.assertBool s (TriggerSource.OfObject squeeId `notElem` sources) "the battlefield incarnation triggered nothing"
          Spec.assertBool s (TriggerSource.OfObject blossomId `elem` sources) "and the control still did"

-- CR 400.7e's slot read from the OTHER direction of a zone change: an entry.
-- "Abilities that trigger when an object moves from one zone to another ... can
-- find the new object that it became in the zone it moved to when the ability
-- triggered, if that zone is a public zone" -- and CR 400.2 lists the
-- battlefield among the public zones, so an enters trigger's payload may name
-- the entrant with no proviso to check.
--
-- Aether Flash, {2}{R}{R} Enchantment, "Whenever a creature enters, this
-- enchantment deals 2 damage to it." Soul Warden proved the CONDITION
-- (permanentEntersSpec above); its "you gain 1 life" names nothing about the
-- creature that entered. This is the first card whose EFFECT refers back to the
-- entrant.
--
-- The contrast with becameSlotSpec is the point of reusing one slot name.
-- There the bearer and the entrant are two incarnations of ONE card, and
-- `became` is the second of them; here the bearer is the enchantment and the
-- entrant is a different card entirely. CR 400.7e distinguishes neither
-- situation: it names "the new object that IT became", where "it" is whatever
-- moved, and the moved object being the bearer is a fact about the condition
-- rather than about the slot.
--
-- Goblin Piker ({1}{R} Creature -- Goblin Warrior 2/1) and Ogre Sentry ({1}{R}
-- Creature -- Ogre Warrior 3/3, defender) are the pair: identical costs and
-- colors, so two Mountains cast either, and the ONLY difference the test can be
-- reading is the toughness the 2 damage is measured against.
aetherFlashSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherFlashSpec s registry =
  let -- alice: Aether Flash already on the battlefield and two Mountains, with
      -- one creature card in hand. Casting it is the only thing on offer, so
      -- S.identityAnswer needs no bespoke interpreter.
      flashBoard creature = do
        mountain <- S.printingOf s registry "Mountain"
        aetherFlash <- S.printingOf s registry "Aether Flash"
        entrant <- S.printingOf s registry creature
        let (flashId, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
        pure (flashId, S.handOne entrant withFlash)
      castIt (gs, spellId) =
        let cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      damageEventsIn gs = Maybe.mapMaybe Event.damageOf (S.eventsOf gs)
      -- CR 120.3e's marked damage on the one battlefield permanent with this
      -- name. Nothing if it is not there, or if there is more than one of it.
      markedOn name gs =
        case filter (\oid -> fmap Face.name (Game.faceOf oid gs) == Just name) (Set.toList (GameState.battlefield gs)) of
          [oid] -> fmap Object.damage (Game.lookupObject oid gs)
          _ -> Nothing
      pikerName = CardName.MkCardName $ Text.pack "Goblin Piker"
      sentryName = CardName.MkCardName $ Text.pack "Ogre Sentry"
   in Spec.describe s "CR 400.7e the entrant an enters trigger names" $ do
        -- The gameplay-level proof, cast to resolution. The discriminating
        -- assertion is the GRAVEYARD: an ability whose `became` slot went
        -- unbound would resolve, find nothing under it and silently deal no
        -- damage, leaving a live 2/1 on the battlefield.
        Spec.it s "CR 603.6a whole card: a Goblin Piker enters and Aether Flash's 2 damage kills it (CR 704.5g)" $ do
          (flashId, board) <- flashBoard "Goblin Piker"
          let after = castIt board
          Spec.assertEqWith s "the Piker is not on the battlefield" (S.countOnBattlefieldByName pikerName S.alice after) 0
          Spec.assertEqWith s "it is in the graveyard, once" (namesIn Zone.Graveyard S.alice after) [pikerName]
          -- Falsifiers. The damage went to the creature, not to a player
          -- (CR 120.1a admits only battles, creatures and planeswalkers), and
          -- Aether Flash did not damage itself into the graveyard either.
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "bob's too" (S.lifeOf S.bob after) (Just 20)
          Spec.assertBool s (Set.member flashId (GameState.battlefield after)) "and the enchantment is still on the battlefield"
          Spec.assertEqWith s "exactly one damage event, of 2" (fmap DamageEvent.amount (damageEventsIn after)) [2]
        -- The control, differing only in the entrant's toughness: 2 damage
        -- marked on a 3/3 is not lethal (CR 704.5g compares the total marked
        -- against toughness), so the creature stays and CARRIES the mark. The
        -- marked damage is what proves the effect landed at all -- without it
        -- "still on the battlefield" would also be what a no-op looks like.
        Spec.it s "CR 704.5g the control: an Ogre Sentry survives the same 2 damage, marked" $ do
          (_, board) <- flashBoard "Ogre Sentry"
          let after = castIt board
          Spec.assertEqWith s "the Sentry is on the battlefield" (S.countOnBattlefieldByName sentryName S.alice after) 1
          Spec.assertEqWith s "the graveyard is empty" (namesIn Zone.Graveyard S.alice after) []
          Spec.assertEqWith s "with 2 damage marked on it" (markedOn sentryName after) (Just 2)
        -- eventBindings in isolation, so the binding is pinned to CR 400.7e
        -- rather than to Aether Flash's payload. The entrant is
        -- ZoneChange.object -- for an ENTRY the arriving incarnation is what
        -- the event is about, so `departed` would be the pre-move id of a card
        -- that is not on the battlefield at all.
        Spec.it s "CR 400.7e eventBindings binds the ENTRANT under became" $ do
          let castCard = ObjectId.MkObjectId 1
              entered = ObjectId.MkObjectId 2
              entry = GameEvent.Moved (ZoneChange.MkZoneChange castCard entered Zone.Stack Zone.Battlefield) S.emptyCharacteristics
          Spec.assertEqWith s "became names the permanent that entered" (Event.eventBindings (TriggerCondition.PermanentEnters (Filter.Type.HasCardType CardType.Creature)) entry) (Map.singleton Binding.became (Binding.toObject entered))
        -- CR 603.6a's "EACH TIME an event puts one or more permanents onto
        -- the battlefield" met with a per-entrant payload: Dragon Fodder
        -- ({1}{R} Sorcery, "create two 1/1 red Goblin creature tokens") makes
        -- two entrants in one event, so one Aether Flash places two triggers
        -- and each has to name ITS OWN. Both tokens dying is what says so --
        -- two triggers sharing one binding would kill one token twice and
        -- leave the other standing.
        --
        -- A token is also the one entrant whose Moved event is
        -- battlefield-to-battlefield (Event.recordTokenEntry's pseudo-move,
        -- where `departed` and `object` are the same fresh id), so this is the
        -- shape where reading either field would look identical. It is here as
        -- the reminder that the fields agree for a token and only for a token.
        -- CR 704.5d then removes the dead tokens from the graveyard, so the
        -- damage's proof is the DamageDealt log, not a graveyard census.
        Spec.it s "CR 603.6a two tokens enter together and each trigger names its own" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          dragonFodder <- S.printingOf s registry "Dragon Fodder"
          let (_, withFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (gs, spellId) = S.handOne dragonFodder withFlash
              after = castIt (gs, spellId)
          Spec.assertEqWith s "no Goblin token survived" (S.countOnBattlefieldByName (CardName.MkCardName $ Text.pack "Goblin Token") S.alice after) 0
          Spec.assertEqWith s "two damage events of 2, one per token" (fmap DamageEvent.amount (damageEventsIn after)) [2, 2]
          Spec.assertEqWith s "and they were dealt to two different objects" (Set.size (Set.fromList (fmap DamageEvent.target (damageEventsIn after)))) 2
        -- CR 608.2h, the case Aether Flash makes reachable with no second
        -- card: "if the effect requires information from a specific object
        -- ... the effect uses the current information of that object if it's
        -- in the public zone it was expected to be in". Two Aether Flashes,
        -- one 2/1 entrant, two triggers -- and the first one's damage kills it
        -- at the next state-based-action check, so the second resolves with
        -- its entrant already gone from the battlefield it was expected to be
        -- on. CR 400.7 minted a fresh id for the graveyard card, so the effect
        -- does not follow it there.
        Spec.it s "CR 608.2h a second Aether Flash resolves with the entrant already dead, and deals nothing" $ do
          mountain <- S.printingOf s registry "Mountain"
          aetherFlash <- S.printingOf s registry "Aether Flash"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, oneFlash) = S.addCreature aetherFlash S.alice (S.landsInPlay mountain 2)
              (_, twoFlashes) = S.addCreature aetherFlash S.alice oneFlash
              (gs, spellId) = S.handOne piker twoFlashes
              cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
              -- The creature spell resolves and enters; the settle's CR 117.5
              -- scan places both triggers.
              entered = S.runPure S.identityAnswer cast Stack.resolveTop
              placed = S.runPure S.identityAnswer entered Engine.settleForPriority
              -- One trigger resolves for 2, and the settle after it is where
              -- CR 704.5g destroys the 2/1.
              hit = S.runPure S.identityAnswer placed Stack.resolveTop
              buried = S.runPure S.identityAnswer hit Engine.settleForPriority
              -- The second trigger, resolving against an id CR 400.7 deleted.
              after = S.runPure S.identityAnswer buried Stack.resolveTop
          Spec.assertEqWith s "both triggers were placed" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "the Piker is dead before the second resolves" (S.countOnBattlefieldByName pikerName S.alice buried) 0
          Spec.assertEqWith s "one damage event so far" (fmap DamageEvent.amount (damageEventsIn buried)) [2]
          Spec.assertEqWith s "the second trigger did resolve" (GameState.stack after) []
          Spec.assertEqWith s "and dealt nothing: still one damage event" (fmap DamageEvent.amount (damageEventsIn after)) [2]
          Spec.assertEqWith s "the card is in the graveyard once, not twice" (namesIn Zone.Graveyard S.alice after) [pikerName]

-- Bitterblossom {1}{B} Kindred Enchantment -- Faerie: "At the beginning of your
-- upkeep, you lose 1 life and create a 1/1 black Faerie Rogue creature token
-- with flying." The pool's first KINDRED card (CR 308), and so the first object
-- of any kind that carries a creature type without being a creature.
kindredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
kindredSpec s registry =
  let upkeep = Phase.Beginning BeginningStep.Upkeep
      beginUpkeep gs = Event.recordEvent (GameEvent.StepBegan upkeep S.alice) (gs {GameState.phase = upkeep, GameState.activePlayer = S.alice})
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Bitterblossom on alice's battlefield, alice's upkeep begun. The middle
      -- state is the one where the trigger is on the stack (CR 603.3b); the last
      -- is after it has resolved, so a Faerie Rogue token stands beside it.
      board bitterblossom =
        let (oid, gs) = S.addCreature bitterblossom S.alice (Setup.emptyGame S.bothPlayers)
            onStack = settle (beginUpkeep gs)
         in (oid, onStack, resolveAll onStack)
      slot = SlotName.MkSlotName (Text.pack "target")
      -- "Target Faerie ...", drawn from a pool: the same Pool + Filter machinery
      -- every printed target spec goes through (Pawl.ResolveSpec's "target
      -- Wall" is the same shape over a creature type that behaves ordinarily).
      faeriesIn pool gs =
        Map.findWithDefault
          Set.empty
          slot
          (Target.legalSets (Just S.alice) S.noSource (Map.singleton slot (TargetSpec.required pool (Just (Filter.Type.HasSubtype Subtype.Faerie)))) gs)
   in Spec.describe s "Kindred" $ do
        -- The proving test for CR 308. CR 308.2 makes the kindred subtypes
        -- "the same as the set of creature subtypes", so the ENCHANTMENT
        -- answers a creature-type filter -- and CR 110.4's six permanent types
        -- do not include kindred, so it still answers no to being a creature.
        -- The token it makes is the control: a real Faerie creature, in both
        -- pools, which is what keeps the enchantment's absence from the
        -- creature pool from being a fixture that simply produced nothing.
        Spec.it s "CR 308.2 a Kindred Enchantment is a legal \"target Faerie permanent\", and CR 110.4 keeps it out of \"target Faerie creature\"" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, _, after) = board bitterblossom
              permanents = faeriesIn Pool.Permanents after
              creatures = faeriesIn Pool.Creatures after
          Spec.assertBool s (Set.member Subtype.Faerie (Projection.subtypesOf blossomId after)) "the enchantment carries the creature type Faerie"
          Spec.assertBool s (not (Projection.isCreatureOf blossomId after)) "and is not a creature"
          Spec.assertBool s (Set.member (Recipient.ToObject blossomId) permanents) "so \"target Faerie permanent\" offers it"
          Spec.assertEqWith s "alongside the token it made, and nothing else" (Set.size permanents) 2
          Spec.assertBool s (not (Set.member (Recipient.ToCreature blossomId) creatures)) "\"target Faerie creature\" does not"
          Spec.assertEqWith s "the token is the only one of those" (Set.size creatures) 1
        -- CR 308.1: "casting and resolving a kindred card follows the rules for
        -- ... the other card type", and here the other type is Enchantment, so
        -- nothing about the trigger is kindred-specific. What this pins is that
        -- the whole printed ability runs -- CR 603.3a's "your upkeep" (the
        -- ability controller's, CR 109.5), the life payment, and CR 111.1's
        -- token -- through the ordinary priority loop.
        Spec.it s "CR 603.3a Bitterblossom's upkeep trigger costs its controller 1 life and mints a 1/1 flying black Faerie Rogue" $ do
          bitterblossom <- S.printingOf s registry "Bitterblossom"
          let (blossomId, onStack, after) = board bitterblossom
          Spec.assertBool s (not (null (GameState.stack onStack))) "the upkeep trigger really reached the stack"
          Spec.assertEqWith s "no life was lost before it resolved" (S.lifeOf S.alice onStack) (Just 20)
          Spec.assertEqWith s "alice paid the 1 life" (S.lifeOf S.alice after) (Just 19)
          case filter (/= blossomId) (Set.toList (GameState.battlefield after)) of
            [token] -> do
              Spec.assertEqWith s "1/1" (Projection.powerOf token after, Projection.toughnessOf token after) (Just 1, Just 1)
              -- CR 202.2b/202.2e: a token has no mana cost, so the colour
              -- indicator is the only thing making it black.
              Spec.assertEqWith s "black" (Projection.colorsOf token after) (Set.singleton Color.Black)
              Spec.assertEqWith s "Faerie Rogue" (Projection.subtypesOf token after) (Set.fromList [Subtype.Faerie, Subtype.Rogue])
              Spec.assertBool s (Map.member Keyword.Type.Flying (Projection.keywordsOf token after)) "with flying"
              Spec.assertBool s (Projection.isCreatureOf token after) "and it, unlike its maker, IS a creature"
            other -> Spec.assertFailure s ("expected exactly one token beside Bitterblossom, got " <> show (length other) <> " other permanents")

-- CR 603.7's arming gate, through Meandering Towershell -- the pool's one card
-- whose delayed ability is printed "on your NEXT turn" (Pawl.Types.Onset).
--
-- The gate is NOT vacuous, and this group's whole point is to prove it. The
-- ability's own condition is StepBegins (Combat DeclareAttackers)
-- ControllersTurn, and a TurnScope cannot tell one of the controller's turns
-- from another -- so an extra combat phase in the SAME turn (Relentless Assault,
-- and its siblings Aggravated Assault, Full Throttle and Aurelia) has a declare
-- attackers step that begins on alice's turn and would fire the return early.
towershellOnsetSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellOnsetSpec s registry = Spec.describe s "DelayedOnset" $ do
  let boardOf = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        mountain <- S.printingOf s registry "Mountain"
        island <- S.printingOf s registry "Island"
        assault <- S.printingOf s registry "Relentless Assault"
        pure (towershellAssaultBoard towershell mountain island assault)
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
  Spec.it s "CR 603.7 a second declare attackers step THIS turn does not fire it" $ do
    (gs, spell) <- boardOf
    let -- alice attacks with the Towershell; its trigger exiles it and arms the
        -- return, gated to a later turn.
        atMain = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
        armed = GameState.delayedTriggers atMain
        -- "After this main phase, there is an additional combat phase followed
        -- by an additional main phase" (CR 500.8).
        cast = snd (Engine.runGamePure S.identityAnswer atMain (S.cast S.alice spell))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        atExtra = runToTurnStep 1 (Phase.Combat CombatStep.DeclareAttackers) S.aggressiveAnswer resolved
        afterExtra = snd (Engine.runGamePure S.aggressiveAnswer atExtra Engine.runStep)
    Spec.assertEqWith s "the return is armed" (length armed) 1
    -- Waiting for a BOUNDARY, not for a turn number: which turn alice's next one
    -- is cannot be known here (Pawl.Types.TurnWindow), and this same turn is not
    -- it whatever number it carries.
    Spec.assertEqWith
      s
      "and gated to alice's next turn, which has not begun"
      (fmap DelayedTrigger.window (Foldable.toList armed))
      [TurnWindow.ControllersNextTurn]
    Spec.assertEqWith s "the extra combat's declare attackers step really happened" (GameState.phase atExtra) (Phase.Combat CombatStep.DeclareAttackers)
    Spec.assertEqWith s "it is still in exile" (S.countOnBattlefieldByName towershellName S.alice afterExtra) 0
    Spec.assertEqWith s "and still armed, unspent" (length (GameState.delayedTriggers afterExtra)) 1
  -- The control that stops the case above from passing for the wrong reason: a
  -- gate that never opened would satisfy every assertion in it. The SAME line of
  -- play -- extra combat phase included -- returns the Towershell on alice's
  -- next turn.
  Spec.it s "CR 603.7b and the same line of play returns it on alice's next turn" $ do
    (gs, spell) <- boardOf
    let atMain = runToTurnStep 1 Phase.PostcombatMain S.aggressiveAnswer gs
        cast = snd (Engine.runGamePure S.identityAnswer atMain (S.cast S.alice spell))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        atNextTurn = runToTurnStep 3 (Phase.Combat CombatStep.DeclareBlockers) S.aggressiveAnswer resolved
    Spec.assertEqWith s "alice's next turn is turn 3" (GameState.turnNumber atNextTurn) 3
    Spec.assertEqWith s "and there it does return" (S.countOnBattlefieldByName towershellName S.alice atNextTurn) 1
    Spec.assertEqWith s "spending the one shot (CR 603.7b)" (length (GameState.delayedTriggers atNextTurn)) 0

-- CR 603.7a's last sentence -- "Other events that happen earlier may make the
-- trigger event impossible" -- with both halves already in the pool: Meandering
-- Towershell's return, armed for alice's NEXT turn, and Stonehorn Dignitary
-- taking that very turn's combat phase away before it arrives.
--
-- The printed phrase names ONE turn. A gate that only said "not before turn n"
-- would leave the entry armed past it and fire the return on the FOLLOWING turn
-- of alice's -- a turn the card does not name -- which is what the second case
-- here discriminates.
towershellSkipSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
towershellSkipSpec s registry = Spec.describe s "DelayedOnsetSkipped" $ do
  let boardOf = do
        towershell <- S.printingOf s registry "Meandering Towershell"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        stonehorn <- S.printingOf s registry "Stonehorn Dignitary"
        pure (towershellStonehornBoard towershell island plains stonehorn)
      towershellName = CardName.MkCardName $ Text.pack "Meandering Towershell"
      -- CR 603.2b: the combat steps of pid's turn that actually BEGAN. A skipped
      -- phase leaves none, which is CR 614.6's "if an event is replaced, it never
      -- happens" -- and is why this is read before the turn ends, since
      -- Engine.handoffTurn clears the log.
      combatStepsOf pid gs =
        [ph | GameEvent.StepBegan ph@(Phase.Combat _) who <- S.eventsOf gs, who == pid]
  -- The control that makes the case below discriminating: the same board and the
  -- same line of play with the Dignitary never cast. S.aggressiveAnswer takes no
  -- action at all, and S.fightAnswer is exactly it plus casting what it can
  -- afford -- so the ONE difference between the two runs is that combat phase.
  Spec.it s "CR 603.7 with alice's next turn intact the Towershell returns on it" $ do
    gs <- boardOf
    let turn3 = runToTurnStep 3 Phase.PostcombatMain S.aggressiveAnswer gs
    -- All five of CR 506.1's steps, because the returning Towershell is "put
    -- onto the battlefield attacking" -- CR 508.8's other half, which is what
    -- keeps the declare blockers and combat damage steps from being skipped.
    Spec.assertEqWith s "alice's turn 3 ran all five combat steps (CR 506.1)" (length (combatStepsOf S.alice turn3)) 5
    Spec.assertEqWith s "and the Towershell returned there" (S.countOnBattlefieldByName towershellName S.alice turn3) 1
    Spec.assertEqWith s "spending the one shot (CR 603.7b)" (length (GameState.delayedTriggers turn3)) 0
  -- THE PROVING CASE. bob's Dignitary, cast on turn 2, takes alice's turn-3
  -- combat phase away -- so the declare attackers step the return watches for
  -- never happens on the one turn "your next turn" named, and CR 603.7a's "other
  -- events that happen earlier may make the trigger event impossible" is the
  -- whole of what follows: the ability never triggers.
  Spec.it s "CR 603.7a a skipped combat phase on the named turn makes the return impossible, not late" $ do
    gs <- boardOf
    let turn3 = runToTurnStep 3 Phase.PostcombatMain S.fightAnswer gs
        turn5 = runToTurnStep 5 Phase.PostcombatMain S.fightAnswer turn3
    Spec.assertEqWith s "no combat step of alice's turn 3 began (CR 500.11)" (combatStepsOf S.alice turn3) []
    Spec.assertEqWith s "so the Towershell did not return on the turn its ability named" (S.countOnBattlefieldByName towershellName S.alice turn3) 0
    -- CR 614.10a spends the skip on ONE occurrence, so alice's turn 5 has a
    -- combat phase of its own -- and with it the very step the return watches
    -- for. That is what makes the assertions below about the ABILITY rather than
    -- about the board: the trigger event does occur here, and an entry left armed
    -- would fire on it and return the Towershell on a turn the card never named.
    -- Not all five steps: alice declares no attackers, so CR 508.8 skips the
    -- declare blockers and combat damage steps.
    Spec.assertBool
      s
      (List.elem (Phase.Combat CombatStep.DeclareAttackers) (combatStepsOf S.alice turn5))
      "alice's turn 5 declare attackers step happens (CR 614.10a)"
    Spec.assertEqWith s "and the return never fires at all" (S.countOnBattlefieldByName towershellName S.alice turn5) 0
    Spec.assertEqWith s "nor is it left in the store forever" (length (GameState.delayedTriggers turn5)) 0

-- alice at her declare attackers step on turn 1 with one Meandering Towershell;
-- bob has four untapped Plains (exactly Stonehorn Dignitary's {3}{W}) and the
-- Dignitary in hand, and nothing else. Both libraries hold Islands so the draw
-- steps of the five turns these cases run through cannot empty one (CR 104.3c).
towershellStonehornBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  GameState.GameState
towershellStonehornBoard towershell island plains stonehorn =
  let (base, _, _) = S.combatBoardOf [towershell] []
      withLands = List.foldl' (\g _ -> snd (S.addCreature plains S.bob g)) base [1 :: Int .. 4]
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 8]
   in snd (S.addHandCard stonehorn S.bob (stock S.bob (stock S.alice withLands)))

-- alice at her declare attackers step with one Meandering Towershell, four
-- untapped Mountains (exactly Relentless Assault's {2}{R}{R}) and the Assault in
-- hand; bob defends. Both libraries hold Islands so the draw steps of the turns
-- these cases run through cannot empty one (CR 104.3c).
--
-- The Islands are in LIBRARIES and never on the battlefield, which matters for
-- this card: CR 702.14c's islandwalk reads the lands the DEFENDING PLAYER
-- CONTROLS, and a library is not the battlefield -- so nothing here turns on
-- evasion.
towershellAssaultBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (GameState.GameState, ObjectId.ObjectId)
towershellAssaultBoard towershell mountain island assault =
  let (base, _, _) = S.combatBoardOf [towershell] []
      withLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) base [1 :: Int .. 4]
      stock pid g = List.foldl' (\h _ -> snd (S.addLibraryCard island pid h)) g [1 :: Int .. 8]
      (withCard, spell) = S.handOne assault (stock S.bob (stock S.alice withLands))
   in ( withCard
          { GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.phase = GameState.phase base,
            GameState.combat = GameState.combat base,
            GameState.remaining = GameState.remaining base
          },
        spell
      )

-- Run whole steps until the board is at `phase` on turn `turn`, WITHOUT running
-- that step. Bounded so a bug cannot loop forever; stops on a finished game.
-- Pawl.CombatSpec has the same helper for the same card, kept local to each
-- group per the suite's convention.
runToTurnStep :: Natural -> Phase.Phase -> (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
runToTurnStep turn phase answer gs0 =
  let go n g =
        if n <= (0 :: Int)
          || Maybe.isJust (GameState.result g)
          || (GameState.turnNumber g == turn && GameState.phase g == phase)
          then g
          else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
   in go 64 gs0

-- CR 119.9: "Some triggered abilities are written, 'Whenever [a player] gains
-- life, . . . .' Such abilities are treated as though they are written, 'Whenever
-- a source causes [a player] to gain life, . . . .' If a player gains 0 life, no
-- life gain event has occurred, and these abilities won't trigger."
--
-- Ajani's Pridemate, {1}{W} Creature -- Cat Soldier 2/2, "Whenever you gain life,
-- put a +1/+1 counter on this creature", the card that proves it. Its payload
-- names only its own source, so every case here isolates the CONDITION.
--
-- What makes the group a proof rather than a demonstration is that each positive
-- has a control differing in ONE thing:
--
--   * the same Soul Warden, the same entering creature, the same 1 life gained --
--     and the Warden under the OTHER player. Only the GAINER differs, and the
--     Pridemate is silent (CR 109.5 / 603.3a's "you").
--   * one combat damage event, two life totals moving in opposite directions, and
--     a Pridemate on each side. Only the DIRECTION differs, and only the gainer's
--     fires: CR 120.3f's lifelink gain is a life gain event and CR 119.2's damage
--     loss is not (GameEvent.LifeLost is a different constructor entirely).
--
-- The zero case is CR 119.9's own last sentence, asserted on the CR 608.2i record
-- rather than through a counter: a 0-damage lifelink event is a real damage event
-- that gains 0 life, so the log must hold no life gain for it to match.
lifeGainTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeGainTriggerSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- A Maybe rather than a defaulted 0, so a Pridemate that is no longer
      -- there reads as Nothing and cannot be mistaken for one that took no
      -- counter.
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- alice always holds the Pridemate and casts the creature; `wardenOwner`
      -- decides who gains the life the entering creature causes. That is the only
      -- difference between the two cases below.
      wardenBoard plains pridemate soulWarden wardenOwner =
        let (_, b0) = S.addCreature soulWarden wardenOwner (S.landsInPlay plains 1)
            (mateId, b1) = S.addCreature pridemate S.alice b0
            (gs, spellId) = S.handOne soulWarden b1
            cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
         in (mateId, resolveAll cast)
      -- Only `attacker` attacks, and nobody blocks, so the life totals move by
      -- exactly the one damage event under test. Declining the block is what puts
      -- the damage on the PLAYER: bob's own Pridemate would otherwise block, and
      -- CR 120.3e's marked damage would leave his life total alone -- costing the
      -- lifelink case its "and bob lost two" control.
      attacksWith attacker p = case p of
        Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
        Prompt.DeclareBlockers {} -> Map.empty
        _ -> S.aggressiveAnswer p
   in Spec.describe s "PlayerGainsLife" $ do
        -- The gameplay-level proof, cast to resolution. alice's Soul Warden sees
        -- the second Warden enter (CR 603.6a), gains her 1 life on resolution (CR
        -- 119.3), and THAT is the event the Pridemate matches -- a second CR 117.5
        -- boundary later, off GameEvent.LifeGained.
        --
        -- Exactly one counter, not two: the newcomer's own "another" declines its
        -- own entry, so exactly one life gain event happened.
        Spec.it s "CR 119.9 whole cards: alice gains 1 life from Soul Warden and her Pridemate grows" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (mateId, settled) = wardenBoard plains pridemate soulWarden S.alice
          Spec.assertEqWith s "alice gained exactly 1" (S.lifeOf S.alice settled) (Just 21)
          Spec.assertEqWith s "the Pridemate took exactly one +1/+1 counter" (countersOn mateId settled) (Just 1)
        -- The control twin, differing in ONE thing: bob controls the Soul Warden,
        -- so bob is the one who gains. The same creature enters, the same 1 life
        -- is gained, the same log entry is written -- and CR 109.5's "you" is
        -- alice, so her Pridemate stays silent.
        --
        -- bob's gain is asserted too, or the case would pass for the wrong reason:
        -- an engine that recorded no event at all would also show no counter.
        Spec.it s "CR 109.5/603.3a the control: BOB gains the life, and alice's Pridemate stays silent" $ do
          plains <- S.printingOf s registry "Plains"
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          soulWarden <- S.printingOf s registry "Soul Warden"
          let (mateId, settled) = wardenBoard plains pridemate soulWarden S.bob
          Spec.assertEqWith s "bob really gained the life" (S.lifeOf S.bob settled) (Just 21)
          Spec.assertEqWith s "alice gained nothing" (S.lifeOf S.alice settled) (Just 20)
          Spec.assertEqWith s "so the Pridemate took no counter" (countersOn mateId settled) (Just 0)
        -- CR 120.3f: "damage dealt by a source with lifelink causes that source's
        -- controller to gain that much life, in addition to the damage's other
        -- results". The second producer, and the one CR 119.9's rewriting is aimed
        -- at -- no effect said "gain life"; a keyword did.
        --
        -- ONE board carries the control. bob has a Pridemate too, and the single
        -- combat damage event moves both life totals: alice's UP by 2 (CR 120.3f)
        -- and bob's DOWN by 2 (CR 119.2 / 120.3a). Only alice's fires, so the
        -- trigger is keyed on gaining life rather than on a life total moving.
        Spec.it s "CR 120.3f lifelink gains life, so the attacker's Pridemate grows and the defender's does not" $ do
          pridemate <- S.printingOf s registry "Ajani's Pridemate"
          childOfNight <- S.printingOf s registry "Child of Night"
          let (gs0, mine, _) = S.combatBoardOf [childOfNight] []
              (aliceMate, gs1) = S.addCreature pridemate S.alice gs0
              (bobMate, gs2) = S.addCreature pridemate S.bob gs1
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Child of Night"
            vampire : _ -> do
              let settled = resolveAll (S.fightWith (attacksWith vampire) gs2)
              Spec.assertEqWith s "alice gained two" (S.lifeOf S.alice settled) (Just 22)
              Spec.assertEqWith s "and bob lost two" (S.lifeOf S.bob settled) (Just 18)
              Spec.assertEqWith s "alice's Pridemate grew" (countersOn aliceMate settled) (Just 1)
              Spec.assertEqWith s "bob's Pridemate did not -- losing life is not gaining it" (countersOn bobMate settled) (Just 0)
        -- CR 119.9's last sentence: "if a player gains 0 life, no life gain event
        -- has occurred".
        --
        -- Hand-built, and honestly so: no card in the pool can hand applyDamage a
        -- 0-amount event, CR 510.1a dropping a creature that assigns 0 or less and
        -- Resolve's DealDamage arm guarding its own quantity. What this pins is
        -- therefore applyDamage's own contract -- the door a future producer would
        -- come through -- rather than a board a player could sit at.
        --
        -- Asserted on the LOG rather than through a counter, because the claim is
        -- about the RECORD: a counter assertion would also pass for an engine that
        -- recorded the zero and then declined to match it, which is not what the
        -- rule says. The 2-damage half is the paired control, so an empty answer
        -- cannot pass for the wrong reason.
        Spec.it s "CR 119.9 a 0-damage lifelink event records no life gain at all" $ do
          childOfNight <- S.printingOf s registry "Child of Night"
          let (oid, gs0) = S.addCreature childOfNight S.alice (Setup.emptyGame S.bothPlayers)
              evOf n = DamageEvent.MkDamageEvent oid (Recipient.ToPlayer S.bob) n False False False 0 (Just S.alice) DamageKind.Combat
              gainsIn gs = [p | GameEvent.LifeGained p _ <- S.eventsOf gs]
              after n = S.runPure S.identityAnswer gs0 (Damage.applyDamage [evOf n])
          Spec.assertEqWith s "two damage records the gain" (gainsIn (after 2)) [S.alice]
          Spec.assertEqWith s "zero damage records nothing" (gainsIn (after 0)) []

-- CR 119.9 read for its NUMBER, which the group above never asks for: Ajani's
-- Pridemate's payload names no amount, so nothing there could tell a bound amount
-- from an unbound one.
--
-- Sanguine Bond, {3}{B}{B} Enchantment, "Whenever you gain life, target opponent
-- loses that much life." CR 603.2 makes the amount part of the event that fired
-- the trigger, and Pawl.Engine.Event.eventBindings stamps it under
-- Pawl.Engine.Binding.eventAmount, which the card's LoseLife reads as an ordinary
-- Quantity.InSlot.
--
-- What makes this a proof rather than a demonstration is that the two gameplay
-- cases carry DIFFERENT amounts, from different producers:
--
--   * Renewed Faith's "you gain 6 life" -- 6, a number nothing else on that board
--     is (three Plains, a mana value of 3, two life totals of 20).
--   * Radiant Fountain's entry trigger -- 2.
--
-- One constant bound in place of the real amount therefore fails one of the two,
-- and a binding that read the gainer's LIFE TOTAL rather than the gain (26 and 22
-- respectively) fails both.
lifeGainAmountSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeGainAmountSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- The same staging strippedTriggerSpec's entry fixture uses: the permanent
      -- is placed, its Moved event recorded, and CR 603.6a's scan run at the next
      -- settle.
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved moved (Projection.project oid gs)] gs))
   in Spec.describe s "CR 119.9 that much life" $ do
        -- The gameplay-level proof, cast to resolution. alice's Renewed Faith
        -- gains her 6 (CR 119.3), the Bond's trigger matches that event, and its
        -- payload reads the SIX out of the slot -- bob's 20 becomes 14.
        --
        -- alice's own total is asserted too: an engine that made the Bond drain
        -- its controller would show the same 14 on bob only if it also failed
        -- here.
        Spec.it s "CR 603.2 whole cards: alice gains 6 from Renewed Faith and Sanguine Bond drains bob for 6" $ do
          plains <- S.printingOf s registry "Plains"
          sanguineBond <- S.printingOf s registry "Sanguine Bond"
          renewedFaith <- S.printingOf s registry "Renewed Faith"
          let (_, board) = S.addCreature sanguineBond S.alice (S.landsInPlay plains 3)
              (gs, spellId) = S.handOne renewedFaith board
              settled = resolveAll (snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId)))
          Spec.assertEqWith s "alice gained exactly 6" (S.lifeOf S.alice settled) (Just 26)
          Spec.assertEqWith s "and bob lost exactly that much" (S.lifeOf S.bob settled) (Just 14)
        -- The SECOND amount, from the other producer, on a board where nothing is
        -- 6: a Bond that bound a constant, or bound the amount from the wrong
        -- event, cannot pass both this and the case above.
        Spec.it s "CR 603.2 a gain of 2 drains 2, not the previous case's 6" $ do
          sanguineBond <- S.printingOf s registry "Sanguine Bond"
          radiantFountain <- S.printingOf s registry "Radiant Fountain"
          let (_, withBond) = S.addCreature sanguineBond S.alice (Setup.emptyGame S.bothPlayers)
              (fountainId, gs) = S.addCreature radiantFountain S.alice withBond
              settled = entering fountainId gs
          Spec.assertEqWith s "alice gained exactly 2" (S.lifeOf S.alice settled) (Just 22)
          Spec.assertEqWith s "and bob lost exactly that much" (S.lifeOf S.bob settled) (Just 18)
        -- eventBindings in isolation, so the binding is pinned to the RULE rather
        -- than to one card's payload -- becameSlotSpec's shape. The 7 is neither
        -- life total nor any other number in reach, so an arm binding anything but
        -- the event's own amount fails here.
        Spec.it s "CR 603.2 eventBindings binds the amount the event carries" $
          Spec.assertEqWith
            s
            "thatMuch is the gain"
            (Event.eventBindings (TriggerCondition.PlayerGainsLife PlayerRelation.You) (GameEvent.LifeGained S.alice 7))
            (Map.singleton Binding.eventAmount (Binding.toAmount 7))

-- The life-GAIN group's mirror: "whenever an opponent loses life", which the
-- rules give no CR 119.9 of its own. What counts as a loss is therefore fixed by
-- the three sites that RECORD one, and this group walks all three:
--
--   * CR 119.3, an effect that causes a player to lose life -- Sign in Blood's
--     "target player draws two cards and loses 2 life".
--   * CR 119.2 / 120.3a, damage dealt to a player by a source without infect --
--     Hill Giant's three.
--   * CR 119.4, life paid as a cost: "in other words, the player loses that much
--     life" -- Greed's "{B}, Pay 2 life: Draw a card".
--
-- Exquisite Blood, {4}{B} Enchantment, "Whenever an opponent loses life, you gain
-- that much life", is the card that proves it, and the first LIFE condition in the
-- pool whose relation is Opponent rather than You (Megrim's discard trigger is the
-- other one) -- so the loser and CR 109.5's "you" are never the same player, and a
-- matcher that ignored the relation would gain alice life off her own losses.
--
-- What makes the group a proof rather than a demonstration:
--
--   * the amounts DIFFER between the damage case (3) and the two others (2), and
--     no life total on any of these boards is 3 or 2 -- so a constant binding
--     fails one case and a total-reading binding fails all of them.
--   * the CR 109.5 control changes only WHO lost the life, on the same card, the
--     same spell and the same amount.
--   * the CR 120.3b control is on the SAME board and the SAME attack as the
--     damage case: Glistener Elf's infect damage gives poison counters INSTEAD of
--     causing life loss, so alice gains the Hill Giant's 3 and not the Elf's 1.
lifeLossTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lifeLossTriggerSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Sign in Blood's one target slot, answered with `who` rather than left to
      -- identityAnswer's lowest-sorting candidate -- which is alice, and so is
      -- the control case rather than the positive one.
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      -- alice: two Swamps for the {B}{B}, an Exquisite Blood, and Sign in Blood
      -- in hand.
      --
      -- BOTH players get two library cards, not only the one the positive case
      -- aims at, and this is load-bearing rather than tidy: Sign in Blood draws
      -- its target two cards as well as costing them the life, so a target with
      -- an empty library loses the game to CR 104.3c the next time a player would
      -- get priority -- before any trigger could resolve. The CR 109.5 control
      -- below would then be silent for THAT reason instead of the relation's, and
      -- would pass however the matcher read the relation. With a library on each
      -- side the two cases differ in the target and in nothing else.
      signInBloodBoard swamp blood signInBlood =
        let (_, withBlood) = S.addCreature blood S.alice (S.landsInPlay swamp 2)
            stock pid gs =
              let (_, one) = S.addLibraryCard swamp pid gs
                  (_, two) = S.addLibraryCard swamp pid one
               in two
         in S.handOne signInBlood (stock S.bob (stock S.alice withBlood))
   in Spec.describe s "PlayerLosesLife" $ do
        -- The gameplay-level proof, cast to resolution. bob loses 2 (CR 119.3),
        -- Exquisite Blood matches THAT event and gains alice the 2 it carried.
        Spec.it s "CR 119.3 whole cards: Sign in Blood costs bob 2 life and Exquisite Blood gains alice that much" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          signInBlood <- S.printingOf s registry "Sign in Blood"
          let (gs, spellId) = signInBloodBoard swamp blood signInBlood
              cast = snd (Engine.runGamePure (aimAt S.bob) gs (S.cast S.alice spellId))
              settled = resolveAll cast
          Spec.assertEqWith s "bob lost exactly 2" (S.lifeOf S.bob settled) (Just 18)
          Spec.assertEqWith s "and alice gained exactly that much" (S.lifeOf S.alice settled) (Just 22)
        -- The control twin, differing in ONE thing: the spell targets ALICE, so
        -- alice is the one who loses. The same card, the same 2 life, the same
        -- GameEvent.LifeLost written -- and "an opponent" is bob, so Exquisite
        -- Blood stays silent.
        --
        -- alice's loss is asserted too, or the case would pass for the wrong
        -- reason: an engine that recorded no loss at all would also show no gain.
        Spec.it s "CR 109.5/603.3a the control: ALICE loses the life, and her own Exquisite Blood stays silent" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          signInBlood <- S.printingOf s registry "Sign in Blood"
          let (gs, spellId) = signInBloodBoard swamp blood signInBlood
              cast = snd (Engine.runGamePure (aimAt S.alice) gs (S.cast S.alice spellId))
              settled = resolveAll cast
          Spec.assertEqWith s "alice really lost the 2" (S.lifeOf S.alice settled) (Just 18)
          Spec.assertEqWith s "bob lost nothing" (S.lifeOf S.bob settled) (Just 20)
        -- CR 119.2 / 120.3a: "damage dealt to a player by a source without infect
        -- causes that player to lose that much life". The second producer, and
        -- the one no effect says the words for -- combat did.
        --
        -- ONE board carries the control. Both of alice's creatures connect, and
        -- CR 120.3b sends Glistener Elf's damage to poison counters INSTEAD of a
        -- life loss, so the 3 alice gains is the Hill Giant's alone. An engine
        -- that read "a life total moved" would gain her 4.
        Spec.it s "CR 119.2 damage loses life and CR 120.3b infect does not, on one attack" $ do
          blood <- S.printingOf s registry "Exquisite Blood"
          hillGiant <- S.printingOf s registry "Hill Giant"
          glistenerElf <- S.printingOf s registry "Glistener Elf"
          let (gs0, _, _) = S.combatBoardOf [hillGiant, glistenerElf] []
              (_, gs1) = S.addCreature blood S.alice gs0
              settled = resolveAll (S.fightWith S.aggressiveAnswer gs1)
          Spec.assertEqWith s "bob lost the Giant's 3 and none of the Elf's 1" (S.lifeOf S.bob settled) (Just 17)
          Spec.assertEqWith s "the Elf really connected" (S.playerCounterOf PlayerCounterKind.Poison S.bob settled) 1
          Spec.assertEqWith s "so alice gained 3, not 4" (S.lifeOf S.alice settled) (Just 23)
        -- CR 119.4's "in other words, the player loses that much life". The third
        -- producer, and the only one that happens while paying a COST rather than
        -- while an effect resolves -- so the record is written outside resolution
        -- and the CR 117.5 trigger scan still has to find it.
        Spec.it s "CR 119.4 bob pays 2 life for Greed and Exquisite Blood gains alice that much" $ do
          swamp <- S.printingOf s registry "Swamp"
          blood <- S.printingOf s registry "Exquisite Blood"
          greed <- S.printingOf s registry "Greed"
          case Face.activatedAbilities (S.combinedFace greed) of
            [] -> Spec.assertFailure s "Greed should carry an activated ability"
            ability : _ -> do
              let (_, withBlood) = S.addCreature blood S.alice (Setup.emptyGame S.bothPlayers)
                  (_, withSwamp) = S.addCreature swamp S.bob withBlood
                  (greedId, withGreed) = S.addCreature greed S.bob withSwamp
                  (_, gs1) = S.addLibraryCard swamp S.bob withGreed
                  gs =
                    gs1
                      { GameState.phase = Phase.PrecombatMain,
                        GameState.activePlayer = S.alice,
                        GameState.priority = Just S.alice
                      }
                  activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.bob greedId ability)
                  settled = resolveAll activated
              Spec.assertEqWith s "bob paid exactly 2" (S.lifeOf S.bob settled) (Just 18)
              Spec.assertEqWith s "and alice gained exactly that much" (S.lifeOf S.alice settled) (Just 22)
        -- eventBindings in isolation, so the binding is pinned to the RULE rather
        -- than to one card's payload -- the gain group's last case, mirrored. The
        -- 7 is no life total and no other number in reach, so an arm binding
        -- anything but the event's own amount fails here.
        --
        -- Both slots at once, and as a WHOLE map rather than a lookup: CR 603.2
        -- makes the amount and the player one environment, and an equality on the
        -- whole map is what would catch an arm that bound a third thing.
        Spec.it s "CR 603.2 eventBindings binds the amount the loss event carries and the loser" $
          Spec.assertEqWith
            s
            "thatMuch is the loss and thatPlayer is who lost it"
            (Event.eventBindings (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent) (GameEvent.LifeLost S.bob 7))
            (Map.fromList [(Binding.eventAmount, Binding.toAmount 7), (Binding.triggerPlayer, Binding.toPlayer S.bob)])
        -- The loser is bound under the OTHER relation too, and that is a claim
        -- about the event rather than about the relation: CR 603.2's environment
        -- is what the event named, and Event.eventBindingSlots answers per
        -- CONDITION with no relation in hand, so a slot it promises has to hold
        -- for every relation the condition admits.
        Spec.it s "CR 603.2 the loser is bound under the You relation as well" $
          Spec.assertEqWith
            s
            "thatPlayer names the loser whichever relation matched"
            (Event.eventBindings (TriggerCondition.PlayerLosesLife PlayerRelation.You) (GameEvent.LifeLost S.alice 3))
            (Map.fromList [(Binding.eventAmount, Binding.toAmount 3), (Binding.triggerPlayer, Binding.toPlayer S.alice)])

-- CR 603.2's other half of a life-loss event: the PLAYER it named, not only the
-- amount. Mindcrank, {2} Artifact, "Whenever an opponent loses life, that player
-- mills that many cards" (CR 701.17a) -- the pool's first life trigger whose
-- payload acts on the player the event named rather than on CR 109.5's "you",
-- which is what makes `Binding.triggerPlayer` on this condition a slot something
-- reads rather than speculative construction.
--
-- THREE SEATS, and that is the whole design of the fixture. On a two-seat board
-- "that player" and "an opponent" name the same person, so an implementation that
-- milled SOME opponent -- the first one, say -- would pass with the binding
-- wrong. With bob and carol both opponents of alice, the two cases below differ
-- only in WHICH of them Sign in Blood targets, so a binding that answers a fixed
-- opponent fails whichever case is not that opponent, and a binding that answers
-- CR 109.5's "you" fails both. Confirmed by mutating each of the two in turn.
--
-- Every seat is stocked with six cards, which is load-bearing rather than tidy --
-- the lesson `lifeLossTriggerSpec`'s fixture already carries. Sign in Blood draws
-- its target two cards before it costs them the life, so a target whose library
-- ran out would lose to CR 104.3c the next time a player would get priority,
-- before the trigger could resolve, and the case would pass for that reason
-- instead of for the binding's.
mindcrankSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
mindcrankSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      -- Sign in Blood's one target slot, answered with `who` -- as the Exquisite
      -- Blood group's helper does, and for the same reason.
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      sizeOf zone pid gs = length (Game.zoneMembers zone pid gs)
      -- alice: two Swamps for the {B}{B}, a Mindcrank, and Sign in Blood in hand.
      -- bob and carol: nothing but libraries, so neither is distinguishable from
      -- the other by anything except being targeted.
      board swamp mindcrank signInBlood =
        let withMana = List.foldl' (\g _ -> snd (S.addCreature swamp S.alice g)) S.threePlayerGame [1 .. (2 :: Int)]
            (_, withCrank) = S.addCreature mindcrank S.alice withMana
            stock pid gs = List.foldl' (\g _ -> snd (S.addLibraryCard swamp pid g)) gs [1 .. (6 :: Int)]
         in S.handOne signInBlood (stock S.carol (stock S.bob (stock S.alice withCrank)))
      cases =
        [ ("bob", S.bob, S.carol),
          ("carol", S.carol, S.bob)
        ]
   in Spec.describe s "Mindcrank names the player who lost the life"
        . Foldable.for_ cases
        $ \(label, loser, bystander) ->
          -- CR 603.2: the targeted player takes the loss, and the SAME player
          -- mills 2. Six cards, less the two Sign in Blood draws them, less the
          -- two milled, leaves two -- while the other opponent's six are
          -- untouched, which is what a binding naming "an opponent" rather than
          -- THE opponent fails.
          Spec.it s ("CR 701.17a the player who lost the life is the player who mills, with " <> label <> " targeted") $ do
            swamp <- S.printingOf s registry "Swamp"
            mindcrank <- S.printingOf s registry "Mindcrank"
            signInBlood <- S.printingOf s registry "Sign in Blood"
            let (gs, spellId) = board swamp mindcrank signInBlood
                cast = snd (Engine.runGamePure (aimAt loser) gs (S.cast S.alice spellId))
                settled = resolveAll cast
            Spec.assertEqWith s "the targeted player really lost the 2" (S.lifeOf loser settled) (Just 18)
            Spec.assertEqWith s "and milled 2 into their own graveyard" (sizeOf Zone.Graveyard loser settled) 2
            Spec.assertEqWith s "leaving 6 - 2 drawn - 2 milled" (sizeOf Zone.Library loser settled) 2
            Spec.assertEqWith s "the OTHER opponent milled nothing" (sizeOf Zone.Graveyard bystander settled) 0
            Spec.assertEqWith s "and their library is whole" (sizeOf Zone.Library bystander settled) 6
            -- alice's graveyard holds Sign in Blood and nothing else: CR 109.5's
            -- "you" is the wrong answer here, and this is what says so.
            Spec.assertEqWith s "alice, who controls Mindcrank, milled nothing" (sizeOf Zone.Graveyard S.alice settled) 1
            Spec.assertEqWith s "and lost no life either" (S.lifeOf S.alice settled) (Just 20)

-- CR 508.3a / 603.3d: Anafenza, the Foremost's OTHER ability -- "whenever this
-- creature attacks, put a +1/+1 counter on another target tapped creature you
-- control". Here because the card was added for its CR 614.1a redirect
-- (Pawl.EventSpec's Anafenza group), and a card's second ability is not exercised
-- by the first one's tests.
--
-- The target filter is `And [Not IsSource, IsTapped, ControlledBy You]`, and the
-- board gives each conjunct exactly one thing to reject: Anafenza herself is
-- tapped and hers, so only "another" keeps her out; the Wall of Stone is hers and
-- not her, so only being untapped does (CR 702.3b keeps it home, so declaring
-- attackers never taps it); and bob's Piker is tapped and not her, so only its
-- controller does. The Piker attacking beside her satisfies all three -- CR
-- 508.1f taps a declared attacker -- and is the only legal target.
anafenzaAttackSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
anafenzaAttackSpec s registry =
  let countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- Records every CR 601.2c legal-recipient set offered, verbatim, and
      -- answers everything aggressively -- which declares every legal attacker,
      -- so the declaration really happens.
      --
      -- The LEGAL SET is what this asserts on rather than only the outcome, and
      -- that is the difference between a discriminating test and a passing one:
      -- with the Piker the lowest-id candidate, an answerer that takes the first
      -- offer reaches the same board whether or not the filter rejected anything.
      recordTargets :: Prompt.Prompt r -> State.State [Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient)] r
      recordTargets p = case p of
        Prompt.ChooseTargets _ _ _ sets -> do
          State.modify' (<> [sets])
          pure (S.aggressiveAnswer p)
        _ -> pure (S.aggressiveAnswer p)
   in Spec.describe s "Anafenza attacks" . Spec.it s "CR 508.3a the attack trigger counters another tapped creature its controller controls" $ do
        anafenza <- S.printingOf s registry "Anafenza, the Foremost"
        piker <- S.printingOf s registry "Goblin Piker"
        wallOfStone <- S.printingOf s registry "Wall of Stone"
        case S.combatBoardOf [anafenza, piker, wallOfStone] [piker] of
          (gs0, [anafenzaId, pikerId, wallId], [theirs]) -> do
            -- bob's Piker is TAPPED, so `ControlledBy You` is the only conjunct
            -- keeping it out of the offer. Left untapped it would be rejected by
            -- IsTapped instead, and the assertion would hold with the
            -- controller clause deleted.
            let gs = S.tapObject theirs gs0
                ((_, settled), offered) =
                  State.runState (Engine.runGame recordTargets gs (Engine.runStep >> Engine.priorityLoop)) []
            Spec.assertEqWith
              s
              "the Piker attacking beside her is the only legal target"
              (fmap (fmap snd . Map.elems) offered)
              [[Set.singleton (Recipient.ToCreature pikerId)]]
            Spec.assertEqWith s "and it took the counter" (countersOn pikerId settled) (Just 1)
            Spec.assertEqWith s "\"another\" keeps Anafenza off her own trigger" (countersOn anafenzaId settled) (Just 0)
            Spec.assertEqWith s "an untapped creature is not a legal target" (countersOn wallId settled) (Just 0)
            Spec.assertEqWith s "and neither is a creature bob controls" (countersOn theirs settled) (Just 0)
          _ -> Spec.assertFailure s "fixture should give alice Anafenza, a Piker and a Wall, and bob a Piker"

-- CR 122.1's experience counters READ, with Ezuri, Claw of Progress {2}{G}{U}
-- Legendary Creature -- Phyrexian Elf Warrior 3/3: "Whenever a creature you
-- control with power 2 or less enters, you get an experience counter. At the
-- beginning of combat on your turn, put X +1/+1 counters on another target
-- creature you control, where X is the number of experience counters you have."
--
-- permanentDiesSpec above is where the counters are HANDED OUT, with Meren of
-- Clan Nel Toth. Nothing counted them until this card: an experience counter is
-- CR 122.1's bare first sentence and no rule reads one, so the only possible
-- reader is a card's own text, and the pool had none.
--
-- Both of Ezuri's abilities are triggered, which is why the whole card sits in
-- this spec rather than being split. The first is CR 603.6a's second written
-- form ("whenever a [type] enters") narrowed by a POWER CEILING, and the second is
-- a CR 603.2b step trigger whose Quantity is Quantity.PlayerCounters -- the arm
-- CR 728.1's rad mill already used for a rule, aimed for the first time at a
-- counter kind only card text can see.
--
-- Every number on these boards is arranged not to coincide, because arithmetic
-- is all this card does. The target's printed 2/1 is not the experience count
-- (3, then 5), the count is not the number of creatures its controller controls
-- (5, then 2), and the two counts differ from each other -- so a payload that
-- added a constant, counted the board, or read the wrong counter kind lands on a
-- power and toughness no assertion here accepts.
ezuriExperienceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ezuriExperienceSpec s registry =
  let experienceOf = S.playerCounterOf PlayerCounterKind.Experience
      countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- The board sitting in pid's beginning of combat step -- CR 506.1's first
      -- combat step, rule 507 -- which is the moment Ezuri's second ability
      -- names. Staged directly, as Pawl.RadSpec stages its precombat main phase,
      -- because Engine.runStep is what writes the CR 603.2b StepBegan record this
      -- trigger matches.
      atBeginningOfCombat pid gs =
        gs
          { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
            GameState.activePlayer = pid,
            GameState.priority = Just pid
          }
      -- Every target slot aimed at one object, where S.identityAnswer would take
      -- the least Recipient -- which on the first board below is one of the three
      -- Pikers rather than the permanent every assertion is about.
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
        _ -> S.identityAnswer p
      -- alice casts the spell in her hand and lets the stack empty, so the spell
      -- resolves and so does whatever Ezuri's entry trigger put on top of it.
      castAndResolve sid gs =
        let onStack = S.runPure S.identityAnswer gs (S.cast S.alice sid)
         in S.runPure S.identityAnswer onStack Engine.priorityLoop
      -- alice's Ezuri beside one Bonded Construct, and nothing else. The
      -- Construct is ARRANGED rather than cast, so it contributes no enters
      -- event and no experience counter of its own -- every counter on these
      -- boards is one the test put there deliberately.
      ezuriAndTarget = do
        ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
        construct <- S.printingOf s registry "Bonded Construct"
        let (ezuriId, withEzuri) = S.addCreature ezuri S.alice (Setup.emptyGame S.bothPlayers)
            (targetId, gs) = S.addCreature construct S.alice withEzuri
        pure (ezuriId, targetId, gs)
   in Spec.describe s "Ezuri, Claw of Progress" $ do
        -- The whole arc #858 asks for, at gameplay level: alice CASTS three
        -- small creature spells, the counters accumulate on her, and a
        -- permanent's size changes by exactly that many. The Construct she
        -- already had is the target, so its printed 2/1 is untouched by the
        -- casting and 5/4 can only be 2/1 plus three.
        Spec.it s "CR 122.1 three cast creature spells become three experience counters, and the combat trigger spends them" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          construct <- S.printingOf s registry "Bonded Construct"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let (_, withEzuri) = S.addCreature ezuri S.alice (S.landsInPlay mountain 6)
              (targetId, board) = S.addCreature construct S.alice withEzuri
              (gs0, firstPiker) = S.handOne piker board
              (secondPiker, gs1) = S.addHandCard piker S.alice gs0
              (thirdPiker, gs2) = S.addHandCard piker S.alice gs1
              cast = castAndResolve thirdPiker (castAndResolve secondPiker (castAndResolve firstPiker gs2))
              combat = S.runPure (aimAt targetId) (atBeginningOfCombat S.alice cast) (Engine.runStep >> Engine.priorityLoop)
          Spec.assertEqWith s "alice started with no experience" (experienceOf S.alice gs2) 0
          Spec.assertEqWith s "three 2/1 spells resolved, so three experience counters" (experienceOf S.alice cast) 3
          Spec.assertEqWith s "bob, who cast nothing, has none" (experienceOf S.bob cast) 0
          Spec.assertEqWith s "the Construct took one +1/+1 counter per experience counter" (countersOn targetId combat) (Just 3)
          Spec.assertEqWith s "so its printed 2/1 reads 5/4" (S.powerToughnessOf targetId combat) (Just (5, 4))
          -- READING a player's counters is not removing them, and CR 728.1's rad
          -- mill -- the pool's other user of this Quantity, which removes one
          -- counter per nonland card it milled -- is why that is worth an
          -- assertion. Ezuri's printed text says only "the number of experience
          -- counters you have", so alice keeps all three.
          Spec.assertEqWith s "and alice still has all three experience counters" (experienceOf S.alice combat) 3
        -- The control at a DIFFERENT count, which is what stops a payload that
        -- hardcodes three from passing the case above. Same two permanents, five
        -- counters instead of three, and 2/1 reads 7/6.
        --
        -- The offered target set is asserted too, because the outcome alone does
        -- not discriminate: with only two creatures on the board, an answerer
        -- taking the first offer reaches the same place whether or not "another"
        -- rejected Ezuri.
        Spec.it s "CR 122.1 five experience counters put five, and \"another\" keeps Ezuri off her own trigger" $ do
          (ezuriId, targetId, board) <- ezuriAndTarget
          let gs = S.addPlayerCounter PlayerCounterKind.Experience 5 S.alice board
              recordTargets :: Prompt.Prompt r -> State.State [Map.Map SlotName.SlotName (Natural, Set.Set Recipient.Recipient)] r
              recordTargets p = case p of
                Prompt.ChooseTargets _ _ _ sets -> do
                  State.modify' (<> [sets])
                  pure (aimAt targetId p)
                _ -> pure (aimAt targetId p)
              ((_, combat), offered) =
                State.runState (Engine.runGame recordTargets (atBeginningOfCombat S.alice gs) (Engine.runStep >> Engine.priorityLoop)) []
          Spec.assertEqWith
            s
            "the Construct is the only legal target"
            (fmap (fmap snd . Map.elems) offered)
            [[Set.singleton (Recipient.ToCreature targetId)]]
          Spec.assertEqWith s "five counters, not three" (countersOn targetId combat) (Just 5)
          Spec.assertEqWith s "so its printed 2/1 reads 7/6" (S.powerToughnessOf targetId combat) (Just (7, 6))
          Spec.assertEqWith s "and Ezuri, whom \"another\" excludes, took none" (countersOn ezuriId combat) (Just 0)
          Spec.assertEqWith s "leaving her printed 3/3" (S.powerToughnessOf ezuriId combat) (Just (3, 3))
        -- ZERO, the case a "for each" that quietly means "one" would pass. The
        -- ability still triggers and still resolves -- CR 603.2b says nothing
        -- about the count -- so the Construct staying 2/1 has to come from the
        -- Quantity reading 0 rather than from nothing happening, and the stack
        -- assertion is what tells those apart.
        Spec.it s "CR 122.1 no experience counters put no +1/+1 counters, though the ability still resolves" $ do
          (_, targetId, board) <- ezuriAndTarget
          let staged = S.withEvents [GameEvent.StepBegan (Phase.Combat CombatStep.BeginningOfCombat) S.alice] (atBeginningOfCombat S.alice board)
              settled = S.runPure (aimAt targetId) staged Engine.settleForPriority
              combat = S.runPure (aimAt targetId) (atBeginningOfCombat S.alice board) (Engine.runStep >> Engine.priorityLoop)
          Spec.assertEqWith s "alice has no experience counters" (experienceOf S.alice board) 0
          Spec.assertEqWith s "the ability went on the stack anyway" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "no +1/+1 counter was put" (countersOn targetId combat) (Just 0)
          Spec.assertEqWith s "so the Construct keeps its printed 2/1" (S.powerToughnessOf targetId combat) (Just (2, 1))
        -- "WITH POWER 2 OR LESS", the Filter.PowerAtMost arm. Hill Giant is 3/3
        -- and Goblin Piker is 2/1, so the same Ezuri pays one experience counter
        -- for the second and nothing for the first. BOTH halves are here, because
        -- a filter that always rejected and one that always admitted are told
        -- apart only by running both.
        Spec.it s "CR 208.1 power 2 or less: a 3/3 entering pays nothing, a 2/1 pays one" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          hillGiant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          mountain <- S.printingOf s registry "Mountain"
          let boardWith n = snd (S.addCreature ezuri S.alice (S.landsInPlay mountain n))
              (giantGs, giantSpell) = S.handOne hillGiant (boardWith 4)
              (pikerGs, pikerSpell) = S.handOne piker (boardWith 2)
          Spec.assertEqWith s "the 3/3 gives alice nothing" (experienceOf S.alice (castAndResolve giantSpell giantGs)) 0
          Spec.assertEqWith s "the 2/1 gives her one" (experienceOf S.alice (castAndResolve pikerSpell pikerGs)) 1
        -- "YOU CONTROL", read through CR 109.5 against the ability's controller
        -- (CR 603.3a). bob's 2/1 entering in front of alice's Ezuri is a creature
        -- with power 2 or less entering, and it pays nobody.
        Spec.it s "CR 109.5 you control: an opponent's 2/1 entering gives alice nothing" $ do
          ezuri <- S.printingOf s registry "Ezuri, Claw of Progress"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withEzuri) = S.addCreature ezuri S.alice (Setup.emptyGame S.bothPlayers)
              (_, entered) = S.entersWithTrigger piker S.bob withEzuri
              after = S.runPure S.identityAnswer entered (Engine.settleForPriority >> Engine.priorityLoop)
          Spec.assertEqWith s "alice gets no experience counter" (experienceOf S.alice after) 0
          Spec.assertEqWith s "and neither does bob, who has no Ezuri" (experienceOf S.bob after) 0

-- CR 601.2i's cast trigger with a payload aimed at a TARGET PLAYER: the pool's
-- first card to hand out poison counters (CR 122.1f, whose tenth loses the game
-- under CR 704.5c) to a player who was CHOSEN rather than derived from the
-- ability's controller (#120).
--
-- Hand of the Praetors, {3}{B} Creature -- Phyrexian Zombie 3/2: "Infect. Other
-- creatures you control with infect get +1/+1. Whenever you cast a creature
-- spell with infect, target player gets a poison counter." Only the third line
-- is this group's subject. The anthem is Pawl.PowerToughnessSpec's, and what the
-- printed infect keyword does to damage is Pawl.DamageSpec's ground already (CR
-- 702.90b).
--
-- The printed condition narrows THREE things in one sentence -- who cast it (CR
-- 109.5's "you", which for a triggered ability is CR 603.3a's controller of the
-- source at the trigger moment), that it was a creature spell, and that it had
-- infect (CR 702.90) -- and the Filter carries all three. Each case below moves
-- exactly one of them, so a Filter that always answered True is distinguishable
-- from one that reads each half.
--
-- THREE SEATS, which the PAYLOAD wants as much as the condition does. On a
-- two-seat board with alice casting, "target player" answered as bob and "an
-- opponent" put the counter in the same place. carol is the seat that separates
-- them: she is a legal target that was not chosen, so an effect that poisoned
-- every opponent fails here too. She serves the condition's "you cast" case for
-- Young Pyromancer's reason as well -- "bob cast it" is not "an opponent cast
-- it" until someone else is sitting there.
handOfThePraetorsSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
handOfThePraetorsSpec s registry =
  let poisonOf = S.playerCounterOf PlayerCounterKind.Poison
      -- The trigger's one target slot, answered with `who` rather than left to
      -- S.identityAnswer, whose lowest-sorting candidate on this board is alice
      -- -- the caster, and so the wrong answer to prove anything with.
      aimAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
        _ -> S.identityAnswer p
      -- alice bears the Hand; alice and bob each get two Forests (Glistener
      -- Elf's {G}) and two Mountains (Goblin Piker's {1}{R}). carol gets no
      -- land: she never casts, and is only ever a seat the counter must miss.
      board forest mountain hand =
        let addLands pid n printing g = List.foldl' (\g' _ -> snd (S.addCreature printing pid g')) g [1 .. (n :: Int)]
            withLands =
              addLands S.bob 2 mountain
                . addLands S.bob 2 forest
                . addLands S.alice 2 mountain
                $ addLands S.alice 2 forest S.threePlayerGame
            (_, withHand) = S.addCreature hand S.alice withLands
         in withHand
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve who caster oid gs = S.runPure (aimAt who) (S.runPure (aimAt who) gs (S.cast caster oid)) Engine.priorityLoop
   in Spec.describe s "Hand of the Praetors" $ do
        -- THE case: the counter lands on the player the answerer named, and on
        -- nobody else. Glistener Elf, {G} Creature -- Phyrexian Elf Warrior 1/1
        -- with infect, is the spell cast.
        Spec.it s "CR 601.2i casting an infect creature spell poisons the TARGETED player" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let (elfId, gs) = S.addHandCard elf S.alice (board forest mountain hand)
              after = castAndResolve S.bob S.alice elfId gs
          Spec.assertEqWith s "nobody is poisoned before the cast" (poisonOf S.bob gs) 0
          Spec.assertEqWith s "bob, who was targeted, has one poison counter" (poisonOf S.bob after) 1
          -- The falsifier for a payload plumbed to the ability's controller:
          -- alice cast it and alice gets nothing.
          Spec.assertEqWith s "alice, who cast it, has none" (poisonOf S.alice after) 0
          -- And the falsifier for one plumbed to every opponent.
          Spec.assertEqWith s "and carol, who was not targeted, has none" (poisonOf S.carol after) 0
        -- The same board and the same answerer, aimed the other way: alice may
        -- target herself, since CR 115.1 puts every player in the pool and
        -- nothing on this card narrows it. A payload that read the caster would
        -- pass this case and fail the one above, and a payload that read an
        -- opponent would do the reverse -- neither passes both.
        Spec.it s "CR 115.1 the same trigger aimed at its own controller poisons her instead" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let (elfId, gs) = S.addHandCard elf S.alice (board forest mountain hand)
              after = castAndResolve S.alice S.alice elfId gs
          Spec.assertEqWith s "alice, who targeted herself, has one" (poisonOf S.alice after) 1
          Spec.assertEqWith s "bob has none" (poisonOf S.bob after) 0
          Spec.assertEqWith s "and neither has carol" (poisonOf S.carol after) 0
        -- The INFECT half of the Filter, moved on its own: alice still casts, and
        -- what she casts is still a creature spell. Goblin Piker, {1}{R} Creature
        -- -- Goblin Warrior 2/1, has no keyword at all.
        Spec.it s "CR 702.90 a creature spell WITHOUT infect fires nothing" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, gs) = S.addHandCard piker S.alice (board forest mountain hand)
              after = castAndResolve S.bob S.alice pikerId gs
          -- Positive control: the cast really happened and really resolved, so
          -- the silence below is the Filter's answer rather than a fixture that
          -- could not pay for anything.
          Spec.assertEqWith s "the Piker resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName piker) S.alice after) 1
          Spec.assertEqWith s "and nobody is poisoned" (poisonOf S.bob after) 0
          Spec.assertEqWith s "not even the caster" (poisonOf S.alice after) 0
        -- The "you cast" half, moved on its own: the same infect creature spell,
        -- cast from the seat to alice's left. carol makes "bob cast it" a
        -- different statement from "an opponent cast it".
        Spec.it s "CR 109.5 'you cast': an OPPONENT's infect creature spell fires nothing" $ do
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          hand <- S.printingOf s registry "Hand of the Praetors"
          elf <- S.printingOf s registry "Glistener Elf"
          let base = board forest mountain hand
              (bobsElf, withBobs) = S.addHandCard elf S.bob base
              (alicesElf, gs) = S.addHandCard elf S.alice withBobs
              byBob = castAndResolve S.bob S.bob bobsElf gs
              byAlice = castAndResolve S.bob S.alice alicesElf gs
          Spec.assertEqWith s "bob's own cast poisons nobody" (poisonOf S.bob byBob) 0
          Spec.assertEqWith s "not alice" (poisonOf S.alice byBob) 0
          Spec.assertEqWith s "and not carol" (poisonOf S.carol byBob) 0
          -- The same board, one caster apart: alice casting her own copy is what
          -- proves the seat is the only thing the silence above turns on.
          Spec.assertEqWith s "the same board poisons bob for alice's own cast" (poisonOf S.bob byAlice) 1

-- CR 603.3b's TWO-PART placement, verbatim: "First, each player, in APNAP order,
-- puts each triggered ability they control with a trigger condition that isn't
-- another ability triggering on the stack in any order they choose. ... Second,
-- each player, in APNAP order, puts all remaining triggered abilities they
-- control on the stack in any order they choose."
--
-- The pool's one producer of the second class, and the only printed card of the
-- shape: Historian's Boon, "{3}{W} Enchantment -- Whenever the final chapter
-- ability of a Saga you control triggers, create a 4/4 white Angel creature token
-- with flying and vigilance", paired with History of Benalia.
--
-- Two lore counters on the Saga and alice's precombat main phase: CR 714.3c's
-- turn-based action puts the third on, chapter III triggers (CR 714.2b), and the
-- Boon's second ability triggers off THAT triggering. Chapter III is first-class
-- and the Boon second-class, so the Boon goes on the stack above it and resolves
-- first, whatever order alice would have preferred.
--
-- WHAT THIS ASSERTS AND WHAT IT DOES NOT. The resulting BOARD is the same either
-- way -- chapter III pumps Knights alice controls and the Boon's token is an
-- Angel -- so the assertion is on stack order and on what alice was asked, both
-- of which a player sees, and not on a board difference. A Saga whose final
-- chapter read "creatures you control" would give a stronger discriminator; none
-- is in the pool.
--
-- `orderReversing` is what keeps the stack assertion from passing for the wrong
-- reason. Under a single APNAP pass alice controls both triggers and IS asked,
-- and this answerer names chapter III LAST -- which puts it on TOP and makes it
-- resolve first, the exact inversion. Under the rule's two passes she controls
-- one trigger in each and is asked nothing at all, so the answerer is inert. That
-- is why the recorded prompt list is expected EMPTY rather than merely free of
-- mixed batches: CR 603.3b's own-order choice is offered inside a pass.
secondPlacementPassSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
secondPlacementPassSpec s registry =
  let boardOf benalia boon =
        let (sagaId, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
            (boonId, withBoon) = S.addCreature boon S.alice base
            withCounters = S.addCounter CounterKind.Lore 2 sagaId withBoon
            ready =
              withCounters
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
         in (sagaId, boonId, S.runPure S.identityAnswer ready (Engine.runTurnBasedActions Phase.PrecombatMain))
      -- Answers CR 603.3b's ordering prompt with the offered indices REVERSED --
      -- the canonical order names the chapter ability first, so this names it
      -- last, and the answer is the order the triggers are PUT on the stack --
      -- while recording every batch it was offered.
      orderReversing :: Prompt.Prompt r -> State.State [[TriggerEntry.TriggerEntry]] r
      orderReversing p = case p of
        Prompt.OrderTriggers _ _ entries -> do
          State.modify' (<> [entries])
          pure (reverse (zipWith const [0 ..] entries))
        _ -> pure (S.identityAnswer p)
      -- The stack from the TOP down, named by each triggered ability's source (CR
      -- 113.7). Anything on the stack that is not a triggered ability is dropped,
      -- and nothing here puts one there.
      triggerSourcesOnStack gs =
        Maybe.mapMaybe
          ( \sid -> case fmap Object.source (Game.lookupObject sid gs) of
              Just (Source.OfTrigger srcId _) -> Just srcId
              _ -> Nothing
          )
          (GameState.stack gs)
      angelToken = CardName.MkCardName (Text.pack "Angel Token")
   in Spec.describe s "SecondPlacementPass" $ do
        Spec.it s "CR 603.3b the ability triggered by another ability's triggering is placed SECOND, so it resolves first" $ do
          benalia <- S.printingOf s registry "History of Benalia"
          boon <- S.printingOf s registry "Historian's Boon"
          let (sagaId, boonId, advanced) = boardOf benalia boon
              ((_, settled), asked) = State.runState (Engine.runGame orderReversing advanced Engine.settleForPriority) []
          Spec.assertEqWith s "the turn-based action put the third lore counter on" (S.counterOf CounterKind.Lore sagaId advanced) 3
          -- Both triggered, and the Boon's is ABOVE chapter III -- the whole of CR
          -- 603.3b's second pass, read off the stack the player sees.
          Spec.assertEqWith s "the Boon's trigger sits on top of the Saga's chapter III" (triggerSourcesOnStack settled) [boonId, sagaId]
          -- Not merely "an ability of the Saga": the one underneath must be the
          -- FINAL chapter, since that is what the Boon's condition names.
          Spec.assertEqWith s "and the one underneath is chapter III" (chaptersOnStackFrom sagaId settled) [3]
          -- The secondary check, and secondary because it can hold vacuously: with
          -- one trigger in each pass alice has nothing to order, so she is asked
          -- nothing. Under a single pass she is asked once, with both entries.
          Spec.assertEqWith s "alice was never offered the two abilities in one ordering" (fmap length asked) []
        Spec.it s "CR 603.3b the Boon's Angel really arrives, and CR 704.5s still takes the Saga" $ do
          benalia <- S.printingOf s registry "History of Benalia"
          boon <- S.printingOf s registry "Historian's Boon"
          let (sagaId, _, advanced) = boardOf benalia boon
              ((_, resolved), _) = State.runState (Engine.runGame orderReversing advanced Engine.priorityLoop) []
          Spec.assertEqWith s "one Angel token" (S.countOnBattlefieldByName angelToken S.alice resolved) 1
          -- The whole printed payload, so the card's data is exercised and not
          -- merely its name: "a 4/4 white Angel creature token with flying and
          -- vigilance". Chapter III creates nothing, so the one token is it.
          case S.tokensOf resolved of
            [token] -> do
              Spec.assertEqWith s "4/4" (S.powerToughnessOf token resolved) (Just (4, 4))
              Spec.assertEqWith s "an Angel" (Projection.subtypesOf token resolved) (Set.singleton Subtype.Angel)
              Spec.assertBool s (Map.member Keyword.Type.Flying (Projection.keywordsOf token resolved)) "with flying"
              Spec.assertBool s (Map.member Keyword.Type.Vigilance (Projection.keywordsOf token resolved)) "and vigilance"
            other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
          Spec.assertBool s (not (S.onBattlefield sagaId resolved)) "and the Saga's story is told, so CR 704.5s sacrifices it"

-- The chapter numbers of `oid`'s own chapter abilities currently on the stack (CR
-- 714.2, CR 704.5s).
chaptersOnStackFrom :: ObjectId.ObjectId -> GameState.GameState -> [Natural]
chaptersOnStackFrom oid gs =
  let from sid = case fmap Object.source (Game.lookupObject sid gs) of
        Just (Source.OfTrigger srcId ability) | srcId == oid -> Saga.chapterOf ability
        _ -> Nothing
   in Maybe.mapMaybe from (GameState.stack gs)

-- Custodi Lich, {3}{B}{B} Creature -- Zombie Cleric 4/2: "When this creature
-- enters, you become the monarch. Whenever you become the monarch, target player
-- sacrifices a creature of their choice." Both printed sentences are in
-- data/cards/custodi-lich.json; nothing is omitted.
--
-- The pool's producer for TriggerCondition.PlayerBecomesMonarch (CR 725.1). The
-- card is its own trigger's cause -- the first ability crowns its controller and
-- the second watches that crowning -- which makes the whole chain observable off
-- one entry, and CR 725.2's crown steal reaches the same condition by a route
-- the card has nothing to do with.
--
-- THREE SEATS throughout. At two players "you" and "an opponent" name
-- complementary halves of a two-element set, so a relation-free arm and a You
-- arm agree on every board; the third seat is what makes crowning somebody who
-- is neither the Lich's controller nor the sacrifice victim expressible.
--
-- Distinct power/toughness on every creature (Lich 4/2, Boggart Brute 3/2,
-- Goblin Piker 2/1, Bird Maiden 1/2, Bog Wraith 3/3) so no assertion below can
-- pass on a numeric coincidence, and the edict's victim always holds TWO
-- creatures so CR 701.21a's choice is a real prompt rather than a forced single
-- candidate -- bob in most cases, carol in the CR 725.4 one, where bob leaves.
monarchTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
monarchTriggerSpec s registry =
  let -- Names `victim` for every target slot that offers them. S.identityAnswer
      -- picks the least Recipient, which would aim the edict at alice herself.
      targetsPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      targetsPlayer victim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer victim) sets
        _ -> S.identityAnswer p
      resolveAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      resolveAll answer gs = snd (Engine.runGamePure answer gs Engine.priorityLoop)
      -- bob's two creatures and carol's one, on top of whatever the caller
      -- built. carol is the control seat: nothing in either test should ever
      -- touch her, so a payload that hit "a player" rather than the targeted one
      -- is visible.
      bystanders piker birdMaiden bogWraith base =
        let (_, g1) = S.addCreature piker S.bob base
            (_, g2) = S.addCreature birdMaiden S.bob g1
         in snd (S.addCreature bogWraith S.carol g2)
      -- CR 725.2's crown steal, driven by the damage EVENT rather than by a full
      -- combat: Monarch.inherentMatch reads the recorded DamageEvent, and
      -- ExpirySpec's monarch group drives the same rule the same way.
      combatDamageTo monarch damager =
        S.withEvents [GameEvent.DamageDealt (DamageEvent.MkDamageEvent damager (Recipient.ToPlayer monarch) 2 False False False 0 Nothing DamageKind.Combat)]
   in Spec.describe s "MonarchTrigger" $ do
        -- The whole chain off one entry: CR 603.6a's entry trigger crowns alice,
        -- Effect.BecomeMonarch records CR 725.1's event, and the second ability
        -- matches it.
        Spec.it s "CR 725.1 Custodi Lich whole card: entering crowns alice, and that crowning fires her edict" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (Setup.emptyGame S.threePlayers)
              (lich, gs) = S.entersWithTrigger custodiLich S.alice base
              after = resolveAll (targetsPlayer S.bob) gs
          Spec.assertEqWith s "no monarch before the Lich resolved its entry trigger" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "CR 725.1 alice is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the crowning recorded its event"
          Spec.assertEqWith s "CR 701.21a the targeted bob lost exactly one of his two" (S.creaturesInPlay S.bob after) 1
          Spec.assertEqWith s "carol, untargeted, lost none" (S.creaturesInPlay S.carol after) 1
          Spec.assertBool s (S.onBattlefield lich after) "and alice's own Lich is untouched"
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []
        -- CR 603.3a / 109.5: the relation is read against the ABILITY'S
        -- CONTROLLER, so a crowning of somebody else is silence. Denethor, Stone
        -- Seer's "target player becomes the monarch" is the pool's one way to
        -- crown a chosen player, and it records the very same event the test
        -- above matched -- so what separates the two tests is WHO was crowned and
        -- nothing else.
        Spec.it s "CR 603.3a/109.5 a crowning of bob does not fire alice's Custodi Lich" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          denethor <- S.printingOf s registry "Denethor, Stone Seer"
          mountain <- S.printingOf s registry "Mountain"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (Setup.emptyGame S.threePlayers)
              lands = List.foldl' (\g _ -> snd (S.addCreature mountain S.alice g)) base [1 .. 4 :: Int]
              -- addCreature, not entersWithTrigger: the Lich is ALREADY on the
              -- battlefield with its entry trigger long since resolved, so the
              -- only crowning in this test is Denethor's.
              (lich, g1) = S.addCreature custodiLich S.alice lands
              (denethorId, g2) = S.addCreature denethor S.alice g1
              gs = g2 {GameState.priority = Just S.alice}
              -- Denethor's two slots, named separately (CR 601.2c lets one
              -- ability write "target" twice): the crown goes to bob, and the 3
              -- damage to CAROL the player, so nothing on the board dies and a
              -- creature count that moved can only have been a sacrifice. Any
              -- OTHER slot -- which today means only the Lich's edict, if it
              -- wrongly fired -- takes bob, so a trigger that should have stayed
              -- silent is loud when it does not.
              denethorAnswers = Map.fromList [(SlotName.MkSlotName (Text.pack "player"), Recipient.ToPlayer S.bob), (SlotName.MkSlotName (Text.pack "damage"), Recipient.ToPlayer S.carol)]
              answer :: Prompt.Prompt r -> r
              answer p = case p of
                Prompt.ChooseTargets _ _ _ sets ->
                  Map.mapWithKey
                    ( \slot offer ->
                        let wanted = Map.findWithDefault (Recipient.ToPlayer S.bob) slot denethorAnswers
                         in Map.findWithDefault Set.empty slot (S.preferring (== wanted) (Map.singleton slot offer))
                    )
                    sets
                _ -> S.identityAnswer p
              activated = case Face.activatedAbilities (S.combinedFace denethor) of
                ability : _ -> S.runPure answer gs (Activate.activateAbility S.alice denethorId ability)
                [] -> gs
              after = resolveAll answer activated
          Spec.assertEqWith s "no monarch going in" (GameState.monarch gs) Nothing
          Spec.assertEqWith s "CR 725.1 bob, the targeted player, took the crown" (GameState.monarch after) (Just S.bob)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.bob) (S.eventsOf after)) "and the event names bob, so there really was a crowning to match"
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich is still on the battlefield to have watched it"
          -- The discriminating trio: nobody sacrificed anything. Under a
          -- relation-free arm bob would have lost one, and under an inverted
          -- relation so would whoever the edict targeted.
          Spec.assertEqWith s "bob kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          -- The ability really resolved in full, so "no sacrifice" cannot mean
          -- "nothing happened": carol took Denethor's 3.
          Spec.assertEqWith s "CR 115.4 carol, the any-target, took the 3" (S.lifeOf S.carol after) (Just 17)
          Spec.assertEqWith s "the stack is empty, so no trigger is waiting" (GameState.stack after) []
        -- CR 725.2's crown steal reaches the SAME condition by a route the card
        -- has nothing to do with: the inherent ability has no source, and
        -- Monarch.inherentMatch rather than Event.matchesTrigger is what fires
        -- it. What the Lich matches is the crowning, not the entry that usually
        -- causes one.
        Spec.it s "CR 725.2 a stolen crown is a crowning, and fires the same trigger" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              (lich, g1) = S.addCreature custodiLich S.alice base
              (brute, gs) = S.addCreature boggartBrute S.alice g1
              after = resolveAll (targetsPlayer S.bob) (combatDamageTo S.bob brute gs)
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch gs) (Just S.bob)
          Spec.assertEqWith s "CR 725.2 alice's creature took it off him" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (S.onBattlefield lich after) "the Lich watched from the battlefield"
          Spec.assertEqWith s "CR 725.1 alice's trigger fired: the targeted bob sacrificed one" (S.creaturesInPlay S.bob after) 1
          Spec.assertEqWith s "carol lost none" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
        -- The discriminating twin of the test above: the SAME board, the same
        -- inherent ability, the same event shape -- only the creature that dealt
        -- the damage differs, so the crown lands on carol instead of alice. An
        -- arm that ignored the relation would fire here too.
        Spec.it s "CR 725.2/109.5 a crown stolen by carol does not fire alice's trigger" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          boggartBrute <- S.printingOf s registry "Boggart Brute"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.bob (Setup.emptyGame S.threePlayers))
              (lich, g1) = S.addCreature custodiLich S.alice base
              (_, gs) = S.addCreature boggartBrute S.alice g1
              wraith = case filter (\oid -> S.soleFaceName oid gs == S.printingName bogWraith) (Game.zoneMembers Zone.Battlefield S.carol gs) of
                oid : _ -> oid
                [] -> S.noSource
              after = resolveAll (targetsPlayer S.bob) (combatDamageTo S.bob wraith gs)
          Spec.assertEqWith s "CR 725.2 carol took the crown" (GameState.monarch after) (Just S.carol)
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.carol) (S.eventsOf after)) "and the crowning event names carol"
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich is still there, and still silent"
          Spec.assertEqWith s "bob kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "the stack is empty" (GameState.stack after) []
        -- CR 725.4's third route into the crown: no effect and no inherent
        -- ability, just the monarch leaving the game. Three seats are mandatory
        -- twice over -- Departure.continuesAfterDeparture skips all of CR 800.4a
        -- at two (CR 800.1), and the edict's victim has to be somebody other
        -- than the departed monarch and the Lich's controller.
        --
        -- The bystanders helper is not used: its two creatures sit with bob, who
        -- is the one leaving here, so carol holds the pair instead (Goblin Piker
        -- 2/1, Bird Maiden 1/2) and CR 701.21a's choice stays a real prompt.
        Spec.it s "CR 725.4 a departure crowns alice, and that crowning fires her edict" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          let base = S.withMonarch S.bob (Setup.emptyGame S.threePlayers)
              (lich, g1) = S.addCreature custodiLich S.alice base
              (_, g2) = S.addCreature piker S.carol g1
              (_, gs) = S.addCreature birdMaiden S.carol g2
              -- CR 104.3a: bob concedes, so the crown is reassigned inside the
              -- departure rather than by anything that resolves afterwards.
              departed = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.bob)
              after = resolveAll (targetsPlayer S.carol) departed
          Spec.assertEqWith s "bob wore the crown going in" (GameState.monarch gs) (Just S.bob)
          Spec.assertEqWith s "alice is the active player, so CR 725.4's first sentence crowns her" (GameState.activePlayer gs) S.alice
          Spec.assertEqWith s "CR 725.4 alice is the monarch" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (S.onBattlefield lich after) "alice's Lich watched from the battlefield"
          -- Asserted BEFORE the event, so a run with the record deleted fails
          -- here rather than on the event and the payload is what is pinned.
          Spec.assertEqWith s "CR 701.21a the targeted carol lost exactly one of her two" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "and alice, untargeted, still has her Lich" (S.creaturesInPlay S.alice after) 1
          Spec.assertBool s (elem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and the reassignment recorded its crowning"
          Spec.assertEqWith s "CR 104.2a two survivors, so the game is still going" (GameState.result after) Nothing
          Spec.assertEqWith s "the stack is empty, so nothing is still pending" (GameState.stack after) []

-- CR 603.7: Ray of Command's THIRD sentence -- "When you lose control of the
-- creature, tap it." A delayed triggered ability whose event is a CONTROL CHANGE,
-- which is the observation point Engine.sampleControl exists to provide: control is
-- derived (CR 613.1b layer 2), so the CR 514.2 sweep that ends the spell's
-- until-end-of-turn control effect announces nothing, and the diff against
-- GameState.controlSample is what mints the GameEvent.ControlChanged the condition
-- matches. CR 514.3a is what then gives the trigger its round: a triggered ability
-- waiting during the cleanup step gets put on the stack and the active player gets
-- priority.
--
-- THREE SEATS, because the condition reads ONE of them. "You" is the ability's
-- controller (CR 603.7d, alice), the creature's owner and the player control
-- returns to is bob, and carol holds a creature alice steals with a card that has no
-- third sentence. On a two-player board "you", "the creature's owner" and "an
-- opponent" collapse, and a condition matching the wrong one of the three would
-- still pass.
--
-- ACT OF TREASON is the negative leg, and the two legs run on ONE board: the same
-- mana, the same seats, two identical tapped Goblin Pikers, the same cleanup step.
-- The single difference is which card did the stealing -- Act of Treason ({2}{R}
-- Sorcery, "Gain control of target creature until end of turn. Untap that creature.
-- It gains haste until end of turn.") prints the same three effects and NOT the tap
-- sentence, so carol's creature coming home untapped is what shows the tap is Ray of
-- Command's own ability rather than anything the cleanup machinery does to a
-- returning permanent.
--
-- Both victims start TAPPED and are untapped by the first sentence of whichever card
-- steals them, so the board makes a ROUND TRIP: tapped, untapped by the spell, tapped
-- again by the trigger. `Tapped` at the end therefore cannot be state left standing,
-- and the untapped reading in the middle is what rules that out.
rayOfCommandSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
rayOfCommandSpec s registry = Spec.describe s "RayOfCommand" $ do
  Spec.it s "CR 603.7 Ray of Command whole card: the borrowed creature is TAPPED when control reverts at cleanup, and Act of Treason's is not" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    rayOfCommand <- S.printingOf s registry "Ray of Command"
    actOfTreason <- S.printingOf s registry "Act of Treason"
    let addN n printing pid g = if n <= (0 :: Int) then g else addN (n - 1) printing pid (snd (S.addCreature printing pid g))
        lands = addN 3 mountain S.alice (addN 4 island S.alice S.threePlayerGame)
        (bobPiker, g1) = S.addCreature piker S.bob lands
        (carolPiker, g2) = S.addCreature piker S.carol g1
        (rayId, g3) = S.addHandCard rayOfCommand S.alice g2
        (actId, g4) = S.addHandCard actOfTreason S.alice g3
        -- Both victims start TAPPED, so the first sentence of each card (CR 701.26b)
        -- has something to do and `Tapped` at the end cannot be state left standing.
        staged = S.tapObject carolPiker (S.tapObject bobPiker g4)
        resolveOne victim spellId g =
          S.settleSba (S.runPure (aimAtVictim victim) (S.runPure (aimAtVictim victim) g (S.cast S.alice spellId)) Stack.resolveTop)
        stolen = resolveOne carolPiker actId (resolveOne bobPiker rayId staged)
        scheduled = stolen {GameState.remaining = Seq.fromList [Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]}
        afterMain = S.runPure S.identityAnswer scheduled Engine.runStep
        afterEnd = S.runPure S.identityAnswer afterMain Engine.runStep
        afterCleanup = S.runPure S.identityAnswer afterEnd Engine.runStep
        tapStateOf oid g = fmap Object.tapped (Game.lookupObject oid g)
    -- The theft really happened, and left both creatures untapped. Without these the
    -- tap assertion below could pass on a board where nothing was stolen at all.
    Spec.assertEqWith s "Ray of Command gave alice control of bob's Piker" (Projection.controllerOf bobPiker stolen) (Just S.alice)
    Spec.assertEqWith s "Act of Treason gave her carol's" (Projection.controllerOf carolPiker stolen) (Just S.alice)
    Spec.assertEqWith s "CR 701.26b and both were untapped by the first sentence of each" (fmap (\oid -> tapStateOf oid stolen) [bobPiker, carolPiker]) [Just TapState.Untapped, Just TapState.Untapped]
    -- CR 514.2 ran, so the control effects ended and control reverted.
    Spec.assertEqWith s "the cleanup step really ran" (GameState.phase afterEnd) (Phase.Ending EndingStep.Cleanup)
    Spec.assertEqWith s "CR 514.2 bob has his Piker back" (Projection.controllerOf bobPiker afterCleanup) (Just S.bob)
    Spec.assertEqWith s "and carol hers" (Projection.controllerOf carolPiker afterCleanup) (Just S.carol)
    -- The sentence under test, asserted FIRST of the three claims about the finished
    -- board: a mutation that stops the trigger firing must go red HERE rather than on
    -- the event record below, which the turn handoff would also have cleared.
    Spec.assertEqWith s "CR 603.7 Ray of Command's third sentence tapped it" (tapStateOf bobPiker afterCleanup) (Just TapState.Tapped)
    Spec.assertEqWith s "Act of Treason prints no such sentence, so carol's comes home untapped" (tapStateOf carolPiker afterCleanup) (Just TapState.Untapped)
    Spec.assertEqWith s "CR 603.7b the entry is spent, so nothing is still armed" (GameState.delayedTriggers afterCleanup) Seq.empty
    Spec.assertEqWith s "and the stack is empty" (GameState.stack afterCleanup) []
    -- CR 514.3a: the trigger got its round INSIDE this turn -- the rule's last sentence
    -- begins another cleanup step rather than passing the turn. That is also what keeps
    -- the event record below readable, since Engine.beginTurnOf clears the log at the
    -- handoff.
    Spec.assertEqWith s "CR 514.3a the turn has not handed off" (GameState.turnNumber afterCleanup) (GameState.turnNumber scheduled)
    -- The observation point fired at all.
    Spec.assertBool s (elem (GameEvent.ControlChanged bobPiker S.alice S.bob) (S.eventsOf afterCleanup)) "Engine.sampleControl minted CR 603.2's event for the reversion"
  where
    -- Narrows every target slot to one object, `aimedCast`'s filter without its cast
    -- pinning: the board holds two stealable creatures on purpose, so the engine's
    -- first offer is not the one either leg means. Filtering the OFFERED set rather
    -- than naming a Recipient keeps the answer in whatever shape the slot offered.
    aimAtVictim :: ObjectId.ObjectId -> Prompt.Prompt r -> r
    aimAtVictim oid p = case p of
      Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just oid) . Recipient.objectOf) legal) sets
      _ -> S.identityAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  logSpec s registry
  scanSpec s registry
  permanentEntersSpec s registry
  sacrificeSpec s registry
  mayhemDevilSpec s registry
  stateTriggerSpec s registry
  textChangedTriggerSpec s registry
  historySpec s registry
  delayedSpec s registry
  tokenSetSpec s registry
  tokenGroupReadSpec s registry
  singleTokenSlotReadSpec s registry
  towershellOnsetSpec s registry
  towershellSkipSpec s registry
  orderingSpec s registry
  secondPlacementPassSpec s registry
  monarchOrderingSpec s registry
  interveningSpec s registry
  poisonousSpec s registry
  ingestSpec s registry
  annihilatorSpec s registry
  battleCrySpec s registry
  prowessSpec s registry
  selfBlocksSpec s registry
  selfBlocksAtLeastSpec s registry
  selfBlocksOneOrMoreSpec s registry
  selfBlocksCreatureSpec s registry
  selfBecomesBlockedSpec s registry
  bushidoSpec s registry
  flankingSpec s registry
  exaltedSpec s registry
  mentorSpec s registry
  mentorsTriggerSpec s registry
  trainingSpec s registry
  decayedSpec s registry
  provokeSpec s registry
  trygonPredatorSpec s registry
  evolveSpec s registry
  krasisSpec s registry
  renownSpec s registry
  arborColossusSpec s registry
  vanishingSpec s registry
  modularSpec s registry
  tovolarSpec s registry
  aragornSpec s registry
  afflictSpec s registry
  meleeSpec s registry
  dethroneSpec s registry
  rampageSpec s registry
  cyclingTriggerSpec s registry
  graveyardTriggerSpec s registry
  gaeasBlessingSpec s registry
  graveyardEffectZoneTriggerSpec s registry
  commandZoneTriggerSpec s registry
  serraAvatarSpec s registry
  diesTriggerSpec s registry
  permanentDiesSpec s registry
  leavesBattlefieldSpec s registry
  becameSlotSpec s registry
  promiseOfTomorrowSpec s registry
  lookBackInterveningSpec s registry
  counterLookBackSpec s registry
  undyingSpec s registry
  afterlifeSpec s registry
  fabricateSpec s registry
  soulshiftSpec s registry
  strippedTriggerSpec s registry
  bystanderSpec s registry
  bystanderZoneSpec s registry
  aetherFlashSpec s registry
  kindredSpec s registry
  discardTriggerSpec s registry
  controllerAtTriggerSpec s registry
  counterTriggerSpec s registry
  lifeGainTriggerSpec s registry
  lifeGainAmountSpec s registry
  lifeLossTriggerSpec s registry
  mindcrankSpec s registry
  anafenzaAttackSpec s registry
  ezuriExperienceSpec s registry
  youngPyromancerSpec s registry
  desolationTwinSpec s registry
  presenceOfTheMasterSpec s registry
  kambalSpec s registry
  brinebornCutthroatSpec s registry
  handOfThePraetorsSpec s registry
  monarchTriggerSpec s registry
  rayOfCommandSpec s registry
