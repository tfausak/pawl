-- Covers M4.5 P4 in full. The turn-scoped event log (Pawl.Types.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff -- `logSpec`. The CR 603.2b step-beginning
-- event and the CR 603.6a widened scan (every battlefield permanent, not just
-- an enters event's newcomer) -- `scanSpec`. The `Sacrifice` opcode and its
-- reserved trigger-source slot, CR 701.21 -- `sacrificeSpec`. CR 603.6a's
-- OTHER written form, "whenever a [type] enters", with Soul Warden --
-- `permanentEntersSpec`. CR 603.8 state
-- triggers -- `stateTriggerSpec`, and CR 612.1's basic-land-type word swap
-- reaching one of those, with Magical Hack aimed at Barbarian Outcast --
-- `textChangedTriggerSpec`. CR 608.2i turn history (Khabál Ghoul's
-- "died this turn") -- `historySpec`. CR 603.7 delayed triggered abilities
-- -- `delayedSpec`. The CR 603.3b ordering prompt -- `orderingSpec`, and its
-- CR 725.2 sourceless case (the monarch's inherent triggers ordered WITH the
-- batch) -- `monarchOrderingSpec`. The CR 603.4 / 608.2a intervening "if" --
-- `interveningSpec`. Also Pawl.Engine.Keyword: CR
-- 702.70 poisonous, the keyword whose rule text IS a triggered ability, and the
-- reserved "that player" slot the scan stamps for it -- `poisonousSpec`. CR
-- 702.91 battle cry, the second such keyword, and with it the CR 603.3b
-- ordering payload's ability discriminator -- two DISTINCT abilities of one
-- source, ordered both ways with different boards, and two triggers of the SAME
-- ability staying indistinguishable, with Hero of Bladehold -- `battleCrySpec`.
-- CR
-- 113.6k's non-battlefield scan -- the graveyard, with Tome Scour milling
-- Narcomoeba -- `graveyardTriggerSpec`. CR 400.7e's OTHER reference inside a
-- look-back trigger, the card it became in the first zone it went to, with
-- Endless Cockroaches -- `becameSlotSpec`, which also pins
-- Event.eventBindingSlots (the per-condition slot set the card lint asks)
-- against the keys eventBindings actually stamps, over every event each
-- condition admits. CR 603.4's intervening "if"
-- read against a source that no longer exists (CR 608.2h), with Deathknell Berserker
-- -- `lookBackInterveningSpec`. CR 603.10's first sentence for a BYSTANDER -- a
-- permanent that was on the battlefield when some OTHER event in the same batch
-- happened and is gone by the CR 117.5 boundary -- with Lightning Skelemental
-- and Khabál Ghoul -- `bystanderSpec`. The same CR 400.7e slot read from the ENTRY
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
-- Clan Nel Toth, which is also the pool's producer for CR 122's experience
-- counters -- `permanentDiesSpec`.
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
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
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
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Expiry as Expiry.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
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
   in resolveAll (snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice oid)))

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
      case Foldable.toList (GameState.events after) of
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
      Spec.assertEqWith s "the end step's beginning is recorded exactly once" (Maybe.mapMaybe began (Foldable.toList (GameState.events after))) [(Phase.Ending EndingStep.EndStep, S.alice)]
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
      Spec.assertEqWith s "no trigger" (length (fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2))) 0
    Spec.it s "CR 603.6a a SelfEnters trigger still fires on its own entry" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          entered = ZoneChange.MkZoneChange ripId ripId Zone.Stack Zone.Battlefield
          gs1 = S.withEvents [GameEvent.Moved entered (Projection.project ripId gs0)] gs0
      case fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1) of
        [pt] -> do
          Spec.assertEqWith s "source is RiP" (PendingTrigger.source pt) (TriggerSource.OfObject ripId)
          Spec.assertEqWith s "controller is alice" (PendingTrigger.controller pt) S.alice
        other -> Spec.assertFailure s ("expected exactly one pending trigger, got " <> show (length other))
    Spec.it s "a graveyard-bound event yields no enters trigger" $ do
      restInPeace <- S.printingOf s registry "Rest in Peace"
      let (ripId, gs0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
          toGrave = ZoneChange.MkZoneChange ripId ripId Zone.Battlefield Zone.Graveyard
          gs1 = S.withEvents [GameEvent.Moved toGrave (Projection.project ripId gs0)] gs0
      Spec.assertEqWith s "no triggers" (length (fst (Event.gatherTriggers (Event.unscannedEvents gs1) gs1))) 0
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
          declared = GameEvent.AttackerDeclared bearer
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
      Spec.assertBool s (matches TriggerFrequency.FirstTimeEachTurn [GameEvent.AttackerDeclared (ObjectId.MkObjectId 2), declared]) "another creature's declaration does not spend this one's first time"
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
          triggers = fst (Event.gatherTriggers (Event.unscannedEvents gs2) gs2)
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
          triggers = fst (Event.gatherTriggers [event] gs1)
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
          cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
          cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
      Spec.assertEqWith s "the trigger names its source" (fmap selfOf (GameState.stack placed)) [Just (Recipient.ToObject ripId)]

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
        -- IMPORTANT-2 (review): the suppression check in Event.stateTriggers
        -- compares BOTH the source object's id and the ability (`Object.source
        -- obj == Source.OfTrigger srcId ab`). Every test above uses exactly one
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

-- Answers the Hack: it targets `oid` and swaps `from` for `to`. Everything else
-- falls through to the identity answer.
answerHackAt :: ObjectId.ObjectId -> Subtype.Subtype -> Subtype.Subtype -> Prompt.Prompt r -> r
answerHackAt oid from to p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
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
      hackIt g = S.runPure (answerHackAt outcastId Subtype.Swamp Subtype.Island) g (do Cast.castSpell S.bob hackId; Stack.resolveTop)
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
-- delayed ability -- "it" must survive the resolution that armed it.
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
              Cast.castSpell S.alice oid
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
              cast = resolveAll (snd (Engine.runGamePure S.identityAnswer inEndStep (Cast.castSpell S.alice oid)))
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
          let onlyMode = Mode.MkMode {Mode.effects = Seq.empty, Mode.targetSpecs = Map.empty, Mode.optionality = Optionality.Mandatory}
              ability =
                TriggeredAbility.MkTriggeredAbility
                  { TriggeredAbility.condition = TriggerCondition.SelfEnters,
                    TriggeredAbility.modal = Modal.MkModal {Modal.modes = Seq.singleton onlyMode, Modal.selection = ModeSelection.ChooseExactly 1},
                    TriggeredAbility.intervening = Nothing
                  }
              -- Stands in for a modal arming spell's own captured chosenModes --
              -- built with the SAME Binding.fromChoices Cast.castSpell uses, so
              -- the collision is the real production shape, not a fabricated one.
              captured = Binding.fromChoices Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 7))
              pending = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject (ObjectId.MkObjectId 0)) S.alice ability captured
              after = snd (Engine.runGamePure S.identityAnswer (Setup.emptyGame S.bothPlayers) (Engine.placeOne pending))
              placedModes = case GameState.stack after of
                placedId : _ -> case Game.lookupObject placedId after of
                  Just obj -> Binding.modesOf (Object.bindings obj)
                  Nothing -> Set.empty
                [] -> Set.empty
          Spec.assertEqWith s "the ability's own mode (0), not the captured spell's mode (7)" placedModes (Set.singleton (ModeIndex.MkModeIndex 0))
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
              cast = S.runPure S.identityAnswer ready (Cast.castSpell S.bob waveId)
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
              cast = S.runPure chooseUnmintedToken gs (Cast.castSpell S.alice waveId)
              armed = S.runPure chooseUnmintedToken cast Engine.priorityLoop
              after = resolveAll (settle (beginEndStep armed))
          case walls armed of
            [firstWall, secondWall] -> do
              Spec.assertEqWith s "only the second minted Wall is left" (walls after) [secondWall]
              Spec.assertBool s (Set.notMember firstWall (GameState.battlefield after)) "the first minted Wall was bound, and it is gone"
            other -> Spec.assertFailure s ("expected two Wall tokens, got " <> show (length other))

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
            cast = resolveAll (snd (Engine.runGamePure S.identityAnswer gs1 (Cast.castSpell S.alice waveId)))
         in (ghoul, beginEndStep cast)
      -- The source of the OTHER pending trigger: Tidal Wave's delayed ability,
      -- whose source is the resolved spell's id rather than any permanent.
      otherThan ghoul gs =
        let sources = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
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
   in Spec.describe s "TriggerOrdering" $ do
        Spec.it s "CR 603.3b two triggers under one controller ask for an order, exactly once" $ do
          tidalWave <- S.printingOf s registry "Tidal Wave"
          island <- S.printingOf s registry "Island"
          khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
          let (_, gs) = boardOf tidalWave khabalGhoul island
              (_, asked) = State.runState (Engine.runGame countingAnswer gs Engine.settleForPriority) 0
          Spec.assertEqWith s "asked once" asked 1
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
          Spec.assertEqWith s "reordered" (Engine.permute "abc" [2, 1, 0]) "cba"
        Spec.it s "permute rejects a short answer, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Engine.permute "abc" [1, 0]) "abc"
        Spec.it s "permute rejects a duplicate index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Engine.permute "abc" [0, 0, 1]) "abc"
        Spec.it s "permute rejects an out-of-range index, keeping the canonical order" $ do
          Spec.assertEqWith s "unchanged" (Engine.permute "abc" [0, 1, 5]) "abc"
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
      abilityEffects = concatMap (Modal.allEffects . TriggeredAbility.modal) (Face.triggeredAbilities (S.faceOf sarcomancy))
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
          let ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 7) (Recipient.ToPlayer S.bob) 2 False False 0 Nothing DamageKind.Combat)
              bindings = Event.eventBindings TriggerCondition.SelfDealsCombatDamageToPlayer ev
          Spec.assertEqWith s "bob is bound under thatPlayer" (Binding.targetsOf bindings) (Map.singleton Binding.triggerPlayer (Recipient.ToPlayer S.bob))
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
                  cast = S.runPure S.aggressiveAnswer inHand {GameState.priority = Just S.alice} (Cast.castSpell S.alice spellId)
                  resolved = S.runPure S.aggressiveAnswer cast Stack.resolveTop
                  after = S.runCombat S.aggressiveAnswer resolved
              Spec.assertBool s (Projection.hasKeyword (Keyword.Type.Poisonous 3) attacker resolved) "the Aura granted poisonous 3"
              Spec.assertEqWith s "bob has three poison" (S.playerCounterOf PlayerCounterKind.Poison S.bob after) 3
              Spec.assertEqWith s "and took the Piker's two" (S.lifeOf S.bob after) (Just 18)

-- CR 702.91a: "Battle cry is a triggered ability. 'Battle cry' means 'Whenever
-- this creature attacks, each other attacking creature gets +1/+0 until end of
-- turn.'" The second keyword in this pool whose rule text IS a triggered
-- ability, after CR 702.70a's poisonous, so it is minted by Pawl.Engine.Keyword
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
        -- answer. CR 603.6a fires Soul Warden's ability once per entering
        -- creature, and Hero of Bladehold's token-maker puts two Soldiers onto
        -- the battlefield at once, so the second ordering choice of the same
        -- combat is a pair of entries differing only in which token each
        -- remembers -- a difference the entry deliberately does not carry.
        Spec.it s "CR 603.6a two triggers of the SAME ability stay indistinguishable" $ do
          hero <- S.printingOf s registry "Hero of Bladehold"
          soulWarden <- S.printingOf s registry "Soul Warden"
          case S.combatBoardOf [hero, soulWarden] [] of
            (gs, [_, wardenId], _) -> case snd (State.runState (Engine.runGame recordEntries gs Engine.runStep) []) of
              [[a, b], [w1, w2]] -> do
                Spec.assertBool s (a /= b) "the Hero's two abilities are still distinguishable"
                Spec.assertEqWith s "the second choice is the Warden's" (TriggerEntry.source w1) (TriggerSource.OfObject wardenId)
                Spec.assertEqWith s "and its two triggers are the same ability from the same source" w1 w2
              other -> Spec.assertFailure s ("expected two ordering payloads of two entries each, got " <> show (fmap length other))
            _ -> Spec.assertFailure s "fixture should give alice a Hero and a Soul Warden"
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
          cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice reunionId)
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
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe (Set.lookupMin . Set.filter ((== Just victim) . Recipient.objectOf)) sets
  Prompt.ChooseAction _ _ actions -> case filter (== A.Cast spell) actions of
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
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject victimId)) sets
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
              cast = S.runPure answer gs (Cast.castSpell S.bob cancelId)
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
              cast = S.runPure answer gs (Cast.castSpell S.bob cancelId)
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
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.alice)) sets
                Prompt.ChooseOptional {} -> OptionalDecision.Exercises
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (Cast.castSpell S.bob boltId)
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
              cast = S.runPure answer gs (Cast.castSpell S.alice cancelId)
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
          case Face.activatedAbilities (S.faceOf sorcerer) of
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
                  spellRun = S.runPure (answerWith victimId) gs (Cast.castSpell S.bob cancelId)
                  spellCountered = S.runPure (answerWith victimId) spellRun Stack.resolveTop
                  spellPlaced = S.runPure (answerWith victimId) spellCountered Engine.settleForPriority
                  spellAfter = S.runPure (answerWith victimId) spellPlaced Stack.resolveTop
                  -- The ABILITY run: alice activates her Sorcerer at herself,
                  -- and bob's Stifle counters the ability. Aimed at alice so the
                  -- effect that must NOT occur is her own life total.
                  atAlice :: Prompt.Prompt r -> r
                  atAlice p = case p of
                    Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToPlayer S.alice)) sets
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
                  abilityRun = S.runPure atAbility activated (Cast.castSpell S.bob stifleId)
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
              cast = S.runPure answer gs (Cast.castSpell S.bob cancelId)
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
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
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
              cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
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
        -- the object. Same fallback eventTriggers' own `bystanders` takes for
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
        let cast = S.runPure answer gs (Cast.castSpell S.alice spellId)
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
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents buried) buried))) []
          Spec.assertBool s (Set.member narcomoebaName (namesIn Zone.Graveyard S.alice buried)) "it is in the graveyard"
        -- "from your library" doing real work, half two: dying is a move to
        -- the same graveyard from the battlefield, and is not this trigger.
        Spec.it s "CR 113.6k Narcomoeba dying from the BATTLEFIELD does not trigger" $ do
          narcomoeba <- S.printingOf s registry "Narcomoeba"
          let (creature, gs) = S.addCreature narcomoeba S.alice (Setup.emptyGame S.bothPlayers)
              died = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents died) died))) []
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
          Spec.assertEqWith s "and a creature entering fires nothing" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents entered) entered))) []

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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
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
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents exiled) exiled))) []
          Spec.assertBool s (Set.member travelerName (namesIn Zone.Exile S.alice exiled)) "it is in exile"
        -- The other half of "from the battlefield": the same card discarded
        -- reaches the same graveyard and has not died (CR 700.4).
        Spec.it s "CR 700.4 a Traveler discarded from the HAND does not trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          let (traveler, gs) = S.addHandCard doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              discarded = S.runPure S.identityAnswer gs (Event.changeZone traveler Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents discarded) discarded))) []
        -- Self-scoped: SOME OTHER creature dying is not this Traveler's
        -- death, even though the Traveler is right there to see it.
        Spec.it s "CR 603.6c another creature dying does not fire the Traveler's trigger" $ do
          doomedTraveler <- S.printingOf s registry "Doomed Traveler"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withTraveler) = S.addCreature doomedTraveler S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs) = S.addCreature piker S.alice withTraveler
              died = S.runPure S.identityAnswer gs (Event.changeZone pikerId Zone.Graveyard)
          Spec.assertEqWith s "nothing triggered" (fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents died) died))) []
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
          Spec.assertEqWith s "so the trigger is hers, not its owner's" (fmap PendingTrigger.controller (fst (Event.gatherTriggers (Event.unscannedEvents died) died))) [S.alice]

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
-- No case here kills the bearer and some other creature at once. A Meren that
-- departs EARLIER in a batch than the death it should be watching is not
-- offered to the scan as a candidate (#615).
permanentDiesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
permanentDiesSpec s registry =
  let anotherCreatureYouControl =
        Filter.Type.And
          [ Filter.Type.HasCardType CardType.Creature,
            Filter.Type.ControlledBy PlayerRelation.You,
            Filter.Type.Not Filter.Type.IsSource
          ]
      sourcesOf gs = fmap PendingTrigger.source (fst (Event.gatherTriggers (Event.unscannedEvents gs) gs))
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
                Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToCreature pikerId)) sets
                _ -> S.identityAnswer p
              cast = S.runPure answer gs (Cast.castSpell S.alice spellId)
              damaged = S.runPure answer cast Stack.resolveTop
              settled = S.runPure answer damaged Engine.settleForPriority
              after = S.runPure answer settled Stack.resolveTop
          Spec.assertEqWith s "alice starts with no experience" (experienceOf S.alice gs) 0
          Spec.assertEqWith s "the Piker is gone" (Game.lookupObject pikerId settled) Nothing
          Spec.assertEqWith s "the trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and alice has exactly one experience counter" (experienceOf S.alice after) 1
          Spec.assertEqWith s "bob has none" (experienceOf S.bob after) 0
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
            (fmap TriggeredAbility.condition (Face.triggeredAbilities (S.faceOf meren)))
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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
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
        concatMap (Map.toList . Binding.targetsOf . maybe Map.empty Object.bindings . flip Game.lookupObject gs) (GameState.stack gs)
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

-- The events a trigger condition GENUINELY fires on (Event.matchesTrigger's own
-- arms are the spec), so eventBindings is exercised through its matching arm
-- rather than through its `_ -> Map.empty` fallthrough. A pair that did not
-- match would pin nothing: both sides would read empty for every condition.
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
          (DamageEvent.MkDamageEvent departed (Recipient.ToPlayer S.bob) 2 False False 0 Nothing DamageKind.Combat)
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
        TriggerCondition.CreatureDealtCombatDamageToMonarch -> one combatDamage
        TriggerCondition.SelfCycled -> one (GameEvent.Discarded S.alice departed DiscardCause.ToPayCyclingCost)
        TriggerCondition.PlayerDiscards _ -> one (GameEvent.Discarded S.alice departed DiscardCause.Ordinary)
        TriggerCondition.SelfAttacks _ -> one (GameEvent.AttackerDeclared departed)
        TriggerCondition.SelfPutIntoGraveyardFromLibrary -> one (moved Zone.Library Zone.Graveyard)
        TriggerCondition.SelfDies -> one (moved Zone.Battlefield Zone.Graveyard)
        TriggerCondition.PermanentDies _ -> one (moved Zone.Battlefield Zone.Graveyard)
        -- CR 603.6c admits every destination, and CR 400.2 splits them into
        -- public and hidden, so both sides of CR 400.7e's proviso have to be
        -- here for the floor to be the honest answer.
        TriggerCondition.SelfLeavesTheBattlefield ->
          moved Zone.Battlefield Zone.Graveyard NonEmpty.:| [moved Zone.Battlefield Zone.Hand]
        TriggerCondition.SpellOrAbilityCounters _ ->
          one (GameEvent.SpellCountered (Countering.MkCountering departed arrived S.alice))

-- Every TriggerCondition, one inhabitant each. The payloads are arbitrary:
-- eventBindings and eventBindingSlots both ignore them, which is itself part of
-- what the pin asserts.
everyTriggerCondition :: [TriggerCondition.TriggerCondition]
everyTriggerCondition =
  [ TriggerCondition.SelfEnters,
    TriggerCondition.PermanentEnters Filter.Type.IsSource,
    TriggerCondition.PermanentDies Filter.Type.IsSource,
    TriggerCondition.StepBegins (Phase.Beginning BeginningStep.Upkeep) TurnScope.EachTurn,
    TriggerCondition.StateIs (Condition.Type.MkCondition (Quantity.Type.Literal 0) Comparison.Exactly (Quantity.Type.Literal 0)),
    TriggerCondition.SelfDealsCombatDamageToPlayer,
    TriggerCondition.CreatureDealtCombatDamageToMonarch,
    TriggerCondition.SelfCycled,
    TriggerCondition.PlayerDiscards PlayerRelation.Opponent,
    TriggerCondition.SelfAttacks TriggerFrequency.EveryTime,
    TriggerCondition.SelfPutIntoGraveyardFromLibrary,
    TriggerCondition.SelfDies,
    TriggerCondition.SelfLeavesTheBattlefield,
    TriggerCondition.SpellOrAbilityCounters PlayerRelation.You
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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
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
              slots = concatMap (Map.toList . Binding.targetsOf . bindingsOn) (GameState.stack settled)
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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
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
          triggers = fst (Event.gatherTriggers (Event.unscannedEvents dead) dead)
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
          triggers = fst (Event.gatherTriggers (Event.unscannedEvents began) began)
      Spec.assertEqWith s "the Ghoul is gone" (Game.lookupObject ghoul began) Nothing
      Spec.assertEqWith s "and nothing triggered" (fmap PendingTrigger.source triggers) []

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
        let cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      namesIn zone pid gs =
        fmap Face.name (Maybe.mapMaybe (\oid -> Game.faceOf oid gs) (Game.zoneMembers zone pid gs))
      damageEventsIn gs = Maybe.mapMaybe Event.damageOf (Foldable.toList (GameState.events gs))
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
              cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice spellId)
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
          (Target.legalSets (Just S.alice) S.noSource (Map.singleton slot (TargetSpec.MkTargetSpec pool (Just (Filter.Type.HasSubtype Subtype.Faerie)))) gs)
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
        cast = snd (Engine.runGamePure S.identityAnswer atMain (Cast.castSpell S.alice spell))
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
        cast = snd (Engine.runGamePure S.identityAnswer atMain (Cast.castSpell S.alice spell))
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
        [ph | GameEvent.StepBegan ph@(Phase.Combat _) who <- Foldable.toList (GameState.events gs), who == pid]
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

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  logSpec s registry
  scanSpec s registry
  permanentEntersSpec s registry
  sacrificeSpec s registry
  stateTriggerSpec s registry
  textChangedTriggerSpec s registry
  historySpec s registry
  delayedSpec s registry
  towershellOnsetSpec s registry
  towershellSkipSpec s registry
  orderingSpec s registry
  monarchOrderingSpec s registry
  interveningSpec s registry
  poisonousSpec s registry
  battleCrySpec s registry
  cyclingTriggerSpec s registry
  graveyardTriggerSpec s registry
  diesTriggerSpec s registry
  permanentDiesSpec s registry
  leavesBattlefieldSpec s registry
  becameSlotSpec s registry
  lookBackInterveningSpec s registry
  strippedTriggerSpec s registry
  bystanderSpec s registry
  aetherFlashSpec s registry
  kindredSpec s registry
  discardTriggerSpec s registry
  controllerAtTriggerSpec s registry
  counterTriggerSpec s registry
