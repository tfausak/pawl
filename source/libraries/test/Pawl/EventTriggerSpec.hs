{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over the events that are not zone changes: draws,
-- discards, counters placed and removed, life gained and lost, damage, casts,
-- and attack declarations. The machinery is Pawl.TriggerSpec.
module Pawl.EventTriggerSpec where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Event.Match as Match
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.Zone as Zone

-- One board for every case below, differing in exactly one thing: WHICH seat
-- holds the Barkhide Mauler and cycles it. alice controls the Prickly Marmoset
-- throughout, so CR 603.3a fixes its "you" as alice on all three boards.
--
-- Three seats, not two. The condition's axis is CR 109.5's "you" against
-- everyone else, and a board with one other player cannot show that "everyone
-- else" is more than the one seat opposite.
--
-- Two Forests each, so the {2} is payable whoever cycles, and a library card
-- each, so CR 104.3c cannot deck the seat that draws before the assertion runs.
marmosetBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  PlayerId.PlayerId ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
marmosetBoard marmoset mauler forest piker cycler =
  let seats = [S.alice, S.bob, S.carol]
      lands = List.foldl' (\g pid -> S.landsFor forest pid 2 g) (Setup.emptyGame S.threePlayers) seats
      libraries = List.foldl' (\g pid -> snd (S.addLibraryCard piker pid g)) lands seats
      (marmosetId, withMarmoset) = S.addPermanent marmoset S.alice libraries
      (maulerId, withMauler) = S.addHandCard mauler cycler withMarmoset
   in ( marmosetId,
        maulerId,
        withMauler
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just cycler
          }
      )

-- CR 702.29a: "'Cycling [cost]' means '[Cost], Discard this card: Draw a
-- card.'", so a cycle IS a discard, and rule 702.29c names that discard when it
-- defines what cycling a card is. Prickly Marmoset, {2}{R} 2/3 Creature --
-- Monkey, is the pool's first card to watch a PLAYER do it rather than to watch
-- itself be cycled: "Whenever you cycle a card, this creature gets +2/+0 until
-- end of turn." First strike is the rest of its text and is inert on every board
-- here.
--
-- Rule 702.29c governs only its own self-scoped phrase; what fixes this
-- watcher-scoped one's "you" is CR 603.3a, the ability's controller.
--
-- Barkhide Mauler is the cycled card throughout -- its whole text is "Cycling
-- {2}", so nothing on it can contribute a trigger and every count below is the
-- Marmoset's alone. 2/3 pumped by +2/+0 is 4/3, so no reading of the rule lands
-- on the same pair of numbers as another.
cyclesTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cyclesTriggerSpec s registry =
  Spec.describe s "CyclesTrigger" $ do
    -- The player axis, which is what makes this condition PlayerCycles rather
    -- than a nullary one: the same board and the same act, one cycling seat
    -- apart. An arm ignoring the discarder would pump alice's Marmoset on all
    -- three.
    Spec.it s "CR 603.3a 'you' is the Marmoset's controller: only alice's cycling pumps it" $ do
      forest <- S.printingOf s registry "Forest"
      marmoset <- S.printingOf s registry "Prickly Marmoset"
      mauler <- S.printingOf s registry "Barkhide Mauler"
      piker <- S.printingOf s registry "Goblin Piker"
      let run cycler =
            let (marmosetId, maulerId, gs) = marmosetBoard marmoset mauler forest piker cycler
             in case Activatable.abilitiesFor maulerId gs of
                  [ability] ->
                    let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility cycler maulerId ability)
                        placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
                        after = S.runPure S.identityAnswer placed Stack.resolveTop
                     in Just
                          ( length (Game.zoneMembers Zone.Graveyard cycler cycled),
                            length (GameState.stack placed),
                            S.powerToughnessOf marmosetId after
                          )
                  _ -> Nothing
      Spec.assertEqWith
        s
        "every seat's cycle reaches its own graveyard, but only alice's adds a trigger and pumps the Marmoset"
        (fmap run [S.alice, S.bob, S.carol])
        [ Just (1, 2, Just (4, 3)),
          Just (1, 1, Just (2, 3)),
          Just (1, 1, Just (2, 3))
        ]
    -- The neighbouring cause, and the reason this is not TriggerCondition.PlayerDiscards:
    -- an ORDINARY discard of the same card by the same player, through the same
    -- CR 400.7 funnel into the same graveyard, is not cycling and fires nothing.
    Spec.it s "CR 702.29c an ordinary discard by the same player is not cycling" $ do
      forest <- S.printingOf s registry "Forest"
      marmoset <- S.printingOf s registry "Prickly Marmoset"
      mauler <- S.printingOf s registry "Barkhide Mauler"
      piker <- S.printingOf s registry "Goblin Piker"
      let (marmosetId, _, gs) = marmosetBoard marmoset mauler forest piker S.alice
          -- The Mauler is the only card in alice's hand, so CR 701.9b has
          -- nothing to ask and the same card leaves by the other door.
          discarded = S.runPure S.identityAnswer gs (Cost.payComponent PaymentMoment.OutsideResolution Map.empty S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
          placed = S.runPure S.identityAnswer discarded Engine.settleForPriority
      Spec.assertEqWith s "the Mauler really did reach alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice discarded)) 1
      Spec.assertEqWith s "nothing was put on the stack" (GameState.stack placed) []
      Spec.assertEqWith s "and the Marmoset is still a 2/3" (S.powerToughnessOf marmosetId placed) (Just (2, 3))

-- The Food token Bartered Cow makes, by name, which is how the cases below read
-- the trigger's whole payload off the board.
foodTokenName :: CardName.CardName
foodTokenName = CardName.MkCardName (Text.pack "Food Token")

-- CR 601.2f's discard-as-a-cost, the door every non-cycling discard in the pool
-- goes through, asked for one card with no criterion.
discardOne :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
discardOne answer gs = S.runPure answer gs (Cost.payComponent PaymentMoment.OutsideResolution Map.empty S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))

-- Which of alice's cards CR 701.9b's choice discards, PINNED -- and filtered out
-- of the set the prompt offered rather than built, so a mutation cannot be
-- repaired by an answerer that goes looking for a legal pick.
discardPick :: ObjectId.ObjectId -> Prompt.Prompt r -> r
discardPick wanted p = case p of
  Prompt.ChooseDiscard _ _ held _ -> filter (== wanted) held
  _ -> S.identityAnswer p

-- CR 701.9a: "To discard a card, move it from its owner's hand to that player's
-- graveyard." Bartered Cow, {3}{W} 3/3 Creature -- Ox, is the pool's first card
-- to watch that happen to ITSELF: "When this creature dies and when you discard
-- this card, create a Food token."
--
-- One ability with TWO trigger conditions, which is CR 113.6k's second sentence
-- in as many words -- the dies half functions from the battlefield, the discard
-- half from the graveyard rule 701.9a has just moved the card to -- and
-- TriggerCondition.AnyOf in the card file. The payload is one Food token and
-- nothing else, no target and no "may", so the only new thing any case below can
-- be passing on is TriggerCondition.SelfDiscarded.
--
-- alice owns, holds and discards the Cow throughout, and that is not a two-seat
-- collapse: CR 701.9a discards a card from its OWNER's hand and CR 113.8 makes
-- that owner the controller of its ability in the graveyard, so no board can
-- separate the two seats.
selfDiscardTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
selfDiscardTriggerSpec s registry =
  let settle gs = S.runPure S.identityAnswer gs Engine.priorityLoop
      priorityTo gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
   in Spec.describe s "SelfDiscardTrigger" $ do
        -- The whole card, discard half: one card in hand, discarded to pay a
        -- cost, and the Food is on the battlefield once the trigger resolves.
        Spec.it s "CR 701.9a whole card: discarding the Cow creates a Food token" $ do
          cow <- S.printingOf s registry "Bartered Cow"
          let (gs, _) = S.handOne cow (Setup.emptyGame S.bothPlayers)
              discarded = discardOne S.identityAnswer gs
              placed = S.runPure S.identityAnswer discarded Engine.settleForPriority
          Spec.assertEqWith s "the Cow reached alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice discarded)) 1
          Spec.assertEqWith s "one trigger on the stack, and only one" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "and alice has one Food token afterwards" (S.countOnBattlefieldByName foodTokenName S.alice (settle discarded)) 1
        -- The discriminating pair: one board, two cards in alice's hand, and only
        -- which one CR 701.9b discards differs. What it pins is that the Food
        -- follows the CARD -- an implementation firing on any discard by the
        -- ability's controller would make one both times. Its negative half alone
        -- would be weak, the Cow still being in a hand no scan reads for this
        -- condition; the graveyard case below is the one that pins the bearer
        -- check itself.
        Spec.it s "CR 701.9a it is the DISCARDED card's own trigger, not its controller's" $ do
          cow <- S.printingOf s registry "Bartered Cow"
          piker <- S.printingOf s registry "Goblin Piker"
          let (cowId, base) = S.addHandCard cow S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, gs0) = S.addHandCard piker S.alice base
              gs = priorityTo gs0
              run wanted = settle (discardOne (discardPick wanted) gs)
          Spec.assertEqWith s "discarding the Cow makes a Food" (S.countOnBattlefieldByName foodTokenName S.alice (run cowId)) 1
          Spec.assertEqWith s "discarding the Piker instead makes none" (S.countOnBattlefieldByName foodTokenName S.alice (run pikerId)) 0
          Spec.assertEqWith s "though exactly one card was discarded either way" (fmap (length . Game.zoneMembers Zone.Graveyard S.alice . run) [cowId, pikerId]) [1, 1]
        -- The same point from the graveyard, which is the board the candidate
        -- scan cannot dismiss: the Cow is ALREADY in alice's graveyard, so
        -- eventTriggers' CR 113.6k source genuinely offers its ability, and
        -- another card's discard still has to leave it silent.
        Spec.it s "CR 113.6k a Cow already in the graveyard ignores another card's discard" $ do
          cow <- S.printingOf s registry "Bartered Cow"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, withCow) = S.addGraveyardCard cow S.alice (Setup.emptyGame S.bothPlayers)
              (gs, _) = S.handOne piker withCow
              after = settle (discardOne S.identityAnswer gs)
          Spec.assertEqWith s "both cards are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
          Spec.assertEqWith s "and no Food was created" (S.countOnBattlefieldByName foodTokenName S.alice after) 0
        -- CR 702.29a: cycling IS discarding, so the cause the event carries is
        -- one this condition must not read -- where its sibling
        -- TriggerCondition.SelfCycled reads nothing else. No printing carries both this
        -- condition and cycling (a Scryfall sweep for "you discard this card"
        -- returns this card, Edgar's Awakening and Titanbones, none of them a
        -- cycler), so the two causes are driven through Event.discard, the one
        -- funnel every discard in the engine shares. The same board, one
        -- DiscardCause apart.
        Spec.it s "CR 702.29a a cycling discard fires it too, and CR 702.29d once" $ do
          cow <- S.printingOf s registry "Bartered Cow"
          let (gs, cowId) = S.handOne cow (Setup.emptyGame S.bothPlayers)
              run cause = S.runPure S.identityAnswer gs (Event.discard cause S.alice cowId)
              stackAfter g = length (GameState.stack (S.runPure S.identityAnswer g Engine.settleForPriority))
          Spec.assertEqWith s "an ordinary discard makes one Food" (S.countOnBattlefieldByName foodTokenName S.alice (settle (run DiscardCause.Ordinary))) 1
          Spec.assertEqWith s "a cycling discard makes one too" (S.countOnBattlefieldByName foodTokenName S.alice (settle (run DiscardCause.ToPayCyclingCost))) 1
          Spec.assertEqWith s "and the cycle placed ONE trigger, not two" (stackAfter (run DiscardCause.ToPayCyclingCost)) 1
        -- The dies half, which shares the ability with the discard half: it still
        -- fires, and the graveyard card the Cow becomes does not fire a second
        -- time on the way. CR 700.4's "dies" is the battlefield-to-graveyard
        -- move, so this is the AnyOf's other branch and nothing else.
        Spec.it s "CR 700.4 the dies half fires once, and the discard half not at all" $ do
          cow <- S.printingOf s registry "Bartered Cow"
          let (cowId, base) = S.addPermanent cow S.alice (Setup.emptyGame S.bothPlayers)
              gs = priorityTo base
              killed = S.settleSba (S.markDamage cowId 3 gs)
              after = settle killed
          Spec.assertBool s (not (S.onBattlefield cowId after)) "the Cow took lethal damage and died"
          Spec.assertEqWith s "exactly one Food token" (S.countOnBattlefieldByName foodTokenName S.alice after) 1

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
  let (megrimId, g1) = S.addPermanent megrim S.bob (Setup.emptyGame S.bothPlayers)
      (landId, g2) = S.addPermanent mountain S.alice g1
      g3 = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) g2 [1 .. (4 :: Int)]
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
-- makes it the producer -- Act of Treason and Ray of Command can only name a
-- creature, and the only card in the pool that triggers on a discard is an
-- enchantment. Word of Seizing names a permanent too and would serve here as
-- well; it is not a second producer this board needs.
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
controllerAtTriggerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
controllerAtTriggerSpec s registry =
  Spec.describe s "ControllerAtTrigger" $ do
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
-- case below is the resolution side of that distinction; the countering, the
-- CR 113.6g, CR 113.9 and CR 601.2f cases live in data/scenarios/event-trigger.
--
-- bob controls the Baral throughout, so CR 109.5 fixes its "you" as bob (CR
-- 603.3a).
--
-- Baral's reflexive "if you do" is one Optional mode over both instructions
-- (#487), so `Exercises` below draws AND discards.
counterTriggerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
counterTriggerSpec s registry =
  Spec.describe s "CounterTrigger" $ do
    -- The negative that keeps
    -- cr-701-6a-whole-cards-bob-s-cancel-counters-alice-s-spell.json from
    -- passing vacuously. CR
    -- 608.2n puts a RESOLVED instant into its owner's graveyard -- the same
    -- zone change rule 701.6a's countering makes -- so an implementation
    -- that matched the zone pair rather than the recorded countering would
    -- fire here too.
    Spec.it s "CR 608.2n bob's own Bolt resolving into that same graveyard fires nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      baral <- S.printingOf s registry "Baral, Chief of Compliance"
      bolt <- S.printingOf s registry "Lightning Bolt"
      let (_, withBaral) = S.addPermanent baral S.bob (Setup.emptyGame S.bothPlayers)
          withLand = snd (S.addPermanent mountain S.bob withBaral)
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

-- CR 603.2's binding half of a per-permanent counter trigger: the ability names
-- the permanent the counters went on, and that permanent is neither the bearer
-- nor, in general, anything the bearer's controller controls.
--
--   * Auntie Ool, Cursewretch {1}{B}{R}{G} 4/4 Legendary Creature -- Goblin
--     Warlock (data/cards/auntie-ool-cursewretch.json): "Ward--Blight 2.
--     Whenever one or more -1/-1 counters are put on a creature, draw a card if
--     you control that creature. If you don't control it, its controller loses 1
--     life." Transcribed whole; the ward goes unexercised here, no spell in this
--     fixture targeting her.
--
--   * Soul Snuffers {2}{B}{B} 3/3 Creature -- Elemental Shaman
--     (data/cards/soul-snuffers.json): "When this creature enters, put a -1/-1
--     counter on each creature." One written instruction, one settled placement
--     per creature, so one trigger per creature -- which is what gives the case
--     five subjects across three seats out of one resolution.
--
--   * Wall of Stone {1}{R}{R} 0/8, one per seat in the first case: the subject
--     that survives its counter by a mile, so CR 704.5f takes nothing off the
--     board before the bindings are read.
--
--   * Goblin Piker {1}{R} 2/1, the second case's subject: the other side of that
--     same rule, dead at the CR 117.5 scan that places the trigger.
--
-- (Every name, cost, type line, P/T and oracle text checked against Scryfall.)
--
-- THREE SEATS, because the card's two branches collapse onto one at two: with a
-- single opponent, "its controller" and "the seat that is not you" name the same
-- player, so a life loss aimed at the trigger's own controller's opponent would
-- be indistinguishable from one aimed at the subject's controller. carol's Wall
-- is what separates them.
--
-- WHAT A WRONG BINDING WOULD SHOW. Binding nothing (the state before this unit)
-- leaves the count at zero for every subject, so alice draws nothing and the
-- life loss reaches nobody; binding the bearer or its controller makes all five
-- subjects "yours", so alice draws five and no seat loses life.
auntieOolSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auntieOolSpec s registry =
  let -- Settle and resolve until the stack is empty: the Snuffers' enter trigger
      -- places the counters, and the five Auntie Ool triggers only reach the
      -- stack at the CR 117.5 scan after it.
      resolveEverything gs =
        let settled = S.runPure S.identityAnswer gs Engine.settleForPriority
         in if null (GameState.stack settled)
              then settled
              else resolveEverything (S.runPure S.identityAnswer settled Stack.resolveTop)
   in Spec.describe s "Auntie Ool, Cursewretch" $ do
        Spec.it s "CR 603.2 the counter trigger names the subject's controller, not the bearer's" $ do
          ool <- S.printingOf s registry "Auntie Ool, Cursewretch"
          snuffers <- S.printingOf s registry "Soul Snuffers"
          wall <- S.printingOf s registry "Wall of Stone"
          swamp <- S.printingOf s registry "Swamp"
          let (oolId, g1) = S.addPermanent ool S.alice S.threePlayerGame
              (aliceWall, g2) = S.addPermanent wall S.alice g1
              (bobWall, g3) = S.addPermanent wall S.bob g2
              (carolWall, g4) = S.addPermanent wall S.carol g3
              -- Five cards, where three are drawn: CR 104.3c decks nobody, and a
              -- library that ran out would end the case before its assertions.
              stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 .. (5 :: Int)]
              (snuffersId, entered) = S.entersWithTrigger snuffers S.alice stocked
              after = resolveEverything entered
          Spec.assertEqWith s "bob, whose Wall took a counter he controls, lost 1 life" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "carol likewise, the seat neither the bearer's nor bob's" (S.lifeOf S.carol after) (Just 19)
          Spec.assertEqWith s "alice, who controls three of the five subjects, lost none" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and drew one card for each subject she controlled" (S.handSize S.alice after) 3
          -- The preconditions the four readings above rest on: five creatures each
          -- took exactly one -1/-1 counter, and alice's hand was empty to begin
          -- with, so the three cards are draws rather than a stocked fixture.
          Spec.assertEqWith s "alice's hand was empty before" (S.handSize S.alice entered) 0
          Spec.assertEqWith
            s
            "every creature took exactly one -1/-1 counter"
            (fmap (\oid -> S.counterOf CounterKind.MinusOneMinusOne oid after) [oolId, aliceWall, bobWall, carolWall, snuffersId])
            [1, 1, 1, 1, 1]
          Spec.assertEqWith s "and the stack is empty" (length (GameState.stack after)) 0
        -- The same reading where the subject is GONE by the time the ability
        -- resolves: Goblin Piker is a 2/1, so its own -1/-1 counter takes it to
        -- 1/0 and CR 704.5f puts it in bob's graveyard at the very CR 117.5 scan
        -- that places the trigger. `became` names a dead id there, and the card
        -- still has to find bob -- which is CR 608.2h's last known information,
        -- reached through Pawl.Engine.Resolve's PlayerRef.ControllerOfBound.
        --
        -- The case above cannot show this: every subject there survives, so a
        -- reader that answered nothing for a dead id would pass it.
        Spec.it s "CR 608.2h the subject that died to its own counter still names its controller" $ do
          ool <- S.printingOf s registry "Auntie Ool, Cursewretch"
          snuffers <- S.printingOf s registry "Soul Snuffers"
          piker <- S.printingOf s registry "Goblin Piker"
          swamp <- S.printingOf s registry "Swamp"
          let (_, g1) = S.addPermanent ool S.alice S.threePlayerGame
              (pikerId, g2) = S.addPermanent piker S.bob g1
              stocked = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g2 [1 .. (5 :: Int)]
              (_, entered) = S.entersWithTrigger snuffers S.alice stocked
              after = resolveEverything entered
          Spec.assertEqWith s "bob lost the life for a Piker that no longer exists" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "carol, who controlled no subject, lost none" (S.lifeOf S.carol after) (Just 20)
          Spec.assertEqWith s "alice drew for the two subjects that were hers" (S.handSize S.alice after) 2
          -- The precondition the reading rests on: the Piker really is gone.
          Spec.assertEqWith s "the Piker left the battlefield" (Game.lookupObject pikerId after) Nothing

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
youngPyromancerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
youngPyromancerSpec s registry =
  let elemental = CardName.MkCardName (Text.pack "Elemental Token")
      elementalsOf = S.countOnBattlefieldByName elemental
      -- alice has Young Pyromancer and four Mountains, bob four Mountains, carol
      -- nothing at all. Four each is Boil's {3}{R}, and covers Goblin Piker's
      -- {2}{R} with one to spare.
      board mountain pyromancer =
        let addLands pid n g = List.foldl' (\g2 _ -> snd (S.addPermanent mountain pid g2)) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 4 S.threePlayerGame)
            (_, withPyromancer) = S.addPermanent pyromancer S.alice withLands
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

-- The printed rider "This ability triggers only once each turn"
-- (Pawl.Types.TriggerLimit), on top of the trigger event the group above covers.
-- No comprehensive rule states the clause; CR 702.179d is where the rulebook
-- prints it verbatim, and Pawl.Engine.Event.withinTriggerLimit is what spends it.
--
-- Whispering Wizard, {3}{U} Creature -- Human Wizard 3/2: "Whenever you cast a
-- noncreature spell, create a 1/1 white Spirit creature token with flying. This
-- ability triggers only once each turn." Nothing of the card is omitted. It is
-- Young Pyromancer above with the rider and a wider filter, which is the point:
-- the SAME three casts run past both creatures below, and the Pyromancer's three
-- Elementals are what prove the board really offers three trigger events rather
-- than one.
--
-- THREE noncreature spells, each a different card with a different draw --
-- Think Twice draws alice one, Divination two, Vision Skeins two to every seat.
-- A cast that silently failed would leave the Spirit count right and a hand size
-- wrong, so "fired once" is told from "fired three times and did nothing twice"
-- and from "cast once".
--
-- THREE seats, so Vision Skeins' "each player" is not two readings at once, and
-- twelve library cards apiece so CR 104.3c decks nobody mid-case.
--
-- Ten Islands: seven pays the three casts of a turn, and the three left over pay
-- the turn-boundary case's fourth cast without an untap step. Every case below
-- casts on that one board, so mana can never be what separates them.
whisperingWizardSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
whisperingWizardSpec s registry =
  let spirit = CardName.MkCardName (Text.pack "Spirit Token")
      spiritsOf = S.countOnBattlefieldByName spirit
      elemental = CardName.MkCardName (Text.pack "Elemental Token")
      -- CR 603.3b's own record of an ability triggering, counted for one source.
      -- The Spirit count says what RESOLVED; this says what TRIGGERED, which is
      -- what the rider limits.
      firedBy oid gs = length $ do
        GameEvent.AbilityTriggered record <- S.eventsOf gs
        Monad.guard (AbilityTriggered.source record == TriggerSource.OfObject oid)
        pure ()
      board island bearer n =
        let withLands = S.landsFor island S.alice 10 S.threePlayerGame
            addBearer (ids, g) _ = let (oid, g2) = S.addPermanent bearer S.alice g in (ids <> [oid], g2)
            (bearers, withBearers) = List.foldl' addBearer ([], withLands) [1 .. (n :: Int)]
            stock g pid = List.foldl' (\g2 _ -> snd (S.addLibraryCard island pid g2)) g [1 .. (12 :: Int)]
            stocked = List.foldl' stock withBearers [S.alice, S.bob, S.carol]
         in ( bearers,
              stocked
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      -- The three casts, resolved one at a time so each trigger is a batch of its
      -- own -- which is the harder case for the rider, the log having to carry
      -- the first firing across two later scans.
      threeCasts think divine skeins gs0 =
        let (t, g1) = S.addHandCard think S.alice gs0
            (d, g2) = S.addHandCard divine S.alice g1
            (v, g3) = S.addHandCard skeins S.alice g2
         in castAndResolve S.alice v (castAndResolve S.alice d (castAndResolve S.alice t g3))
      -- The same three trigger events inside ONE gather: nobody receives priority
      -- between the casts, so all three SpellCast events are unscanned when
      -- Engine.placePendingTriggers finally runs and the batch holds three
      -- entries at once. Three INSTANTS, since CR 307.1 would not let a sorcery
      -- go on a stack that is not empty.
      threeAtOnce think skeins gs0 =
        let (t1, g1) = S.addHandCard think S.alice gs0
            (t2, g2) = S.addHandCard think S.alice g1
            (v, g3) = S.addHandCard skeins S.alice g2
            castAll = S.runPure S.identityAnswer g3 (S.cast S.alice t1 >> S.cast S.alice t2 >> S.cast S.alice v)
         in S.runPure S.identityAnswer castAll Engine.priorityLoop
   in Spec.describe s "TriggerLimit" $ do
        -- THE case: three trigger events in one turn, one triggering.
        Spec.it s "three noncreature casts in one turn trigger Whispering Wizard once" $ do
          island <- S.printingOf s registry "Island"
          wizard <- S.printingOf s registry "Whispering Wizard"
          think <- S.printingOf s registry "Think Twice"
          divine <- S.printingOf s registry "Divination"
          skeins <- S.printingOf s registry "Vision Skeins"
          let (bearers, gs) = board island wizard 1
              after = threeCasts think divine skeins gs
          -- Each cast resolved, and each one differently: a fixture that cast
          -- only the first would read 1 here rather than 5.
          Spec.assertEqWith s "alice drew from all three spells" (S.handSize S.alice after) 5
          Spec.assertEqWith s "and only Vision Skeins reached bob" (S.handSize S.bob after) 2
          Spec.assertEqWith s "and carol alike" (S.handSize S.carol after) 2
          Spec.assertEqWith s "the ability triggered exactly once" (fmap (`firedBy` after) bearers) [1]
          Spec.assertEqWith s "so exactly one Spirit token" (spiritsOf S.alice after) 1
        -- The same three casts against the UNLIMITED twin. One creature apart
        -- from the case above, and the only thing it can prove is that the board
        -- offers three trigger events -- so "one Spirit" above is the rider and
        -- not a board that cast once.
        Spec.it s "the same three casts fire Young Pyromancer three times" $ do
          island <- S.printingOf s registry "Island"
          pyromancer <- S.printingOf s registry "Young Pyromancer"
          think <- S.printingOf s registry "Think Twice"
          divine <- S.printingOf s registry "Divination"
          skeins <- S.printingOf s registry "Vision Skeins"
          let (bearers, gs) = board island pyromancer 1
              after = threeCasts think divine skeins gs
          Spec.assertEqWith s "the unlimited ability triggered three times" (fmap (`firedBy` after) bearers) [3]
          Spec.assertEqWith s "so three Elemental tokens" (S.countOnBattlefieldByName elemental S.alice after) 3
        -- The other half of "more than once in a turn": three trigger events in
        -- ONE batch, where no event is in the log yet when the batch is filtered.
        -- The Pyromancer half is the same board one creature apart, and proves
        -- the batch really does hold three entries.
        Spec.it s "three casts in one batch trigger Whispering Wizard once" $ do
          island <- S.printingOf s registry "Island"
          wizard <- S.printingOf s registry "Whispering Wizard"
          pyromancer <- S.printingOf s registry "Young Pyromancer"
          think <- S.printingOf s registry "Think Twice"
          skeins <- S.printingOf s registry "Vision Skeins"
          let (bearers, gs) = board island wizard 1
              after = threeAtOnce think skeins gs
              (twins, twinBoard) = board island pyromancer 1
              twinAfter = threeAtOnce think skeins twinBoard
          Spec.assertEqWith s "all three spells resolved" (S.handSize S.alice after) 4
          Spec.assertEqWith s "the ability triggered exactly once" (fmap (`firedBy` after) bearers) [1]
          Spec.assertEqWith s "so exactly one Spirit token" (spiritsOf S.alice after) 1
          Spec.assertEqWith s "the unlimited twin saw three events in that batch" (fmap (`firedBy` twinAfter) twins) [3]
          Spec.assertEqWith s "and made three Elementals" (S.countOnBattlefieldByName elemental S.alice twinAfter) 3
        -- A cast the Filter rejects spends nothing: the rider is spent by the
        -- ability TRIGGERING, not by an event that merely looks like its own.
        Spec.it s "a creature spell neither fires the ability nor spends its rider" $ do
          island <- S.printingOf s registry "Island"
          wizard <- S.printingOf s registry "Whispering Wizard"
          homunculus <- S.printingOf s registry "Furtive Homunculus"
          think <- S.printingOf s registry "Think Twice"
          let (bearers, gs) = board island wizard 1
              (creature, g1) = S.addHandCard homunculus S.alice gs
              (spell, g2) = S.addHandCard think S.alice g1
              creatureCast = castAndResolve S.alice creature g2
              after = castAndResolve S.alice spell creatureCast
          Spec.assertEqWith s "the Homunculus resolved onto the battlefield" (S.countOnBattlefieldByName (S.printingName homunculus) S.alice creatureCast) 1
          Spec.assertEqWith s "and fired nothing" (fmap (`firedBy` creatureCast) bearers) [0]
          Spec.assertEqWith s "with no Spirit token" (spiritsOf S.alice creatureCast) 0
          Spec.assertEqWith s "the noncreature cast that follows still fires" (fmap (`firedBy` after) bearers) [1]
          Spec.assertEqWith s "and makes its Spirit" (spiritsOf S.alice after) 1

-- The other printed rider, "This ability triggers only once" -- once per GAME
-- rather than once per turn (Pawl.Types.TriggerLimit's OncePerGame). No
-- comprehensive rule states it either; it is the per-turn clause of
-- `whisperingWizardSpec` above with the window widened, and
-- Pawl.Engine.Event.withinTriggerLimit spends both.
--
-- Acrobatic Cheerleader, {1}{W} Creature -- Human Survivor 2/2: "Survival -- At
-- the beginning of your second main phase, if this creature is tapped, put a
-- flying counter on it. This ability triggers only once." Nothing of the card is
-- omitted; the ability word is flavor (CR 207.2c).
--
-- TWO of alice's turns, because one cannot tell the readings apart: the rider is
-- spent on the first firing whichever window it names, so only a SECOND turn's
-- trigger event separates "once each turn" (which would fire again) from "once"
-- (which does not). CR 603.3b's log is what the handoff clears, so the second
-- turn is also where a per-turn record would silently re-arm.
--
-- The creature is tapped by hand on each of alice's turns rather than by
-- attacking: CR 508.1f's tap is a fine way to satisfy the intervening "if", but
-- an attack puts a combat damage step between the two readings and the case is
-- about neither. Both turns tap it the same way, so the tap is never what
-- separates them.
--
-- The reading is Projection.keywordsOf, which counts INSTANCES, and a CR 122.1b
-- keyword counter grants one apiece -- so one counter reads as Just 1 where the
-- per-turn reading's two counters would read as Just 2. A bare "it flies" cannot
-- tell the two apart and is not the assertion.
acrobaticCheerleaderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
acrobaticCheerleaderSpec s registry =
  let -- Run whole steps until `phase` is the CURRENT one and has NOT yet run --
      -- Engine.runStep runs GameState.phase and only then advances -- so a case
      -- can read the board as a step begins. Bounded so a fixture that never
      -- reaches the step ends rather than hangs.
      stepUntil done gs0 =
        let go n g =
              if n <= (0 :: Int) || done g
                then g
                else go (n - 1) (snd (Engine.runGamePure S.identityAnswer g Engine.runStep))
         in go 40 gs0
      atPhase p g = GameState.phase g == p
      alicesPrecombatMain g = GameState.activePlayer g == S.alice && atPhase Phase.PrecombatMain g
      oneStep = snd . (\g -> Engine.runGamePure S.identityAnswer g Engine.runStep)
      tapState oid g = fmap Object.tapped (Map.lookup oid (GameState.objects g))
      board cheerleader plains =
        let (oid, withCheerleader) = S.addPermanent cheerleader S.alice (Setup.emptyGame S.bothPlayers)
            stock g pid = List.foldl' (\g2 _ -> snd (S.addLibraryCard plains pid g2)) g [1 .. (12 :: Int)]
            stocked = List.foldl' stock withCheerleader [S.alice, S.bob]
         in ( oid,
              S.tapObject
                oid
                stocked
                  { GameState.phase = Phase.PrecombatMain,
                    -- The schedule has to agree with the phase the board is set
                    -- to. Setup.emptyGame's own `remaining` still holds the
                    -- beginning phase and the precombat main, so stepping from
                    -- here begins the precombat main a SECOND time -- and CR
                    -- 505.1b counts main phases that have begun, which would make
                    -- the postcombat main this turn's third.
                    GameState.remaining = Seq.drop 1 (Turn.dropRestOfPhase (Phase.Beginning BeginningStep.Upkeep) Turn.laterPhases),
                    GameState.activePlayer = S.alice,
                    GameState.priority = Just S.alice
                  }
            )
   in Spec.describe s "TriggerLimit" $ do
        Spec.it s "Acrobatic Cheerleader's rider is spent for the whole game, not the turn" $ do
          cheerleader <- S.printingOf s registry "Acrobatic Cheerleader"
          plains <- S.printingOf s registry "Plains"
          let (oid, gs) = board cheerleader plains
              firstMain = stepUntil (atPhase Phase.PostcombatMain) gs
              fired = oneStep firstMain
              -- Through alice's ending phase, bob's whole turn, and alice's untap
              -- step, which is what untaps the creature again.
              nextTurn = stepUntil alicesPrecombatMain fired
              retapped = S.tapObject oid nextTurn
              secondMain = stepUntil (atPhase Phase.PostcombatMain) retapped
              after = oneStep secondMain
          Spec.assertEqWith s "one instance of flying across both of alice's second main phases" (Map.lookup Keyword.Flying (Projection.keywordsOf oid after)) (Just 1)
          Spec.assertEqWith s "the first of the two is where it triggered" (Map.lookup Keyword.Flying (Projection.keywordsOf oid fired)) (Just 1)
          Spec.assertEqWith s "and the second really offered the trigger again, tapped as its second main phase began" (GameState.phase secondMain, tapState oid secondMain, GameState.activePlayer secondMain) (Phase.PostcombatMain, Just TapState.Tapped, S.alice)

-- CR 505.1b: "second main phase" counts the main phases that have occurred this
-- turn, so an extra main phase moves which phase the card means -- it does not
-- give the card a second chance at one. CR 505.1a is the other half and the
-- reason this is not the phase's name: every main phase after the first is a
-- POSTCOMBAT main phase, so a turn with an extra one holds two of those and only
-- the ordinal tells the second main phase from the third.
--
-- Acrobatic Cheerleader, {1}{W} Creature -- Human Survivor 2/2: "Survival -- At
-- the beginning of your second main phase, if this creature is tapped, put a
-- flying counter on it. This ability triggers only once." Relentless Assault,
-- {2}{R}{R} Sorcery: "Untap all creatures that attacked this turn. After this
-- main phase, there is an additional combat phase followed by an additional main
-- phase." Nothing of either is omitted.
--
-- The line of play makes the two readings disagree in the counter, not merely in
-- the timing: alice casts the Assault in her precombat main and does NOT attack
-- in the extra combat, so at the SECOND main phase the Cheerleader is untapped
-- and CR 603.4's intervening "if" is false. She then attacks in the regular
-- combat, so at the THIRD main phase it IS tapped -- which is the one moment a
-- reading keyed to the phase's NAME would fire, and the printed card does not.
--
-- The pair is one board apart: the control never casts the Assault, and its
-- postcombat main really is the second main phase, so the same attack does put
-- the counter on. Both runs start from the same fixture and answer every prompt
-- the same way; only the cast differs.
--
-- The reading is Projection.keywordsOf, which counts CR 122.1b counter INSTANCES
-- -- Nothing where the ability never fired, against the control's Just 1 -- so a
-- bare "it does not fly" cannot stand in for it.
secondMainPhaseSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
secondMainPhaseSpec s registry =
  let atPhase p g = GameState.phase g == p
      -- Run whole steps until `phase` is the current one and has NOT yet run.
      -- Bounded so a fixture that never reaches the step ends rather than hangs.
      stepUntil :: (forall r. Prompt.Prompt r -> r) -> (GameState.GameState -> Bool) -> GameState.GameState -> GameState.GameState
      stepUntil answer done gs0 =
        let go n g =
              if n <= (0 :: Int) || done g
                then g
                else go (n - 1) (snd (Engine.runGamePure answer g Engine.runStep))
         in go 64 gs0
      oneStep :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
      oneStep answer = snd . (\g -> Engine.runGamePure answer g Engine.runStep)
      tapState oid g = fmap Object.tapped (Map.lookup oid (GameState.objects g))
      flyingOn oid g = Map.lookup Keyword.Flying (Projection.keywordsOf oid g)
      -- Four Mountains is exactly Relentless Assault's {2}{R}{R}. bob holds no
      -- creature, so nothing blocks and the Cheerleader's attack is what taps it
      -- (CR 508.1f); twelve library cards a seat so no draw step decks anybody
      -- (CR 104.3c).
      board cheerleader mountain island assault =
        let (oid, withCheerleader) = S.addPermanent cheerleader S.alice (Setup.emptyGame S.bothPlayers)
            withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) withCheerleader [1 .. (4 :: Int)]
            stock g pid = List.foldl' (\g2 _ -> snd (S.addLibraryCard island pid g2)) g [1 .. (12 :: Int)]
            stocked = List.foldl' stock withLands [S.alice, S.bob]
            (spell, withSpell) = S.addHandCard assault S.alice stocked
         in ( oid,
              spell,
              withSpell
                { GameState.phase = Phase.PrecombatMain,
                  -- The same agreement between phase and schedule
                  -- `acrobaticCheerleaderSpec`'s board makes, and for the same
                  -- reason: a precombat main begun twice miscounts every later
                  -- main phase.
                  GameState.remaining = Seq.drop 1 (Turn.dropRestOfPhase (Phase.Beginning BeginningStep.Upkeep) Turn.laterPhases),
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      boardOf = do
        cheerleader <- S.printingOf s registry "Acrobatic Cheerleader"
        mountain <- S.printingOf s registry "Mountain"
        island <- S.printingOf s registry "Island"
        assault <- S.printingOf s registry "Relentless Assault"
        pure (board cheerleader mountain island assault)
   in Spec.describe s "SecondMainPhase" $ do
        Spec.it s "CR 505.1b an extra main phase makes the postcombat main the third, and it does not trigger" $ do
          (oid, spell, gs) <- boardOf
          let cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spell))
              resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
              -- S.identityAnswer declares no attackers, so the extra combat
              -- passes with the Cheerleader untapped.
              secondMain = stepUntil S.identityAnswer (atPhase Phase.PostcombatMain) resolved
              afterSecond = oneStep S.identityAnswer secondMain
              -- S.aggressiveAnswer attacks with everything, so the regular combat
              -- taps it before the turn's third main phase.
              thirdMain = stepUntil S.aggressiveAnswer (atPhase Phase.PostcombatMain) afterSecond
              after = oneStep S.aggressiveAnswer thirdMain
          Spec.assertEqWith s "no flying counter: the third main phase is not the second" (flyingOn oid after) Nothing
          Spec.assertEqWith s "the extra main phase really ran, with the Cheerleader untapped" (GameState.phase secondMain, tapState oid secondMain) (Phase.PostcombatMain, Just TapState.Untapped)
          Spec.assertEqWith s "and the third main phase really ran, with it tapped" (GameState.phase thirdMain, tapState oid thirdMain) (Phase.PostcombatMain, Just TapState.Tapped)
          Spec.assertEqWith s "the rider is unspent, so nothing but the ordinal held the trigger back" (length (GameState.triggeredThisGame after)) 0

-- The same CR 601.2i cast, read for WHICH cast of the turn it was --
-- SpellCast.ordinal. The cast-side twin of Erudite Wizard's draw ordinal
-- (data/scenarios/event-trigger), and the two conditions answer the same question
-- about different events.
--
-- Clarion Spirit, {1}{W} Creature -- Spirit 2/2: "Whenever you cast your second
-- spell each turn, create a 1/1 white Spirit creature token with flying."
-- Nothing of this card is omitted, and nothing of it is anything but the
-- ordinal -- so these cases cannot be passing on some other clause. Chosen over
-- Lavinia, Foil to Conspiracy, who prints the same ordinal beside a mana ability
-- and an activation rider naming a turn with no phase (Pawl.ManaSpec's
-- laviniaTurnRiderSpec) -- two more clauses, neither bearing on the ordinal
-- either way.
--
-- The spells cast are Boil, {3}{R} Instant "Destroy all Islands", for
-- youngPyromancerSpec's reasons: it targets nothing, so no answerer choice
-- enters the fixture, and nobody here controls an Island, so a resolution
-- changes nothing an assertion reads. The Spirit token is the only thing a cast
-- can add to the battlefield, and the count of them is the whole observable --
-- so "fired on the second" is told apart from "fired on any" (three tokens) and
-- from "fired on the first" (a token after the first cast) by reading it after
-- EACH cast rather than at the end.
--
-- THREE seats, for youngPyromancerSpec's reason, and the opponent case below
-- needs them: bob's cast between two of alice's is what separates a count of
-- the casts the Filter admits from a count of every cast in the log.
clarionSpiritSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
clarionSpiritSpec s registry =
  let spirit = CardName.MkCardName (Text.pack "Spirit Token")
      spiritsOf = S.countOnBattlefieldByName spirit
      -- Four Mountains per Boil, and no untap step runs in any of these cases,
      -- so alice's sixteen are exactly the four casts the longest one makes.
      board mountain clarion =
        let addLands pid n g = List.foldl' (\g2 _ -> snd (S.addPermanent mountain pid g2)) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 16 S.threePlayerGame)
            (_, withClarion) = S.addPermanent clarion S.alice withLands
         in withClarion
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      -- n copies of Boil in a hand, returned in the order they were added.
      handOf boil pid n gs =
        List.foldl'
          (\(oids, g) _ -> let (oid, g2) = S.addHandCard boil pid g in (oids <> [oid], g2))
          ([], gs)
          [1 .. (n :: Int)]
   in Spec.describe s "SpellCast, an ordinal" $ do
        -- THE case, and the one three casts are needed for: the ordinal is an
        -- EQUALITY, so the third cast fires nothing either.
        Spec.it s "CR 601.2i the turn's SECOND cast fires Clarion Spirit, and no other" $ do
          mountain <- S.printingOf s registry "Mountain"
          clarion <- S.printingOf s registry "Clarion Spirit"
          boil <- S.printingOf s registry "Boil"
          case handOf boil S.alice 3 (board mountain clarion) of
            ([first, second, third], gs) -> do
              let afterFirst = castAndResolve S.alice first gs
                  afterSecond = castAndResolve S.alice second afterFirst
                  afterThird = castAndResolve S.alice third afterSecond
              Spec.assertEqWith s "no Spirit before any cast" (spiritsOf S.alice gs) 0
              Spec.assertEqWith s "the FIRST cast makes none" (spiritsOf S.alice afterFirst) 0
              Spec.assertEqWith s "the SECOND makes exactly one" (spiritsOf S.alice afterSecond) 1
              Spec.assertEqWith s "and the THIRD makes no more" (spiritsOf S.alice afterThird) 1
            _ -> Spec.assertFailure s "fixture should put three Boil in alice's hand"
        -- "EACH turn": the count restarts at the handoff, which is what tells a
        -- per-turn ordinal from a running total. A total would fire once, on the
        -- second cast of the four, and never again.
        --
        -- Boil is an instant, so alice's two casts after the handoff are legal on
        -- bob's turn, and TurnScope.EachTurn is what lets them fire at all.
        Spec.it s "CR 601.2i the count is per turn: the handoff clears it and the next turn fires again" $ do
          mountain <- S.printingOf s registry "Mountain"
          clarion <- S.printingOf s registry "Clarion Spirit"
          boil <- S.printingOf s registry "Boil"
          case handOf boil S.alice 4 (board mountain clarion) of
            ([a, b, c, d], gs) -> do
              let thisTurn = castAndResolve S.alice b (castAndResolve S.alice a gs)
                  handed = S.runPure S.identityAnswer thisTurn Engine.handoffTurn
                  nextTurn = castAndResolve S.alice d (castAndResolve S.alice c handed)
              Spec.assertEqWith s "the first turn's second cast fired it once" (spiritsOf S.alice thisTurn) 1
              Spec.assertEqWith s "the handoff clears the log the count reads" (GameState.events handed) Seq.empty
              Spec.assertEqWith s "and the new turn's second cast fires it again" (spiritsOf S.alice nextTurn) 2
            _ -> Spec.assertFailure s "fixture should put four Boil in alice's hand"
        -- A spell the log holds no cast of has no ordinal, as it has no storm
        -- count: Game.castsBefore answers Nothing for both readers. No real
        -- board reaches it (CR 601.2i logs the cast before CR 603.2 checks), so
        -- the never-cast Boil in hand stands in for it.
        Spec.it s "CR 601.2i a spell with no cast in the log has no place among the turn's casts" $ do
          mountain <- S.printingOf s registry "Mountain"
          clarion <- S.printingOf s registry "Clarion Spirit"
          boil <- S.printingOf s registry "Boil"
          case handOf boil S.alice 2 (board mountain clarion) of
            ([first, uncast], gs) -> do
              let afterFirst = castAndResolve S.alice first gs
                  logged = Maybe.mapMaybe (Game.castOf . LoggedEvent.event) (Foldable.toList (GameState.events afterFirst))
                  context = Filter.contextFor (Game.teams afterFirst) (Just S.alice) Nothing
                  ordinal oid = Match.castOrdinal context (Filter.Type.And []) Nothing oid afterFirst
              case logged of
                [cast] -> do
                  Spec.assertEqWith s "the uncast Boil has no ordinal" (ordinal uncast) Nothing
                  Spec.assertEqWith s "and no casts before it" (fmap length (Game.castsBefore uncast afterFirst)) Nothing
                  Spec.assertEqWith s "while the cast one is the first" (ordinal (SpellWasCast.spell cast)) (Just 1)
                other -> Spec.assertFailure s ("expected one logged cast, got " <> show (length other))
            _ -> Spec.assertFailure s "fixture should put two Boil in alice's hand"

-- CR 113.6k: the first ability in the pool that functions from the STACK. The
-- same rule that put Narcomoeba's in a graveyard, one zone over.
--
-- Desolation Twin, {10} Creature -- Eldrazi 10/10: "When you cast this spell,
-- create a 10/10 colorless Eldrazi creature token." Chosen from the cast-trigger
-- family because it is the one member whose WHOLE printed text pawl can write:
-- every other printing in that family wants CR 707.10's copy-a-spell. Nothing of
-- this card is omitted.
--
-- The bearer is the SPELL, which is what makes this a zone test rather than
-- another SpellCast case: at CR 601.2i the Twin is on nobody's battlefield and in
-- nobody's graveyard, so every candidate source but Event.eventTriggers'
-- `spellCast` misses it entirely, and the token below never appears.
desolationTwinSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
desolationTwinSpec s registry =
  let eldrazi = CardName.MkCardName (Text.pack "Eldrazi Token")
      eldraziOf = S.countOnBattlefieldByName eldrazi
      -- Ten Mountains, which is the Twin's {10} exactly and Goblin Piker's
      -- {1}{R} with plenty to spare -- the negative case below casts on the same
      -- board, so mana can never be what separates the two.
      board mountain =
        let withLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.alice g)) (Setup.emptyGame S.bothPlayers) [1 .. (10 :: Int)]
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
        let addLands pid n printing g = List.foldl' (\g2 _ -> snd (S.addPermanent printing pid g2)) g [1 .. (n :: Int)]
            withLands =
              addLands S.bob 3 mountain
                . addLands S.bob 3 swamp
                . addLands S.alice 3 mountain
                $ addLands S.alice 3 swamp S.threePlayerGame
            (_, withPresence) = S.addPermanent presence S.alice withLands
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
        let addLands pid n g = List.foldl' (\g2 _ -> snd (S.addPermanent mountain pid g2)) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 S.threePlayerGame
            (_, withKambal) = S.addPermanent kambal S.alice withLands
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
brinebornCutthroatSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
brinebornCutthroatSpec s registry =
  let -- alice bears the Cutthroat and three Forests, one per Fog: no untap step
      -- alice keeps priority throughout: CR 117.1a lets her cast an instant on
      -- anybody's turn, which is the whole premise of the card.
      onTurnOf pid gs = gs {GameState.activePlayer = pid, GameState.priority = Just S.alice}
   in Spec.describe s "SpellCast during an opponent's turn" $ do
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
          let addLands printing pid n g = List.foldl' (\g2 _ -> snd (S.addPermanent printing pid g2)) g [1 .. (n :: Int)]
              lands = addLands mountain S.alice 3 (addLands island S.alice 2 S.threePlayerGame)
              (cutthroatId, withCutthroat) = S.addHandCard cutthroat S.alice lands
              (pikerId, gs) = S.addHandCard piker S.alice withCutthroat
              bobsTurn = (onTurnOf S.bob gs) {GameState.phase = Phase.PrecombatMain}
          Spec.assertBool s (S.castable S.alice cutthroatId bobsTurn) "flash makes the Cutthroat castable on bob's turn"
          Spec.assertBool s (not (S.castable S.alice pikerId bobsTurn)) "and a creature without it is not"

-- CR 701.26b's untap as a TRIGGER EVENT, which nothing could watch until
-- Oreskos Sun Guide, {1}{W} Creature -- Cat Monk: "Inspired -- Whenever this
-- creature becomes untapped, you gain 2 life." ("Inspired" is CR 207.2c's
-- ability word and carries no rules meaning.) One trigger condition over one event, and
-- the effect is a life gain the engine already had, so the only new thing these
-- cases can be passing on is the condition and the event behind it.
--
-- BOTH ROADS that untap are driven: CR 502.3's turn-based batch in
-- Pawl.Engine.Engine.untapAll, which writes its own events so the step stays
-- simultaneous, and Pawl.Engine.Event.untap, the one-at-a-time funnel an
-- Effect.Untap and CR 107.6's untap symbol share. Nothing in data/cards/ pairs a
-- "becomes untapped" trigger with an untap effect on one board, so the second
-- road is driven through its funnel directly, the way the cycling case above
-- drives Pawl.Engine.Event.discard.
--
-- alice's Goblin Piker is the second permanent on every board but the entry
-- one: it makes the untap step a BATCH rather than a single permanent, so the
-- first two cases separate "an untap happened" from "the BEARER's untap
-- happened" rather than "nothing happened at all".
oreskosSunGuideSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oreskosSunGuideSpec s registry =
  let untapStep gs = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
      placeTriggers gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveOne gs = S.runPure S.identityAnswer gs Stack.resolveTop
   in Spec.describe s "Oreskos Sun Guide" $ do
        -- The whole card on its printed road: the Guide is tapped when alice's
        -- untap step runs, so CR 502.3 untaps it, CR 701.26b records the event,
        -- and the trigger resolves for 2 life.
        Spec.it s "CR 502.3 the untap step untaps the Guide and its trigger gains alice 2 life" $ do
          guide <- S.printingOf s registry "Oreskos Sun Guide"
          piker <- S.printingOf s registry "Goblin Piker"
          let (guideId, g0) = S.addPermanent guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addPermanent piker S.alice g0
              board = S.tapObject pikerId (S.tapObject guideId g1)
              stepped = untapStep board
              placed = placeTriggers stepped
          Spec.assertEqWith s "alice gains 2 once the trigger resolves" (S.lifeOf S.alice (resolveOne placed)) (Just 22)
          Spec.assertEqWith s "one trigger reached the stack, and only one" (length (GameState.stack placed)) 1
          Spec.assertEqWith s "and the Guide is upright" (fmap Object.tapped (Game.lookupObject guideId stepped)) (Just TapState.Untapped)
        -- The discriminating twin, one tap state apart: the Guide is ALREADY
        -- upright, so rule 701.26b's second sentence leaves it alone while the
        -- Piker beside it still untaps. An untap event is recorded on this board
        -- too, which is what makes this the bearer check rather than a board
        -- where nothing happened.
        Spec.it s "CR 701.26b an already-upright Guide is not untapped, and the Piker's untap is not its own" $ do
          guide <- S.printingOf s registry "Oreskos Sun Guide"
          piker <- S.printingOf s registry "Goblin Piker"
          let (_, g0) = S.addPermanent guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addPermanent piker S.alice g0
              board = S.tapObject pikerId g1
              stepped = untapStep board
              placed = placeTriggers stepped
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice (resolveOne placed)) (Just 20)
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "though the Piker did untap" (fmap Object.tapped (Game.lookupObject pikerId stepped)) (Just TapState.Untapped)
        -- CR 603.2e's second sentence, the collapse this condition has to
        -- survive: the Guide ENTERS the battlefield untapped, which is not a
        -- transition, so nothing triggers. Through the stack rather than through
        -- S.addPermanent, so the real entry funnel runs.
        Spec.it s "CR 603.2e a Guide that ENTERS untapped does not trigger" $ do
          guide <- S.printingOf s registry "Oreskos Sun Guide"
          let (_, staged) = S.spellOnStack guide S.alice (Setup.emptyGame S.bothPlayers)
              entered = resolveOne staged
              placed = placeTriggers entered
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice (resolveOne placed)) (Just 20)
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "though the Guide is on the battlefield and upright" (S.countOnBattlefieldByName (S.printingName guide) S.alice entered) 1
        -- The OTHER road, one permanent at a time: Pawl.Engine.Event.untap is
        -- what an Effect.Untap and CR 107.6's untap symbol both call, and it
        -- writes the same event. The Piker is untapped through the same funnel
        -- on the same board and fires nothing.
        Spec.it s "CR 701.26b the one-at-a-time funnel records the same event" $ do
          guide <- S.printingOf s registry "Oreskos Sun Guide"
          piker <- S.printingOf s registry "Goblin Piker"
          let (guideId, g0) = S.addPermanent guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addPermanent piker S.alice g0
              board = S.tapObject pikerId (S.tapObject guideId g1)
              untapOne oid = placeTriggers (S.runPure S.identityAnswer board (Event.untap oid))
          Spec.assertEqWith s "untapping the Guide gains alice 2" (S.lifeOf S.alice (resolveOne (untapOne guideId))) (Just 22)
          Spec.assertEqWith s "untapping the Piker instead gains nothing" (S.lifeOf S.alice (resolveOne (untapOne pikerId))) (Just 20)

-- CR 113.2c: two instances of one ability function independently, so each
-- spends its own "This ability triggers only once each turn"
-- (Pawl.Engine.Event.withinTriggerLimit). Well Rested, {1}{G} Enchantment --
-- Aura: "Enchant creature / Enchanted creature has 'Whenever this creature
-- becomes untapped, put two +1/+1 counters on it, then you gain 2 life and draw
-- a card. This ability triggers only once each turn.'" Nothing omitted.
--
-- Two on one Goblin Piker grant it two value-identical instances. The untap
-- step fires both; a second untap that turn, through Event.untap, fires
-- neither.
wellRestedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wellRestedSpec s registry =
  Spec.describe s "Well Rested" $ do
    Spec.it s "CR 113.2c two Well Rested on one creature each trigger once that turn" $ do
      rested <- S.printingOf s registry "Well Rested"
      piker <- S.printingOf s registry "Goblin Piker"
      island <- S.printingOf s registry "Island"
      let (pikerId, g0) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
          (first, g1) = S.addPermanent rested S.alice g0
          (second, g2) = S.addPermanent rested S.alice g1
          stocked = List.foldl' (\g _ -> snd (S.addLibraryCard island S.alice g)) (S.attach second pikerId (S.attach first pikerId g2)) [1 .. (5 :: Int)]
          resolveOne gs = S.runPure S.identityAnswer gs Stack.resolveTop
          settle gs = resolveOne (resolveOne (S.runPure S.identityAnswer gs Engine.settleForPriority))
          rested1 = settle (S.runPure S.identityAnswer (S.tapObject pikerId stocked) (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap)))
          rested2 = S.runPure S.identityAnswer (S.runPure S.identityAnswer (S.tapObject pikerId rested1) (Event.untap pikerId)) Engine.settleForPriority
          countersOn gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject pikerId gs)
      Spec.assertEqWith s "both instances resolved: four +1/+1 counters" (countersOn rested1) (Just 4)
      Spec.assertEqWith s "and 4 life and two cards" (S.lifeOf S.alice rested1, S.handSize S.alice rested1) (Just 24, 2)
      Spec.assertEqWith s "a second untap that turn triggers neither" (length (GameState.stack rested2)) 0
    -- The per-GAME rider on the same tally. Engine.reactions writes one
    -- Acrobatic Cheerleader triggering to the turn's log AND to
    -- GameState.triggeredThisGame; it is one triggering, so a second instance
    -- is still owed its own. A regression fence driving withinTriggerLimit
    -- directly: no card in data/cards/ grants a "triggers only once" ability,
    -- and the Cheerleader's event recurs once a turn, so no board gives a source
    -- its second instance between two such events in one turn.
    Spec.it s "CR 113.2c a per-game rider spent this turn is counted once" $ do
      cheerleader <- S.printingOf s registry "Acrobatic Cheerleader"
      case Face.triggeredAbilities (S.combinedFace cheerleader) of
        [] -> Spec.assertFailure s "Acrobatic Cheerleader should declare one triggered ability"
        ability : _ -> do
          let (oid, g0) = S.addPermanent cheerleader S.alice (Setup.emptyGame S.bothPlayers)
              record = AbilityTriggered.MkAbilityTriggered (TriggerSource.OfObject oid) S.alice ability
              spent = Event.recordEvent (GameEvent.AbilityTriggered record) g0 {GameState.triggeredThisGame = Seq.singleton record}
              pending = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) S.alice ability Map.empty Nothing Nothing
          Spec.assertEqWith s "the second of two instances still triggers" (length (Event.withinTriggerLimit spent [pending 2])) 1
          Spec.assertEqWith s "while a lone instance is spent" (length (Event.withinTriggerLimit spent [pending 1])) 0

-- CR 701.68d's blight as a TRIGGER EVENT, which nothing could watch: the whole
-- printed pool blights, and not one card triggers on a player doing it
-- (Scryfall oracle:blight, every "whenever" clause read, 2026-09-05). So the
-- watcher is made up -- Synthetic Blight Chronicler {1}{B} 1/3 Creature --
-- Human Cleric (data/cards/synthetic-blight-chronicler.json): "Whenever a
-- player blights, you draw a card and you lose 1 life." One trigger condition over one event,
-- and both effects are ones the engine already had, so the only new thing these
-- cases can be passing on is the condition and the event behind it.
--
-- The blighter is Sinister Gnarlbark, {2}{B} 0/4 Creature -- Treefolk Warlock
-- (data/cards/sinister-gnarlbark.json): "At the beginning of your end step,
-- draw a card and blight 1." (Name, cost, type line, P/T and oracle text
-- checked against Scryfall.) Its own draw is what tells rule 101.3's ignored
-- PART from an aborted instruction on the no-creature board below.
--
-- WHY A COUNTER TRIGGER CANNOT STAND IN, which is the whole of rule 701.68d's
-- last clause: the Solemnity case blights with every counter kept off the
-- board, so no GameEvent.CountersPut is written and the Chronicler fires all
-- the same.
blightChroniclerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
blightChroniclerSpec s registry =
  let placeTriggers gs = S.runPure S.identityAnswer gs Engine.settleForPriority
      resolveOne gs = S.runPure S.identityAnswer gs Stack.resolveTop
      minusCountersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid gs)
   in Spec.describe s "Synthetic Blight Chronicler" $ do
        -- The card on its plainest board: alice's Gnarlbark blights her own
        -- Gnarlbark, and bob's Chronicler -- a seat away from the blight --
        -- draws and drains him for it.
        Spec.it s "CR 701.68d a blight triggers a watcher on another seat" $ do
          (gnarlbarkId, board) <- blightChroniclerBoard s registry False False
          let blighted = resolveOne board
              placed = placeTriggers blighted
              after = resolveOne placed
          Spec.assertEqWith s "bob loses 1 to his Chronicler" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "and draws the card beside it" (S.handSize S.bob after) 1
          Spec.assertEqWith s "the Gnarlbark took rule 701.68a's counter" (minusCountersOn gnarlbarkId blighted) (Just 1)
          Spec.assertEqWith s "one trigger reached the stack, and only one" (length (GameState.stack placed)) 1
        -- Rule 701.68d's "regardless of what events actually occurred", one
        -- permanent apart from the case above: Solemnity, {2}{W} Enchantment
        -- (data/cards/solemnity.json), "If a counter would be put on an
        -- artifact, creature, enchantment, or land, it isn't." The blight puts
        -- nothing, so a condition reading GameEvent.CountersPut would see no
        -- event at all -- and the Chronicler still fires.
        Spec.it s "CR 701.68d Solemnity keeps every counter off and the Chronicler still triggers" $ do
          (gnarlbarkId, board) <- blightChroniclerBoard s registry True False
          let blighted = resolveOne board
              placed = placeTriggers blighted
              after = resolveOne placed
          Spec.assertEqWith s "bob loses 1 all the same" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "though no counter was put on the Gnarlbark" (minusCountersOn gnarlbarkId blighted) (Just 0)
          Spec.assertEqWith s "and alice's own draw still happened" (S.handSize S.alice blighted) 1
        -- Rule 701.68b's board, and the negative of the first case one
        -- permanent apart: the Gnarlbark dies to state-based actions with its
        -- own trigger already on the stack (CR 603.3b), so alice controls no
        -- creature, rule 701.68a's process never runs, and no blight happened
        -- to trigger on. The draw beside it still does, which is what tells CR
        -- 101.3's ignored part from an aborted instruction.
        Spec.it s "CR 701.68b a controller with no creature blights nothing, and nothing triggers" $ do
          (gnarlbarkId, board) <- blightChroniclerBoard s registry False False
          let dead = S.settleSba (S.markDamage gnarlbarkId 4 board)
              blighted = resolveOne dead
              placed = placeTriggers blighted
              after = resolveOne placed
          Spec.assertEqWith s "bob's life is untouched" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "and he drew nothing" (S.handSize S.bob after) 0
          Spec.assertBool s (not (S.onBattlefield gnarlbarkId blighted)) "the Gnarlbark left before its trigger resolved"
          Spec.assertEqWith s "alice drew all the same" (S.handSize S.alice blighted) 1
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack placed)) 0
        -- CR 102.1's bare "a player", which PlayerRelation.AnyPlayer is and
        -- neither You nor Opponent is: a second Chronicler under ALICE, the
        -- blighting seat, fires beside bob's. An Opponent reading would leave
        -- her at 20 and a You reading would leave him there, so the one board
        -- separates all three.
        Spec.it s "CR 701.68d AnyPlayer reaches the blighting player's own seat too" $ do
          (_, board) <- blightChroniclerBoard s registry False True
          let blighted = resolveOne board
              placed = placeTriggers blighted
              after = resolveOne (resolveOne placed)
          Spec.assertEqWith s "alice loses 1 to her own Chronicler" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "and bob loses 1 to his" (S.lifeOf S.bob after) (Just 19)
          Spec.assertEqWith s "both triggers reached the stack" (length (GameState.stack placed)) 2

-- Sinister Gnarlbark on alice's battlefield and a Synthetic Blight Chronicler
-- on bob's, with the libraries stocked past every draw these cases take (CR
-- 104.3c), alice's end step begun
-- and the Gnarlbark's trigger settled onto the stack (CR 603.3b). Returns the
-- Gnarlbark and that state.
--
-- Two flags, each adding ONE permanent, so every pair of boards below differs in
-- exactly one thing: Solemnity on bob's battlefield, and a second Chronicler on
-- alice's.
blightChroniclerBoard ::
  (Monad m) =>
  Spec.Spec m n ->
  Registry.Registry m ->
  Bool ->
  Bool ->
  m (ObjectId.ObjectId, GameState.GameState)
blightChroniclerBoard s registry withSolemnity withOwnWatcher = do
  swamp <- S.printingOf s registry "Swamp"
  gnarlbark <- S.printingOf s registry "Sinister Gnarlbark"
  chronicler <- S.printingOf s registry "Synthetic Blight Chronicler"
  solemnity <- S.printingOf s registry "Solemnity"
  let (gnarlbarkId, g1) = S.addPermanent gnarlbark S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addPermanent chronicler S.bob g1
      g3 = if withSolemnity then snd (S.addPermanent solemnity S.bob g2) else g2
      g4 = if withOwnWatcher then snd (S.addPermanent chronicler S.alice g3) else g3
      g5 = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.alice g)) g4 [1 :: Int .. 3]
      g6 = List.foldl' (\g _ -> snd (S.addLibraryCard swamp S.bob g)) g5 [1 :: Int .. 3]
      endStep = Phase.Ending EndingStep.EndStep
      begun =
        Event.recordEvent
          (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice))
          (g6 {GameState.phase = endStep, GameState.activePlayer = S.alice})
  pure (gnarlbarkId, S.runPure S.identityAnswer begun Engine.settleForPriority)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  cyclesTriggerSpec s registry
  selfDiscardTriggerSpec s registry
  controllerAtTriggerSpec s registry
  counterTriggerSpec s registry
  auntieOolSpec s registry
  youngPyromancerSpec s registry
  whisperingWizardSpec s registry
  acrobaticCheerleaderSpec s registry
  secondMainPhaseSpec s registry
  clarionSpiritSpec s registry
  desolationTwinSpec s registry
  presenceOfTheMasterSpec s registry
  kambalSpec s registry
  brinebornCutthroatSpec s registry
  oreskosSunGuideSpec s registry
  wellRestedSpec s registry
  blightChroniclerSpec s registry
