{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Pawl.Engine.Trigger over the events that are not zone changes: draws,
-- discards, counters placed and removed, life gained and lost, damage, casts,
-- and attack declarations. The machinery is Pawl.TriggerSpec.
module Pawl.EventTriggerSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
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
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.Zone as Zone

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
          discardBy pid = S.runPure S.identityAnswer gs (Cost.payComponent PaymentMoment.OutsideResolution Map.empty pid S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
          byAlice = discardBy S.alice
          byBob = discardBy S.bob
          settle g = S.runPure S.identityAnswer g Engine.priorityLoop
      Spec.assertEqWith s "alice's card reached her graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice byAlice)) 1
      Spec.assertEqWith s "and bob's reached his" (length (Game.zoneMembers Zone.Graveyard S.bob byBob)) 1
      Spec.assertEqWith s "the opponent's discard costs her 2" (S.lifeOf S.alice (settle byAlice)) (fmap (subtract 2) (S.lifeOf S.alice gs))
      Spec.assertEqWith s "the controller's own discard costs him nothing" (S.lifeOf S.bob (settle byBob)) (S.lifeOf S.bob gs)
      Spec.assertEqWith s "and costs alice nothing either" (S.lifeOf S.alice (settle byBob)) (S.lifeOf S.alice gs)
      Spec.assertEqWith s "bob's discard put nothing on the stack at all" (GameState.stack (S.runPure S.identityAnswer byBob Engine.settleForPriority)) []

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
      (marmosetId, withMarmoset) = S.addCreature marmoset S.alice libraries
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
    -- The whole card: alice cycles the Mauler for {2}, the Marmoset's trigger is
    -- placed above the cycling ability, and the Marmoset is a 4/3 once it
    -- resolves.
    Spec.it s "CR 702.29a whole card: cycling a card pumps Prickly Marmoset" $ do
      forest <- S.printingOf s registry "Forest"
      marmoset <- S.printingOf s registry "Prickly Marmoset"
      mauler <- S.printingOf s registry "Barkhide Mauler"
      piker <- S.printingOf s registry "Goblin Piker"
      let (marmosetId, maulerId, gs) = marmosetBoard marmoset mauler forest piker S.alice
      Spec.assertEqWith s "the Marmoset starts a 2/3" (S.powerToughnessOf marmosetId gs) (Just (2, 3))
      case Activate.abilitiesFor maulerId gs of
        [ability] -> do
          let cycled = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice maulerId ability)
              placed = S.runPure S.identityAnswer cycled Engine.settleForPriority
              after = S.runPure S.identityAnswer placed Stack.resolveTop
          Spec.assertEqWith s "the Mauler was discarded to pay the cost" (length (Game.zoneMembers Zone.Graveyard S.alice cycled)) 1
          Spec.assertEqWith s "the trigger is on the stack, above the cycling ability" (length (GameState.stack placed)) 2
          Spec.assertEqWith s "and the Marmoset is a 4/3 once it resolves" (S.powerToughnessOf marmosetId after) (Just (4, 3))
        abilities -> Spec.assertFailure s ("expected one cycling ability, got " <> show (length abilities))
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
             in case Activate.abilitiesFor maulerId gs of
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
          let (cowId, base) = S.addCreature cow S.alice (Setup.emptyGame S.bothPlayers)
              gs = priorityTo base
              killed = S.settleSba (S.markDamage cowId 3 gs)
              after = settle killed
          Spec.assertBool s (not (S.onBattlefield cowId after)) "the Cow took lethal damage and died"
          Spec.assertEqWith s "exactly one Food token" (S.countOnBattlefieldByName foodTokenName S.alice after) 1

-- CR 121.1's draw, counted. "Whenever you draw your second card each turn" is the
-- pool's reader of WHICH draw of the turn a draw was, and Erudite Wizard, {2}{U}
-- 2/3 Creature -- Human Wizard, prints nothing else: "Whenever you draw your
-- second card each turn, put a +1/+1 counter on this creature." One condition,
-- one targetless effect, so the only new thing these cases can be passing on is
-- the ordinal.
--
-- The draws are Think Twice's, {1}{U} Instant "Draw a card" -- ONE card per
-- resolution, so each case decides for itself how many draws the turn has had
-- and a miscount cannot hide inside a multi-card draw. alice controls the Wizard
-- throughout, so CR 109.5 fixes its "you" as her.
--
-- Every case reads the counter through the Wizard's power and toughness rather
-- than off the object, so what is asserted is what a player at the table sees.
drawTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
drawTriggerSpec s registry =
  Spec.describe s "DrawTrigger" $ do
    -- THE case: two draws on one board, so "the second" is told apart from "any".
    -- CR 121.2 makes each its own draw, and only the second one fires.
    Spec.it s "CR 121.1 the turn's first draw fires nothing and its second fires the Wizard" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      wizard <- S.printingOf s registry "Erudite Wizard"
      let (wizardId, gs, firsts) = drawBoard island piker think wizard 2
      case firsts of
        [firstId, secondId] -> do
          let afterFirst = resolveCast gs firstId
              afterSecond = resolveCast afterFirst secondId
          Spec.assertEqWith s "one card drawn so far" (Map.lookup S.alice (GameState.drawsThisTurn afterFirst)) (Just 1)
          Spec.assertEqWith s "the FIRST draw leaves the Wizard printed-size" (S.powerToughnessOf wizardId afterFirst) (Just (2, 3))
          Spec.assertEqWith s "two cards drawn" (Map.lookup S.alice (GameState.drawsThisTurn afterSecond)) (Just 2)
          Spec.assertEqWith s "the SECOND draw puts a +1/+1 counter on it" (S.powerToughnessOf wizardId afterSecond) (Just (3, 4))
        _ -> Spec.assertFailure s "fixture should put two Think Twice in alice's hand"
    -- CR 121.1's other producer of a draw: the draw step's turn-based action. The
    -- same board and the same single cast, one draw step apart -- so the cast's
    -- draw is the turn's first on one and its second on the other.
    Spec.it s "CR 121.1 the draw step's draw is the turn's first, so the next one fires" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      wizard <- S.printingOf s registry "Erudite Wizard"
      let (wizardId, gs, firsts) = drawBoard island piker think wizard 1
      case firsts of
        [thinkId] -> do
          let stepped = S.runPure S.identityAnswer (gs {GameState.phase = Phase.Beginning BeginningStep.DrawStep}) S.drawStep
              afterStep = resolveCast (stepped {GameState.phase = Phase.PrecombatMain}) thinkId
              afterNoStep = resolveCast gs thinkId
          Spec.assertEqWith s "the draw step drew one card" (Map.lookup S.alice (GameState.drawsThisTurn stepped)) (Just 1)
          Spec.assertEqWith s "so the cast's draw is the second and fires" (S.powerToughnessOf wizardId afterStep) (Just (3, 4))
          Spec.assertEqWith s "without the draw step it is the first and fires nothing" (S.powerToughnessOf wizardId afterNoStep) (Just (2, 3))
        _ -> Spec.assertFailure s "fixture should put one Think Twice in alice's hand"
    -- "EACH turn", which is what makes the tally a per-turn count rather than a
    -- running total: four draws across two turns fire the Wizard TWICE, on the
    -- second draw of each. A tally that accumulated would fire once and stop.
    --
    -- Think Twice is an instant, so the two casts after the handoff are legal on
    -- bob's turn -- and the draws they make are still alice's own (CR 121.1).
    Spec.it s "CR 121.1 the count is per turn: the handoff clears it and the next turn fires again" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      wizard <- S.printingOf s registry "Erudite Wizard"
      let (wizardId, gs, firsts) = drawBoard island piker think wizard 4
      case firsts of
        [a, b, c, d] -> do
          let thisTurn = resolveCast (resolveCast gs a) b
              handed = S.runPure S.identityAnswer thisTurn Engine.handoffTurn
              nextTurn = resolveCast (resolveCast handed c) d
          Spec.assertEqWith s "the first turn's second draw fired it once" (S.powerToughnessOf wizardId thisTurn) (Just (3, 4))
          Spec.assertEqWith s "the handoff clears the tally for every seat" (GameState.drawsThisTurn handed) Map.empty
          Spec.assertEqWith s "and the new turn's second draw fires it again" (S.powerToughnessOf wizardId nextTurn) (Just (4, 5))
        _ -> Spec.assertFailure s "fixture should put four Think Twice in alice's hand"
    -- "YOU draw", not "a player draws". The same Wizard, the same two Think
    -- Twice, the same two draws -- one seat apart, which is the only difference
    -- a board with two seats can express and the one this axis turns on.
    Spec.it s "CR 109.5 'you': bob drawing his second card leaves alice's Wizard alone" $ do
      island <- S.printingOf s registry "Island"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      wizard <- S.printingOf s registry "Erudite Wizard"
      let (wizardId, base, _) = drawBoard island piker think wizard 0
          withLands = S.landsFor island S.bob 4 base
          addThink (ids, g) _ = let (oid, g') = S.addHandCard think S.bob g in (ids <> [oid], g')
          (bobsThinks, withHand) = List.foldl' addThink ([], withLands) [1 .. (2 :: Int)]
          gs = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.bob g)) withHand [1 .. (10 :: Int)]
      case bobsThinks of
        [a, b] -> do
          let after = resolveCastBy S.bob (resolveCastBy S.bob gs a) b
          Spec.assertEqWith s "bob drew two cards" (Map.lookup S.bob (GameState.drawsThisTurn after)) (Just 2)
          Spec.assertEqWith s "alice drew none of them" (Map.lookup S.alice (GameState.drawsThisTurn after)) Nothing
          Spec.assertEqWith s "so her Wizard is printed-size" (S.powerToughnessOf wizardId after) (Just (2, 3))
        _ -> Spec.assertFailure s "fixture should put two Think Twice in bob's hand"

-- alice controls an Erudite Wizard and enough Islands to cast `copies` Think
-- Twice, holds that many of them (none at all when `copies` is 0, which is the
-- board the opponent case builds on), and has ten Goblin Pikers in her library --
-- more than any case draws, so CR 104.3c never decks her before an assertion
-- runs. Returns the Wizard, the board, and the Think Twice ids in hand order.
drawBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  (ObjectId.ObjectId, GameState.GameState, [ObjectId.ObjectId])
drawBoard island piker think wizard copies =
  let (wizardId, base) = S.addCreature wizard S.alice (S.landsInPlay island (2 * copies))
      addThink (ids, g) _ = let (oid, g') = S.addHandCard think S.alice g in (ids <> [oid], g')
      (thinkIds, withHand) = List.foldl' addThink ([], base) [1 .. copies]
      stocked = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) withHand [1 .. (10 :: Int)]
   in ( wizardId,
        stocked
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            -- NOT the first turn: CR 103.8a has the starting player skip that
            -- turn's draw step, and one of the cases above turns on a draw step
            -- that draws.
            GameState.turnNumber = 2
          },
        thinkIds
      )

-- Cast one spell and let the stack empty: the draw happens, and any trigger it
-- fires resolves before the next assertion.
resolveCast :: GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveCast = resolveCastBy S.alice

resolveCastBy :: PlayerId.PlayerId -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveCastBy pid gs oid =
  let cast = S.runPure S.identityAnswer gs (S.cast pid oid)
   in S.runPure S.identityAnswer cast Engine.priorityLoop

-- CR 702.94a's miracle: the reveal-as-you-drawn window (CR 121.9) and the linked
-- triggered ability (CR 603.11) it opens.
--
-- Thunderous Wrath, {4}{R}{R} Instant, "Thunderous Wrath deals 5 damage to any
-- target." plus "Miracle {R}", is the producer -- every clause expressible, and
-- the cost gap between {4}{R}{R} and {R} is what makes the alternative cost
-- observable at all: alice never has six mana on any of these boards, so a leg
-- that dealt 5 damage can only have paid the miracle cost.
--
-- The draws are Think Twice's, {1}{U} Instant "Draw a card", and the draw step's,
-- for `drawTriggerSpec`'s reasons. bob is the victim throughout and starts every
-- leg at the same life, so "5 damage happened" is one subtraction either way.
--
-- Every assertion reads the BOARD -- bob's life, and which zone the Wrath is in
-- -- rather than whether a prompt was raised.
miracleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
miracleSpec s registry =
  Spec.describe s "Miracle" $ do
    -- THE case, and its two negatives on one board: the same first draw of the
    -- same turn, answered three ways. CR 702.94a's two "may"s are separate
    -- questions, so declining the reveal and declining the cast are different
    -- boards -- the third leg reveals and still does not cast.
    Spec.it s "CR 702.94a a revealed first draw may be cast for its miracle cost, and both 'may's are the player's" $ do
      island <- S.printingOf s registry "Island"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      thunder <- S.printingOf s registry "Thunderous Wrath"
      let (gs, thinks) = miracleBoard island mountain piker think thunder 1 0
          wrath = S.printingName thunder
      case thinks of
        [thinkId] -> do
          let cast = resolveCastWith (miracleAnswer OptionalDecision.Exercises OptionalDecision.Exercises) gs thinkId
              hidden = resolveCastWith (miracleAnswer OptionalDecision.Declines OptionalDecision.Exercises) gs thinkId
              shown = resolveCastWith (miracleAnswer OptionalDecision.Exercises OptionalDecision.Declines) gs thinkId
          Spec.assertEqWith s "revealing and casting deals bob 5" (S.lifeOf S.bob cast) (fmap (subtract 5) (S.lifeOf S.bob gs))
          Spec.assertEqWith s "and the Wrath resolved into alice's graveyard" (namedIn wrath Zone.Graveyard S.alice cast) 1
          Spec.assertEqWith s "declining the reveal leaves bob alone" (S.lifeOf S.bob hidden) (S.lifeOf S.bob gs)
          Spec.assertEqWith s "and the Wrath an ordinary card in her hand" (namedIn wrath Zone.Hand S.alice hidden) 1
          Spec.assertEqWith s "revealing and then declining the cast leaves bob alone too" (S.lifeOf S.bob shown) (S.lifeOf S.bob gs)
          Spec.assertEqWith s "and the Wrath still in her hand" (namedIn wrath Zone.Hand S.alice shown) 1
          Spec.assertEqWith s "the reveal happened on that leg even so" (length (filter isMiracleReveal (S.eventsOf shown))) 1
          Spec.assertEqWith s "and did not on the leg that declined it" (length (filter isMiracleReveal (S.eventsOf hidden))) 0
        _ -> Spec.assertFailure s "fixture should put one Think Twice in alice's hand"
    -- THE DISCRIMINATING CASE, and the turn boundary in the same pair. Two draws
    -- on one board with a Goblin Piker ahead of the Wrath, so the Wrath arrives on
    -- the SECOND draw -- and CR 702.94a's gate must keep the window shut. The
    -- other leg is that board with a turn handoff between the two casts, which
    -- makes the very same draw the new turn's first.
    --
    -- Both legs run the answerer that reveals and casts everything it is offered,
    -- so a leg that does nothing did nothing because the engine never asked.
    Spec.it s "CR 702.94a the second draw of a turn opens no window, and the handoff reopens it" $ do
      island <- S.printingOf s registry "Island"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      thunder <- S.printingOf s registry "Thunderous Wrath"
      let (gs, thinks) = miracleBoard island mountain piker think thunder 2 1
          wrath = S.printingName thunder
          step = resolveCastWith miracleTaken
      case thinks of
        [a, b] -> do
          let afterFirst = step gs a
              sameTurn = step afterFirst b
              nextTurn = step (S.runPure miracleTaken afterFirst Engine.handoffTurn) b
          Spec.assertEqWith s "the first draw took the Piker, not the Wrath" (namedIn wrath Zone.Hand S.alice afterFirst) 0
          Spec.assertEqWith s "drawn second, the Wrath sits in hand" (namedIn wrath Zone.Hand S.alice sameTurn) 1
          Spec.assertEqWith s "and bob takes nothing" (S.lifeOf S.bob sameTurn) (S.lifeOf S.bob gs)
          Spec.assertEqWith s "after the handoff the same draw is the turn's first, so it is cast" (namedIn wrath Zone.Graveyard S.alice nextTurn) 1
          Spec.assertEqWith s "and bob takes 5" (S.lifeOf S.bob nextTurn) (fmap (subtract 5) (S.lifeOf S.bob gs))
        _ -> Spec.assertFailure s "fixture should put two Think Twice in alice's hand"
    -- CR 121.1's other producer of a draw. The window is inside the draw funnel,
    -- so the draw step's turn-based action opens it exactly as a spell's draw
    -- does -- and no Think Twice is cast on this board at all.
    Spec.it s "CR 121.9 the draw step's own draw opens the window" $ do
      island <- S.printingOf s registry "Island"
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      think <- S.printingOf s registry "Think Twice"
      thunder <- S.printingOf s registry "Thunderous Wrath"
      let (base, _) = miracleBoard island mountain piker think thunder 0 0
          gs = base {GameState.phase = Phase.Beginning BeginningStep.DrawStep}
          drawn = S.runPure miracleTaken gs S.drawStep
          after = S.runPure miracleTaken (drawn {GameState.phase = Phase.PrecombatMain}) Engine.priorityLoop
      Spec.assertEqWith s "the draw step drew alice's first card" (Map.lookup S.alice (GameState.drawsThisTurn drawn)) (Just 1)
      Spec.assertEqWith s "the Wrath was cast off it" (namedIn (S.printingName thunder) Zone.Graveyard S.alice after) 1
      Spec.assertEqWith s "and bob takes 5" (S.lifeOf S.bob after) (fmap (subtract 5) (S.lifeOf S.bob gs))

-- alice, in her precombat main phase on turn 2 (so CR 103.8a's skipped draw step
-- is not in play), holding `copies` Think Twice, with two Islands per copy and one
-- Mountain out -- {R} exactly, which is the miracle cost and nowhere near
-- Thunderous Wrath's printed {4}{R}{R}.
--
-- Her library has `ahead` Goblin Pikers on top of one Thunderous Wrath, then ten
-- more Pikers beneath it, so no leg decks her before an assertion runs (CR
-- 104.3c). addLibraryCard puts each new card on top, so the stocking order below
-- is bottom-up.
--
-- Returns the board and the Think Twice ids in hand order.
miracleBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Int ->
  Int ->
  (GameState.GameState, [ObjectId.ObjectId])
miracleBoard island mountain piker think thunder copies ahead =
  let lands = S.landsFor mountain S.alice 1 (S.landsInPlay island (2 * copies))
      addThink (ids, g) _ = let (oid, g') = S.addHandCard think S.alice g in (ids <> [oid], g')
      (thinkIds, withHand) = List.foldl' addThink ([], lands) [1 .. copies]
      pile g n = List.foldl' (\g' _ -> snd (S.addLibraryCard piker S.alice g')) g [1 .. n]
      stocked = pile (snd (S.addLibraryCard thunder S.alice (pile withHand (10 :: Int)))) ahead
   in ( stocked
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.turnNumber = 2
          },
        thinkIds
      )

-- Answer CR 702.94a's two "may"s with the pinned decisions, aim every target at
-- bob, and cast nothing at a player's own timing.
--
-- The two answers are PINNED rather than searched for, so a mutation that broke
-- which question is being asked cannot be repaired by the answerer picking the
-- other one. ChooseAction passes: every cast on these boards is driven by the
-- test calling Cast.castSpell directly, so nothing else can spend alice's
-- Mountain.
-- Both "may"s taken: the answerer the two gate cases run, so a leg where nothing
-- happened is a leg the engine never asked.
miracleTaken :: Prompt.Prompt r -> r
miracleTaken = miracleAnswer OptionalDecision.Exercises OptionalDecision.Exercises

-- Cast one spell under `answer` and let the stack empty. `resolveCastBy`'s shape
-- with the answerer supplied, which is the whole of what these cases vary.
resolveCastWith :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> ObjectId.ObjectId -> GameState.GameState
resolveCastWith answer gs oid = S.runPure answer (S.runPure answer gs (S.cast S.alice oid)) Engine.priorityLoop

miracleAnswer :: OptionalDecision.OptionalDecision -> OptionalDecision.OptionalDecision -> Prompt.Prompt r -> r
miracleAnswer reveal offer p = case p of
  Prompt.OfferedMiracleReveal {} -> reveal
  Prompt.OfferedCast {} -> offer
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter (== Recipient.ToPlayer S.bob) legal) sets
  _ -> S.identityAnswer p

-- How many cards named `name` this player has in that zone.
namedIn :: CardName.CardName -> Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> Int
namedIn name zone pid gs = length (filter ((== Just name) . fmap S.nameOf . flip Game.cardOf gs) (Game.zoneMembers zone pid gs))

-- CR 702.94a's own reveal, told from CR 701.20a's ordinary one by its cause.
isMiracleReveal :: GameEvent.GameEvent -> Bool
isMiracleReveal event = case event of
  GameEvent.Revealed (Revealed.MkRevealed _ _ RevealCause.ForMiracle _) -> True
  _ -> False

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

-- Cast `spell` and narrow every target slot it offers to `victim`, answering
-- everything else as S.identityAnswer does. The cast is pinned to the one card
-- rather than left to S.castAnswer because a padded hand holds other castable
-- cards, and a leg that spent the mana on one of those would never reach it.
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
      answerWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
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
        -- can't be countered" sits on a creature card, so an uncountered
        -- resolution leaves a permanent behind for the rest of the case to
        -- carry, where the instant's resolution ends the board it was cast on.
        -- Both cards are in the pool and both reach this gate the same way --
        -- through Face.counterability, read off the spell on the stack.
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
          let (oolId, g1) = S.addCreature ool S.alice S.threePlayerGame
              (aliceWall, g2) = S.addCreature wall S.alice g1
              (bobWall, g3) = S.addCreature wall S.bob g2
              (carolWall, g4) = S.addCreature wall S.carol g3
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
          let (_, g1) = S.addCreature ool S.alice S.threePlayerGame
              (pikerId, g2) = S.addCreature piker S.bob g1
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

-- The printed rider "This ability triggers only once each turn"
-- (Pawl.Types.TriggerLimit), on top of the trigger event the group above covers.
-- No comprehensive rule states the clause; CR 702.179d is where the rulebook
-- prints it verbatim, and Pawl.Engine.Engine.withinTurnLimit is what spends it.
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
      firedBy oid gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, AbilityTriggered.source record == TriggerSource.OfObject oid]
      board island bearer n =
        let withLands = S.landsFor island S.alice 10 S.threePlayerGame
            addBearer (ids, g) _ = let (oid, g') = S.addCreature bearer S.alice g in (ids <> [oid], g')
            (bearers, withBearers) = List.foldl' addBearer ([], withLands) [1 .. (n :: Int)]
            stock g pid = List.foldl' (\g' _ -> snd (S.addLibraryCard island pid g')) g [1 .. (12 :: Int)]
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
        -- The rider is spent per TURN, and the record it is spent against is
        -- GameState.events, which the handoff clears.
        Spec.it s "the rider re-arms at the turn boundary" $ do
          island <- S.printingOf s registry "Island"
          wizard <- S.printingOf s registry "Whispering Wizard"
          think <- S.printingOf s registry "Think Twice"
          divine <- S.printingOf s registry "Divination"
          skeins <- S.printingOf s registry "Vision Skeins"
          let (_, gs) = board island wizard 1
              spent = threeCasts think divine skeins gs
              -- bob's turn, alice's Islands still tapped from her own: only the
              -- three she never spent pay for this, and Think Twice is an instant
              -- so CR 304.1 lets her cast it on a turn that is not hers.
              handed = S.runPure S.identityAnswer spent Engine.handoffTurn
              (fourth, ready) = S.addHandCard think S.alice (handed {GameState.priority = Just S.alice})
              after = castAndResolve S.alice fourth ready
          Spec.assertEqWith s "one Spirit at the end of alice's turn" (spiritsOf S.alice spent) 1
          Spec.assertEqWith s "and a second on the next turn's first cast" (spiritsOf S.alice after) 2
        -- Where a badly placed record gets it wrong: two bearers, one rider each.
        -- A limit kept per ABILITY rather than per OBJECT would leave one Spirit
        -- here, and a limit kept per CONTROLLER likewise.
        Spec.it s "a second Whispering Wizard spends a rider of its own" $ do
          island <- S.printingOf s registry "Island"
          wizard <- S.printingOf s registry "Whispering Wizard"
          think <- S.printingOf s registry "Think Twice"
          divine <- S.printingOf s registry "Divination"
          skeins <- S.printingOf s registry "Vision Skeins"
          let (bearers, gs) = board island wizard 2
              after = threeCasts think divine skeins gs
          Spec.assertEqWith s "two bearers on the board" (length bearers) 2
          Spec.assertEqWith s "each triggered exactly once" (fmap (`firedBy` after) bearers) [1, 1]
          Spec.assertEqWith s "so two Spirit tokens" (spiritsOf S.alice after) 2
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

-- The rider on ONE of two abilities a single object bears. Both watch the same
-- event, one prints the rider and one does not, and the unlimited one firing must
-- not spend the limited one's turn: CR 113.7 makes them abilities of one source,
-- and CR 603.2 makes each of them an ability that triggers on its own. This is
-- what proves Engine.limitKey keys on the ABILITY and not on its condition.
--
-- WHY A SYNTHETIC. Scryfall o:"triggers only once each turn", 2026-09-03: 143
-- printings, and no face among them opens two trigger clauses the same way, so
-- not one of them has two abilities that could share a key. The multi-trigger
-- cards there pair the rider with a DIFFERENT condition.
--
-- Synthetic Twinned Vigil, {2}{W} Enchantment: "Whenever you cast a spell, you
-- gain 1 life." and "Whenever you cast a spell, if you control a creature, you
-- gain 5 life. This ability triggers only once each turn." Nothing of it is
-- omitted.
--
-- The intervening "if" (CR 603.4) is what separates the two in TIME, which is
-- what the collision needs: the first cast is a CREATURE SPELL, so as it is cast
-- alice controls no creature and only the unlimited ability triggers -- and it is
-- that ability's own log record which used to suppress the limited one for the
-- rest of the turn.
--
-- 1 and 5 rather than two equal gains, so no total below is reachable two ways;
-- twelve library cards a seat, since Think Twice draws.
twinnedVigilSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
twinnedVigilSpec s registry =
  let castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      board island vigil =
        let withLands = S.landsFor island S.alice 10 S.threePlayerGame
            (_, withVigil) = S.addCreature vigil S.alice withLands
            stock g pid = List.foldl' (\g' _ -> snd (S.addLibraryCard island pid g')) g [1 .. (12 :: Int)]
            stocked = List.foldl' stock withVigil [S.alice, S.bob, S.carol]
         in stocked
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
   in Spec.describe s "TriggerLimit" $ do
        Spec.it s "an unlimited sibling ability spends no part of the rider" $ do
          island <- S.printingOf s registry "Island"
          vigil <- S.printingOf s registry "Synthetic Twinned Vigil"
          homunculus <- S.printingOf s registry "Furtive Homunculus"
          think <- S.printingOf s registry "Think Twice"
          let gs = board island vigil
              (creature, g1) = S.addHandCard homunculus S.alice gs
              (firstSpell, g2) = S.addHandCard think S.alice g1
              (secondSpell, g3) = S.addHandCard think S.alice g2
              afterCreature = castAndResolve S.alice creature g3
              afterFirst = castAndResolve S.alice firstSpell afterCreature
              afterSecond = castAndResolve S.alice secondSpell afterFirst
          Spec.assertEqWith s "the limited ability fires on the cast that follows, 21 plus 1 plus 5" (S.lifeOf S.alice afterFirst) (Just 27)
          Spec.assertEqWith s "and its own rider still binds it for the rest of the turn, 27 plus 1" (S.lifeOf S.alice afterSecond) (Just 28)
          -- The preconditions the two assertions above rest on, AFTER them so
          -- neither can absorb a mutation aimed at the rider's key.
          Spec.assertEqWith s "the creature cast fired the unlimited ability alone, 20 plus 1" (S.lifeOf S.alice afterCreature) (Just 21)
          Spec.assertEqWith s "the creature really landed, so the intervening if held for the next cast" (S.countOnBattlefieldByName (S.printingName homunculus) S.alice afterCreature) 1

-- The same CR 601.2i cast, read for WHICH cast of the turn it was --
-- SpellCast.ordinal. The cast-side twin of drawTriggerSpec's Erudite Wizard, and
-- the two conditions answer the same question about different events.
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
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature mountain pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 4 (addLands S.alice 16 S.threePlayerGame)
            (_, withClarion) = S.addCreature clarion S.alice withLands
         in withClarion
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castAndResolve caster oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast caster oid)) Engine.priorityLoop
      -- n copies of Boil in a hand, returned in the order they were added.
      handOf boil pid n gs =
        List.foldl'
          (\(oids, g) _ -> let (oid, g') = S.addHandCard boil pid g in (oids <> [oid], g'))
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
        -- The Filter, applied to the EARLIER casts and not only to the one being
        -- matched: bob's Boil sits between alice's two in the log, so a count of
        -- every cast in it would make alice's second the turn's third and fire
        -- nothing. The pair of boards differ in exactly that cast.
        Spec.it s "CR 109.5 'you cast': an opponent's cast is not counted toward the ordinal" $ do
          mountain <- S.printingOf s registry "Mountain"
          clarion <- S.printingOf s registry "Clarion Spirit"
          boil <- S.printingOf s registry "Boil"
          case handOf boil S.alice 2 (board mountain clarion) of
            ([first, second], withHand) -> do
              let (bobsBoil, gs) = S.addHandCard boil S.bob withHand
                  afterAlice = castAndResolve S.alice first gs
                  afterBob = castAndResolve S.bob bobsBoil afterAlice
                  interleaved = castAndResolve S.alice second afterBob
                  straight = castAndResolve S.alice second afterAlice
              Spec.assertEqWith s "bob's cast alone makes nobody a Spirit" (spiritsOf S.alice afterBob) 0
              Spec.assertEqWith s "alice's second still fires with his cast in between" (spiritsOf S.alice interleaved) 1
              -- The same board without bob's cast, which is the only difference
              -- between the two: it fires either way.
              Spec.assertEqWith s "and fires without it" (spiritsOf S.alice straight) 1
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
          let (guideId, g0) = S.addCreature guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addCreature piker S.alice g0
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
          let (_, g0) = S.addCreature guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addCreature piker S.alice g0
              board = S.tapObject pikerId g1
              stepped = untapStep board
              placed = placeTriggers stepped
          Spec.assertEqWith s "alice's life is untouched" (S.lifeOf S.alice (resolveOne placed)) (Just 20)
          Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack placed)) 0
          Spec.assertEqWith s "though the Piker did untap" (fmap Object.tapped (Game.lookupObject pikerId stepped)) (Just TapState.Untapped)
        -- CR 603.2e's second sentence, the collapse this condition has to
        -- survive: the Guide ENTERS the battlefield untapped, which is not a
        -- transition, so nothing triggers. Through the stack rather than through
        -- S.addCreature, so the real entry funnel runs.
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
          let (guideId, g0) = S.addCreature guide S.alice (Setup.emptyGame S.bothPlayers)
              (pikerId, g1) = S.addCreature piker S.alice g0
              board = S.tapObject pikerId (S.tapObject guideId g1)
              untapOne oid = placeTriggers (S.runPure S.identityAnswer board (Event.untap oid))
          Spec.assertEqWith s "untapping the Guide gains alice 2" (S.lifeOf S.alice (resolveOne (untapOne guideId))) (Just 22)
          Spec.assertEqWith s "untapping the Piker instead gains nothing" (S.lifeOf S.alice (resolveOne (untapOne pikerId))) (Just 20)

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
  let (gnarlbarkId, g1) = S.addCreature gnarlbark S.alice (Setup.emptyGame S.bothPlayers)
      (_, g2) = S.addCreature chronicler S.bob g1
      g3 = if withSolemnity then snd (S.addCreature solemnity S.bob g2) else g2
      g4 = if withOwnWatcher then snd (S.addCreature chronicler S.alice g3) else g3
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
  discardTriggerSpec s registry
  cyclesTriggerSpec s registry
  selfDiscardTriggerSpec s registry
  drawTriggerSpec s registry
  miracleSpec s registry
  controllerAtTriggerSpec s registry
  counterTriggerSpec s registry
  auntieOolSpec s registry
  youngPyromancerSpec s registry
  whisperingWizardSpec s registry
  twinnedVigilSpec s registry
  clarionSpiritSpec s registry
  desolationTwinSpec s registry
  presenceOfTheMasterSpec s registry
  kambalSpec s registry
  brinebornCutthroatSpec s registry
  oreskosSunGuideSpec s registry
  blightChroniclerSpec s registry
