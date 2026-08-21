-- Pawl.Engine.Trigger over the events that are not zone changes: draws,
-- discards, counters placed and removed, life gained and lost, damage, casts,
-- and attack declarations. The machinery is Pawl.TriggerSpec.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.EventTriggerSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Damage as Damage
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Plot as Plot
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.Action as A
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.ControlChanged as ControlChanged
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LifeChange as LifeChange
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

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
          discardBy pid = S.runPure S.identityAnswer gs (Cost.payComponent pid S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
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
          discarded = S.runPure S.identityAnswer gs (Cost.payComponent S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))
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
discardOne answer gs = S.runPure answer gs (Cost.payComponent S.alice S.noSource (CostComponent.DiscardCards (DiscardCards.MkDiscardCards 1 (Filter.Type.And []))))

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
      firedBy oid gs = length [() | GameEvent.AbilityTriggered record <- S.eventsOf gs, AbilityTriggered.source record == oid]
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
      attacksWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
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
              gainsIn gs = [p | GameEvent.LifeGained (LifeChange.MkLifeChange p _) <- S.eventsOf gs]
              after n = S.runPure S.identityAnswer gs0 (Damage.applyDamage [evOf n])
          Spec.assertEqWith s "two damage records the gain" (gainsIn (after 2)) [S.alice]
          Spec.assertEqWith s "zero damage records nothing" (gainsIn (after 0)) []

-- CR 603.10's first sentence, asked of a permanent that never leaves: "objects
-- that exist immediately after an event are checked to see if the event matched
-- any trigger conditions, and continuous effects that exist at that time are used
-- to determine what the trigger conditions are". The abilities that decide are the
-- ones the permanent had AT THE EVENT, not the ones it has when the CR 117.5 scan
-- gets around to looking.
--
-- The two readings need an ability set that MOVES between an event and the scan,
-- with the event first. Synthetic Humbling Draught, {W} Instant, "You gain 2 life.
-- Until end of turn, target creature loses all abilities. You gain 3 life", is that
-- in one resolution: the first CR 119.3 life gain is recorded, the CR 613.1f
-- layer-6 removal lands after it, and the second gain after that -- all before any
-- player could have priority, so the whole thing is one CR 117.5 batch.
--
-- The SECOND gain is what makes the board tell the three readings apart, on the one
-- number every case asserts. Aimed at the Pridemate:
--
--   * abilities as of each event (the rule): the first gain triggers, the second
--     does not -- ONE counter.
--   * abilities as of the scan: neither triggers, the Pridemate having none by then
--     -- NO counter.
--   * abilities as of the batch's first event: both trigger, one sample being
--     stretched over two events that straddle the strip -- TWO counters.
--
-- SYNTHETIC, after searching the printings. Every card that says "loses all
-- abilities" and does anything else in the same resolution strips FIRST -- Day of
-- Black Sun, Patriar's Humiliation, Resolute Rejection, Snakeform, Abigale -- so
-- the ability set has already moved when the event happens and the readings agree.
-- The three printings that order it the other way each need a trigger condition
-- pawl does not have: Merfolk Trickster and The Wondrous Wasp tap first, and
-- nothing here watches a permanent becoming tapped, while Tishana's Tidebinder
-- counters an ability first. Nothing in the CR forbids the card as written; it is
-- Blossoming Calm's shape with Turn to Frog's one layer.
--
-- The pair differs in the TARGET and in nothing else: the same board, the same
-- mana, the same 5 life gained, the same two legal creatures to aim at. Aiming at
-- the Goblin Piker is the control that proves the plumbing -- the Pridemate takes
-- both counters -- and aiming at the Pridemate is the case.
abilitiesWhenTriggeredSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
abilitiesWhenTriggeredSpec s registry =
  let countersOn oid gs = fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid gs)
      -- Pinned to the one recipient rather than searched for among the legal ones:
      -- a searching answerer would find the other creature again once the case
      -- under test broke.
      aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt victimId p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature victimId))) sets
        _ -> S.identityAnswer p
      board = do
        plains <- S.printingOf s registry "Plains"
        pridemate <- S.printingOf s registry "Ajani's Pridemate"
        piker <- S.printingOf s registry "Goblin Piker"
        draught <- S.printingOf s registry "Synthetic Humbling Draught"
        let (mateId, b0) = S.addCreature pridemate S.alice (S.landsInPlay plains 1)
            (pikerId, b1) = S.addCreature piker S.alice b0
        pure (mateId, pikerId, S.handOne draught b1)
      drinkAt victimId (gs, spellId) =
        let cast = snd (Engine.runGamePure (aimAt victimId) gs (S.cast S.alice spellId))
         in snd (Engine.runGamePure (aimAt victimId) cast Engine.priorityLoop)
   in Spec.describe s "CR 603.10 abilities as of the event" $ do
        Spec.it s "CR 603.10 the control: the Draught strips the PIKER and both gains reach the Pridemate" $ do
          (mateId, pikerId, gs) <- board
          let after = drinkAt pikerId gs
          Spec.assertEqWith s "alice gained 2 and then 3" (S.lifeOf S.alice after) (Just 25)
          Spec.assertEqWith s "and her Pridemate, untouched, took a counter for each" (countersOn mateId after) (Just 2)
        Spec.it s "CR 603.10 stripping the PRIDEMATE takes the second gain from it and not the first" $ do
          (mateId, _, gs) <- board
          let after = drinkAt mateId gs
          Spec.assertEqWith s "the same 5 life, the strip changing neither gain" (S.lifeOf S.alice after) (Just 25)
          Spec.assertBool s (null (Projection.triggeredAbilitiesOf mateId after)) "and the Pridemate really has no abilities left"
          Spec.assertEqWith s "exactly one counter: the gain it still had the ability for" (countersOn mateId after) (Just 1)

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
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.MkMoved moved (Projection.project oid gs))] gs))
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
        --
        -- BOB's gain read under the You relation, which is what pins the gainer to
        -- the EVENT rather than to the relation: the arm binds whichever player the
        -- event names, and an arm that reached for the ability's controller instead
        -- would answer alice here.
        Spec.it s "CR 603.2 eventBindings binds the amount the event carries, and the player who gained it" $
          Spec.assertEqWith
            s
            "thatMuch is the gain and thatPlayer is the gainer"
            (Event.eventBindings (TriggerCondition.PlayerGainsLife PlayerRelation.You) (GameEvent.LifeGained (LifeChange.MkLifeChange S.bob 7)))
            (Binding.setTriggerPlayer S.bob (Map.singleton Binding.eventAmount (Binding.toAmount 7)))

-- CR 119.9's event read for its PLAYER, which neither group above can ask for:
-- Ajani's Pridemate and Sanguine Bond both watch under CR 109.5's "you", where
-- the gainer and the ability's controller are one seat.
--
-- False Cure, {B}{B} Instant, "Until end of turn, whenever a player gains life,
-- that player loses 2 life for each 1 life they gained." Three things at once,
-- and the board is built so that each fails on its own:
--
--   * CR 102.1's bare "a player" (PlayerRelation.AnyPlayer), so a gain by
--     somebody who is not the caster fires it. bob's Radiant Fountain is that
--     gain.
--   * CR 603.2's gaining player, bound under Binding.triggerPlayer. THREE seats,
--     because a two-seat board collapses "that player" onto the one opponent --
--     carol sits there so that "an opponent" and "the player who gained" are not
--     the same reading.
--   * CR 603.7b's stated duration, which keeps the entry armed through firing:
--     the second gain, alice's own, is a different seat and a different amount.
--   * CR 603.2c's repeat WITHIN one batch, which the two gains above cannot show
--     because they arrive in batches of one. Centaur Peacemaker, on its own
--     board below, puts every seat's gain in a single batch.
--
-- The doubling is Quantity.Plus of the slot with itself, Pawl.Types.Quantity
-- having no multiply -- exact for "2 life for each 1 life", and what makes the
-- two amounts tell a bound amount from a constant.
falseCureSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
falseCureSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- lifeGainAmountSpec's entry staging: the permanent is already placed, its
      -- Moved event is recorded, and CR 603.6a's scan runs at the next settle.
      entering oid gs =
        let moved = ZoneChange.MkZoneChange oid oid Zone.Stack Zone.Battlefield
         in resolveAll (settle (S.withEvents [GameEvent.Moved (Moved.MkMoved moved (Projection.project oid gs))] gs))
      -- alice casts the Cure off two Swamps on a three-seat board, and everything
      -- else is already on the battlefield: bob's Radiant Fountain (CR 119.3, "you
      -- gain 2 life" for BOB), alice's Soul Warden and a Goblin Piker under carol
      -- for the Warden to see enter.
      armed = do
        swamp <- S.printingOf s registry "Swamp"
        falseCure <- S.printingOf s registry "False Cure"
        fountain <- S.printingOf s registry "Radiant Fountain"
        soulWarden <- S.printingOf s registry "Soul Warden"
        piker <- S.printingOf s registry "Goblin Piker"
        let lands = S.landsFor swamp S.alice 2 S.threePlayerGame
            (fountainId, withFountain) = S.addCreature fountain S.bob lands
            (_, withWarden) = S.addCreature soulWarden S.alice withFountain
            (pikerId, withPiker) = S.addCreature piker S.carol withWarden
            (spellId, withSpell) = S.addHandCard falseCure S.alice withPiker
        pure (fountainId, pikerId, resolveAll (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
      -- The Cure armed on `base` with Centaur Peacemaker, {1}{G}{W} Creature --
      -- Centaur Cleric 3/3, "When this creature enters, each player gains 4
      -- life." ONE resolution records a LifeGained per seat, so the whole board's
      -- gains reach the CR 117.5 settle as one batch. Its OWN minimal board: the
      -- Soul Warden above would see the Peacemaker enter and add a gain that is
      -- not part of the batch under test.
      peacemakerArmed base = do
        swamp <- S.printingOf s registry "Swamp"
        falseCure <- S.printingOf s registry "False Cure"
        peacemaker <- S.printingOf s registry "Centaur Peacemaker"
        let lands = S.landsFor swamp S.alice 2 base
            (peacemakerId, withPeacemaker) = S.addCreature peacemaker S.alice lands
            (spellId, withSpell) = S.addHandCard falseCure S.alice withPeacemaker
        pure (peacemakerId, resolveAll (snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))))
   in Spec.describe s "CR 603.7 False Cure" $ do
        -- The gain that is NOT the caster's. bob gains 2 and loses 4 -- and alice's
        -- and carol's totals are asserted untouched, which is the whole of #826: an
        -- arm that bound CR 109.5's "you" in place of the event's player would show
        -- alice at 16 and bob at 22 on this very board.
        Spec.it s "CR 102.1/603.2 bob gains 2 from his own Radiant Fountain and loses 4" $ do
          (fountainId, _, gs) <- armed
          let after = entering fountainId gs
          Spec.assertEqWith s "bob gained 2 and then lost 4" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "alice, who cast the Cure, is untouched" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and so is carol" (S.lifeOf S.carol after) (Just 20)
        -- The SECOND firing, on a different seat and a different amount: CR 603.7b's
        -- stated duration keeps the entry armed, and the Soul Warden's 1 becomes 2
        -- rather than the 4 above. A doubling that bound a constant fails here; an
        -- entry spent by its first firing fires not at all.
        Spec.it s "CR 603.7b the same entry fires again for alice, whose gain is 1" $ do
          (fountainId, pikerId, gs) <- armed
          let afterBob = entering fountainId gs
              after = entering pikerId afterBob
          Spec.assertEqWith s "alice gained 1 from her Soul Warden and lost 2" (S.lifeOf S.alice after) (Just 19)
          Spec.assertEqWith s "bob is where the first firing left him" (S.lifeOf S.bob after) (Just 18)
          Spec.assertEqWith s "carol gained nothing and lost nothing" (S.lifeOf S.carol after) (Just 20)
        -- The control, through the narrowest path that ends the duration: CR 514.2's
        -- cleanup, which Expiry.dropAtCleanup is. The SAME gain on the SAME board
        -- afterwards costs bob nothing, so the two cases differ in exactly one thing.
        Spec.it s "CR 514.2 the entry is gone after cleanup, so the same gain costs nothing" $ do
          (fountainId, _, gs) <- armed
          let after = entering fountainId (Expiry.dropAtCleanup gs)
          Spec.assertEqWith s "bob gained his 2 and kept it" (S.lifeOf S.bob after) (Just 22)
          Spec.assertEqWith s "alice is untouched either way" (S.lifeOf S.alice after) (Just 20)
        -- CR 603.2c inside ONE batch, which the cases above cannot reach: the
        -- entry's trigger event occurs three times before the settle, and CR
        -- 603.7b's stated duration lifts the one shot, so 603.2c's "it can trigger
        -- repeatedly" applies and every seat pays 8. An entry taking the first
        -- match out of the batch drains alice alone and leaves bob and carol at 24.
        Spec.it s "CR 603.2c three seats gain in one batch, so the entry fires three times" $ do
          (peacemakerId, gs) <- peacemakerArmed S.threePlayerGame
          let after = entering peacemakerId gs
          Spec.assertEqWith s "alice starts at 20" (S.lifeOf S.alice gs) (Just 20)
          Spec.assertEqWith s "bob starts at 20" (S.lifeOf S.bob gs) (Just 20)
          Spec.assertEqWith s "carol starts at 20" (S.lifeOf S.carol gs) (Just 20)
          Spec.assertEqWith s "alice gained 4 and lost 8" (S.lifeOf S.alice after) (Just 16)
          Spec.assertEqWith s "bob gained 4 and lost 8" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "carol gained 4 and lost 8" (S.lifeOf S.carol after) (Just 16)
        -- The other half of the pair, differing in exactly one thing -- how many
        -- occurrences the batch holds. FOUR seats, so the firing count is four and
        -- not the three above: a fixed number of firings, or one per batch, passes
        -- at most one of the two boards. Four rather than two, since two seats
        -- would collapse "that player" onto the one opponent.
        Spec.it s "CR 603.2c a fourth seat in the batch is a fourth firing" $ do
          (peacemakerId, gs) <- peacemakerArmed S.fourPlayerGame
          let after = entering peacemakerId gs
          Spec.assertEqWith s "dave starts at 20" (S.lifeOf S.dave gs) (Just 20)
          Spec.assertEqWith s "alice gained 4 and lost 8" (S.lifeOf S.alice after) (Just 16)
          Spec.assertEqWith s "bob gained 4 and lost 8" (S.lifeOf S.bob after) (Just 16)
          Spec.assertEqWith s "carol gained 4 and lost 8" (S.lifeOf S.carol after) (Just 16)
          Spec.assertEqWith s "dave gained 4 and lost 8" (S.lifeOf S.dave after) (Just 16)
        -- The vacuity guard, not a prover: the same Peacemaker with NO Cure armed
        -- leaves every seat holding its 4 (CR 119.3). Without it a board where
        -- "each player gains 4" quietly gained nobody anything would read as a
        -- passing 16 above, the drains never having happened either.
        Spec.it s "CR 119.3 with no entry armed, each of the three seats keeps its 4" $ do
          peacemaker <- S.printingOf s registry "Centaur Peacemaker"
          let (peacemakerId, gs) = S.addCreature peacemaker S.alice S.threePlayerGame
              after = entering peacemakerId gs
          Spec.assertEqWith s "alice is at 24" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "bob is at 24" (S.lifeOf S.bob after) (Just 24)
          Spec.assertEqWith s "carol is at 24" (S.lifeOf S.carol after) (Just 24)

-- CR 120.3's event read by its RECIPIENT, which no condition could ask for
-- before: every damage arm beside this one watches a permanent DEALING damage.
--
-- Two producers. Ripjaw Raptor, {2}{G}{G} Creature -- Dinosaur 4/5, whose whole
-- text is "Enrage -- Whenever this creature is dealt damage, draw a card", reads
-- no part of the event; Coalhauler Swine, further down, reads its amount. Enrage
-- is an ability word (CR 207.2c) with no rules meaning, so the condition is
-- ordinary and nothing about it reaches Pawl.Types.Keyword.
--
-- What the group has to separate, a board or a pair of boards each:
--
--   * the RECIPIENT, not the damager and not any permanent -- a Hill Giant
--     beside the Raptor, under the same controller, takes the same damage and
--     draws nothing.
--   * the DAMAGE KIND, which this arm does not filter on where its neighbours all
--     do: a noncombat event and a CR 510.2 combat event each draw one.
--   * CR 510.2's simultaneity, which is the reading a naive once-per-batch arm
--     gets wrong: two blockers deal two events and draw TWO cards.
--   * the AMOUNT the event carried, which the Swine's payload reads back as CR
--     120.3's "that much": a pair of boards carrying different numbers, and a
--     second pair separating a per-event read from a per-batch one.
--
-- The observable is asserted BEFORE and AFTER every time -- hand size for the
-- Raptor, life totals for the Swine. "alice holds one card" alone passes on a
-- board she drew for turn on.
enrageSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enrageSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      -- A noncombat event's own path: Damage.applyDamage records the DamageDealt
      -- entries, the settle gathers what they triggered and puts it on the stack
      -- (CR 603.3), and the priority loop resolves it. The narrowest path that
      -- shows the behaviour.
      dealing events gs = resolveAll (settle (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      -- alice's library is stocked, or CR 104.3c decks her before the assertion
      -- runs.
      stock printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      damageOn oid gs = fmap Object.damage (Game.lookupObject oid gs)
   in Spec.describe s "CR 120.3 enrage" $ do
        -- The noncombat half, and the CONTROL as a pair of boards differing in
        -- exactly one thing: which of alice's two creatures the event's recipient
        -- names. bob's Goblin Piker is the source either way, and the Hill Giant's
        -- toughness is 3 so that the control board's creature survives to be
        -- asked about.
        Spec.it s "CR 120.3 a noncombat event on the Raptor draws exactly one card, and one on another creature draws none" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          giant <- S.printingOf s registry "Hill Giant"
          piker <- S.printingOf s registry "Goblin Piker"
          let (raptorId, g1) = S.addCreature raptor S.alice (Setup.emptyGame S.bothPlayers)
              (mineId, g2) = S.addCreature giant S.alice g1
              (theirsId, g3) = S.addCreature piker S.bob g2
              gs = stock piker S.alice 5 g3
              atRaptor = dealing [noncombat theirsId raptorId 1] gs
              atGiant = dealing [noncombat theirsId mineId 1] gs
          Spec.assertEqWith s "alice starts with an empty hand" (S.handSize S.alice gs) 0
          Spec.assertEqWith s "the Raptor's own event drew her one" (S.handSize S.alice atRaptor) 1
          Spec.assertEqWith s "and the damage really landed on the Raptor" (damageOn raptorId atRaptor) (Just 1)
          Spec.assertEqWith s "the same event on her Hill Giant drew nothing" (S.handSize S.alice atGiant) 0
          Spec.assertEqWith s "though that damage landed too" (damageOn mineId atGiant) (Just 1)
        -- CR 510.2's combat damage, through the whole declare-block-deal sequence.
        -- ONE blocker, so exactly one event reaches the Raptor -- which is what
        -- proves the arm does not filter on DamageKind, the noncombat case above
        -- being the other half of that pair.
        Spec.it s "CR 510.2 combat damage fires it too: one blocker, one card" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          let (gs0, mine, _) = S.combatBoardOf [raptor] [piker]
              gs = stock piker S.alice 5 gs0
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Ripjaw Raptor"
            raptorId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "alice started with an empty hand" (S.handSize S.alice gs) 0
              Spec.assertEqWith s "and drew exactly one" (S.handSize S.alice after) 1
              Spec.assertEqWith s "the one blocker's 2 is marked on the Raptor" (damageOn raptorId after) (Just 2)
        -- TWO blockers, so CR 510.2 deals two events simultaneously and
        -- Pawl.Engine.Damage records one DamageDealt each. Two triggers, two cards
        -- -- which is what the Ripjaw Raptor rulings say and what an arm folding the
        -- batch into one firing gets wrong.
        --
        -- The blockers are DIFFERENT printings, 2/1 and 1/1, so the 3 marked on the
        -- Raptor is a number neither event carries alone: a single fabricated event
        -- of the whole batch's damage would be indistinguishable otherwise.
        Spec.it s "CR 510.2 two simultaneous events draw two cards, not one" $ do
          raptor <- S.printingOf s registry "Ripjaw Raptor"
          piker <- S.printingOf s registry "Goblin Piker"
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          let (gs0, mine, _) = S.combatBoardOf [raptor] [piker, sorcerer]
              gs = stock piker S.alice 5 gs0
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Ripjaw Raptor"
            raptorId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "alice started with an empty hand" (S.handSize S.alice gs) 0
              Spec.assertEqWith s "one card per event, so two" (S.handSize S.alice after) 2
              Spec.assertEqWith s "and both events' damage is marked" (damageOn raptorId after) (Just 3)
        -- CR 603.2's amount, the half of CR 120.3's event no payload could read
        -- before. Coalhauler Swine, {4}{R}{R} Creature -- Boar Beast 4/4, whose
        -- whole text is "Whenever this creature is dealt damage, it deals that
        -- much damage to each player."
        --
        -- A PAIR OF BOARDS differing in exactly one thing -- the number the event
        -- carries -- because "each player lost 3" alone is passed by a constant
        -- binding of 3 as happily as by reading the event.
        --
        -- Neither amount coincides with a characteristic a wrong binding could
        -- reach: 3 and 5 are neither the Swine's power nor its toughness (4) nor
        -- the Goblin Piker damager's power (2), so a binding reading a
        -- characteristic instead of the event fails both boards rather than
        -- passing one by luck.
        --
        -- THREE SEATS, because "each player" over two collapses onto "you and an
        -- opponent"; carol blocks the payload that hit only the combatants.
        --
        -- TOUGHNESS 4 is load-bearing: 3 is not lethal (CR 704.5g), so the Swine
        -- is still on the battlefield when its own trigger resolves and the
        -- assertion says nothing about CR 608.2h. The 5-damage board IS lethal, and
        -- is asserted on life totals only -- the number the event carried, which
        -- the binding captured at CR 603.2 rather than at resolution.
        Spec.it s "CR 120.3 the payload reads the amount the event carried" $ do
          swine <- S.printingOf s registry "Coalhauler Swine"
          piker <- S.printingOf s registry "Goblin Piker"
          brigade <- S.printingOf s registry "Foriysian Brigade"
          let (swineId, g1) = S.addCreature swine S.alice S.threePlayerGame
              (mineId, g2) = S.addCreature brigade S.alice g1
              (theirsId, gs) = S.addCreature piker S.bob g2
              lives g = (S.lifeOf S.alice g, S.lifeOf S.bob g, S.lifeOf S.carol g)
              threeAt = dealing [noncombat theirsId swineId 3] gs
              fiveAt = dealing [noncombat theirsId swineId 5] gs
              atOther = dealing [noncombat theirsId mineId 3] gs
          Spec.assertEqWith s "all three seats start at 20" (lives gs) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "CR 120.3a: a 3-damage event costs every player 3" (lives threeAt) (Just 17, Just 17, Just 17)
          Spec.assertEqWith s "and the 3 really landed on the Swine (CR 120.3e)" (damageOn swineId threeAt) (Just 3)
          Spec.assertEqWith s "the same board with a 5-damage event costs every player 5" (lives fiveAt) (Just 15, Just 15, Just 15)
          -- The RECIPIENT control, on the same board and the same amount: the
          -- event pointed at alice's Foriysian Brigade instead, so the life totals
          -- above are this trigger and not damage-adjacent bookkeeping. Its
          -- toughness is 4 too, so it survives to be asked about.
          Spec.assertEqWith s "an event on another of her creatures moves no life total" (lives atOther) (Just 20, Just 20, Just 20)
          Spec.assertEqWith s "though that damage landed too (CR 120.3e)" (damageOn mineId atOther) (Just 3)
        -- CR 510.2 again, now on the AMOUNT rather than on the firing count: two
        -- blockers of 2 and 1 deal two events, and each trigger reads its OWN
        -- event, so the players lose 2 and then 1. The readings this separates,
        -- both of which give 3 per firing and so 6 in total, are "the damage
        -- marked on the recipient" (CR 120.3e has recorded all 3 before either
        -- trigger resolves) and "the batch's total".
        --
        -- Two seats suffice here -- the "each player" reach is the board above's
        -- job, and this one is about which number each of two firings reads.
        Spec.it s "CR 510.2 two simultaneous events are read one at a time, not summed" $ do
          swine <- S.printingOf s registry "Coalhauler Swine"
          piker <- S.printingOf s registry "Goblin Piker"
          sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
          let (gs, mine, _) = S.combatBoardOf [swine] [piker, sorcerer]
          case mine of
            [] -> Spec.assertFailure s "fixture should have given alice a Coalhauler Swine"
            swineId : _ -> do
              let after = resolveAll (S.fightWith S.aggressiveAnswer gs)
              Spec.assertEqWith s "both players started at 20" (S.lifeOf S.alice gs, S.lifeOf S.bob gs) (Just 20, Just 20)
              Spec.assertEqWith s "2 then 1, so 3 each -- not 3 twice" (S.lifeOf S.alice after, S.lifeOf S.bob after) (Just 17, Just 17)
              Spec.assertEqWith s "with both blockers' damage marked on the Swine" (damageOn swineId after) (Just 3)

-- CR 120.1's SOURCE of the damage, the half of the same event enrage above does
-- not read: Belltower Sphinx, {4}{U} Creature -- Sphinx 2/5, "Flying. Whenever a
-- source deals damage to this creature, that source's controller mills that many
-- cards." Its payload is PlayerRef.ControllerOfBound over the slot
-- Pawl.Engine.Event.eventBindings stamps from DamageEvent.source.
--
-- NONCOMBAT events throughout, which is the point: the slot is spelled
-- `combatDamager` for CR 510.2's sake, and a stamp that filtered on DamageKind
-- would leave every board here unmilled.
--
-- THREE SEATS, because "that source's controller" over two collapses onto "the
-- opponent" -- and the pair of boards below is what separates them: the same
-- Sphinx is hit by bob's Piker on one and carol's on the other, so a payload
-- reading a fixed relation mills the wrong seat rather than no seat at all.
--
-- The amounts are 3 and 6, neither of which is the Sphinx's power (2) or
-- toughness (5) nor the Goblin Piker damager's power (2), so a binding reading a
-- characteristic fails both boards rather than passing one by luck. 6 is lethal
-- (CR 704.5g) and the Sphinx dies, which the mill does not care about: CR 603.2
-- captured the amount when the ability triggered.
belltowerSphinxSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
belltowerSphinxSpec s registry =
  let resolveAll gs = snd (Engine.runGamePure S.identityAnswer gs Engine.priorityLoop)
      settle gs = snd (Engine.runGamePure S.identityAnswer gs Engine.settleForPriority)
      dealing events gs = resolveAll (settle (S.runPure S.identityAnswer gs (Damage.applyDamage events)))
      -- Every library is stocked past the largest mill, or the mill runs out of
      -- cards and both readings of the rule produce the same graveyard.
      stock printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      noncombat src target amount = DamageEvent.MkDamageEvent src (Recipient.ToCreature target) amount False False False 0 Nothing DamageKind.Noncombat
      graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      graves g = (graveyardSize S.alice g, graveyardSize S.bob g, graveyardSize S.carol g)
      damageOn oid gs = fmap Object.damage (Game.lookupObject oid gs)
      -- One board for both cases: alice's Sphinx, and an IDENTICAL Goblin Piker
      -- under each of the two opponents, so the pair below differs in the damager
      -- alone.
      board sphinx piker =
        let (sphinxId, g1) = S.addCreature sphinx S.alice S.threePlayerGame
            (bobsId, g2) = S.addCreature piker S.bob g1
            (carolsId, g3) = S.addCreature piker S.carol g2
         in (sphinxId, bobsId, carolsId, stock piker S.alice 9 (stock piker S.bob 9 (stock piker S.carol 9 g3)))
   in Spec.describe s "CR 120.1 that source's controller" $ do
        Spec.it s "CR 120.1 the damager's controller mills, and the count is the event's amount" $ do
          sphinx <- S.printingOf s registry "Belltower Sphinx"
          piker <- S.printingOf s registry "Goblin Piker"
          let (sphinxId, bobsId, _, gs) = board sphinx piker
              byBobThree = dealing [noncombat bobsId sphinxId 3] gs
              byBobSix = dealing [noncombat bobsId sphinxId 6] gs
          Spec.assertEqWith s "every graveyard starts empty" (graves gs) (0, 0, 0)
          Spec.assertEqWith s "bob's Piker deals 3, so bob mills 3 and nobody else mills" (graves byBobThree) (0, 3, 0)
          -- alice's 1 is the dead Sphinx itself (CR 704.5g), not a mill.
          Spec.assertEqWith s "the same board with a 6-damage event mills 6" (graves byBobSix) (1, 6, 0)
          Spec.assertEqWith s "and the damage really landed on the Sphinx" (damageOn sphinxId byBobThree) (Just 3)
          Spec.assertEqWith s "6 was lethal, so the Sphinx is gone -- CR 603.2 had already captured the amount" (damageOn sphinxId byBobSix) Nothing
        -- The SEAT, as a pair of boards differing in exactly one thing: which
        -- opponent's Piker dealt the damage. A payload reading a fixed relation --
        -- PlayerRef.Relative Opponent, or the bearer's controller -- mills the same
        -- seat on both, which two seats could not have told apart.
        Spec.it s "CR 120.1 the seat is the damager's controller, not a fixed relation" $ do
          sphinx <- S.printingOf s registry "Belltower Sphinx"
          piker <- S.printingOf s registry "Goblin Piker"
          let (sphinxId, bobsId, carolsId, gs) = board sphinx piker
              byBob = dealing [noncombat bobsId sphinxId 3] gs
              byCarol = dealing [noncombat carolsId sphinxId 3] gs
          Spec.assertEqWith s "every graveyard starts empty" (graves gs) (0, 0, 0)
          Spec.assertEqWith s "bob's Piker deals it, so bob mills" (graves byBob) (0, 3, 0)
          Spec.assertEqWith s "carol's identical Piker deals the same 3, and now it is CAROL who mills" (graves byCarol) (0, 0, 3)
          Spec.assertEqWith s "the same damage landed on the Sphinx either way" (fmap (damageOn sphinxId) [byBob, byCarol]) [Just 3, Just 3]

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
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
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
            (Event.eventBindings (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent) (GameEvent.LifeLost (LifeChange.MkLifeChange S.bob 7)))
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
            (Event.eventBindings (TriggerCondition.PlayerLosesLife PlayerRelation.You) (GameEvent.LifeLost (LifeChange.MkLifeChange S.alice 3)))
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
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
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

-- CR 102.1's bare "a player" -- every player in the game, the ability's own
-- controller included. The Master of Lake-town, {1}{B}{B} Legendary Creature --
-- Human Advisor 3/2, "Deathtouch. Whenever a player loses life, that player mills
-- that many cards." Mindcrank's sentence above with the relation widened, and the
-- pool's first printing that neither PlayerRelation.You nor PlayerRelation.Opponent
-- states: the two partition the table, so either is observably narrower than what
-- is printed.
--
-- The card's third ability -- "when The Master of Lake-town dies, draw a card for
-- each graveyard with seven or more cards in it" -- is masterOfLaketownDeathSpec
-- below.
--
-- THREE SEATS, for the reason Mindcrank's fixture gives and one more. On a
-- two-seat board "a player" is "you and your opponent", so the widened relation is
-- indistinguishable from a card that spelled both out; and the seat an "each
-- opponent" misreading DROPS is the controller's own, which only a case aimed at
-- her can catch. So the three cases below aim Magister Sphinx's entry trigger at
-- alice, at bob and at carol in turn, and each asserts all three graveyards.
--
-- The three seats start at 23, 19 and 27 against the Sphinx's 10, so the loss is
-- 13, 9 and 17 -- distinct from each other, from every starting total, and from
-- the number of players, so no two readings of the rule produce the same mill.
-- The boards are otherwise identical: the ONE thing the cases differ in is which
-- seat the trigger names.
--
-- Every library is stocked past the deepest mill, so CR 104.3c never decides a
-- case before its assertion runs.
masterOfLaketownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
masterOfLaketownSpec s registry =
  let -- Cast, then settle-and-resolve until the stack runs dry: the Sphinx, its CR
      -- 603.6a entry trigger, and the life-loss trigger that one causes. NOT
      -- Engine.priorityLoop, which advances the turn and clears the event log the
      -- second trigger is scanned against.
      castAndTrigger :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
      castAndTrigger answer spellId gs =
        let step g = S.runPure answer (S.runPure answer g Engine.settleForPriority) Stack.resolveTop
         in List.foldl' (\g _ -> step g) (S.runPure answer gs (S.cast S.alice spellId)) [1 .. 6 :: Int]
      -- FILTERED out of the offered set rather than built from the seat: CR 115.1's
      -- pool of players offers the recipients, and a slot answered with anything
      -- the offer did not contain is dropped at CR 608.2b with no error to read.
      -- Pinned, too -- an answerer that took whatever was legal would find another
      -- seat after a mutation and keep the case green.
      aimedAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
      aimedAt who p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToPlayer who) . snd) sets
        _ -> S.identityAnswer p
      graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      board master sphinx plains island swamp =
        let withLands = S.landsFor swamp S.alice 1 (S.landsFor island S.alice 1 (S.landsFor plains S.alice 5 S.threePlayerGame))
            (_, withMaster) = S.addCreature master S.alice withLands
            stock pid g = List.foldl' (\b _ -> snd (S.addLibraryCard swamp pid b)) g [1 .. (20 :: Int)]
            stocked = List.foldl' (flip stock) withMaster [S.alice, S.bob, S.carol]
            at pid n = Map.adjust (\pl -> pl {Player.life = n}) pid
            lifed = stocked {GameState.players = at S.alice 23 (at S.bob 19 (at S.carol 27 (GameState.players stocked)))}
         in S.handOne sphinx lifed
      -- (label, the seat the Sphinx names, what that seat loses reaching 10).
      cases :: [(String, PlayerId.PlayerId, Int)]
      cases =
        [ ("alice, who controls the Master", S.alice, 13),
          ("bob", S.bob, 9),
          ("carol", S.carol, 17)
        ]
   in Spec.describe s "The Master of Lake-town watches every player's life total"
        . Foldable.for_ cases
        $ \(label, victim, loss) ->
          Spec.it s ("CR 102.1 \"a player\" admits " <> label <> ", who mills exactly what they lost") $ do
            master <- S.printingOf s registry "The Master of Lake-town"
            sphinx <- S.printingOf s registry "Magister Sphinx"
            plains <- S.printingOf s registry "Plains"
            island <- S.printingOf s registry "Island"
            swamp <- S.printingOf s registry "Swamp"
            let (gs, spellId) = board master sphinx plains island swamp
                after = castAndTrigger (aimedAt victim) spellId gs
            Spec.assertEqWith s "the named seat really came down to 10" (S.lifeOf victim after) (Just 10)
            Spec.assertEqWith s "and milled exactly the life they lost" (graveyardSize victim after) loss
            -- The other two seats, asserted every time rather than only where a
            -- reading would touch them: the trigger fires once per recorded loss,
            -- so a mill anywhere else is a second fire nobody asked for.
            Foldable.for_ (List.filter (/= victim) [S.alice, S.bob, S.carol]) $ \bystander -> do
              Spec.assertEqWith s "an untouched seat lost no life" (S.lifeOf bystander after) (S.lifeOf bystander gs)
              Spec.assertEqWith s "and milled nothing" (graveyardSize bystander after) 0

-- CR 700.4 / 404.1 / 608.2h: The Master of Lake-town's THIRD ability -- "when
-- The Master of Lake-town dies, draw a card for each graveyard with seven or
-- more cards in it". masterOfLaketownSpec above is the same card's CR 102.1
-- clause; this is the clause that folds PLAYERS by a fact about a zone they own,
-- which Filter.CardsInGraveyardAtLeast is the first atom to ask and
-- Pawl.Engine.Count.bakePerspective the site that answers it.
--
-- THREE SEATS holding 7, 6 and 8 AT THE MOMENT OF THE COUNT, so the answer is 2
-- and no other reading of the sentence agrees: "more than seven" and "each
-- opponent's graveyard" each answer 1, "six or more" and "every player" answer
-- 3, a sum of cards answers 21, and an atom left unbaked answers 0. The three
-- sizes are pairwise distinct, so a fold reading the wrong seat's graveyard
-- cannot land on the right number by luck.
--
-- The counted sizes are NOT the stocked ones, which is CR 404.1 doing real work:
-- the Bolt is an instant that finished resolving and the Master is a destroyed
-- permanent, so both are already in alice's graveyard when the trigger resolves.
-- She is stocked 5 and counted 7 -- and 7 exactly is what tells >= from >.
--
-- The second case is the paired negative, differing in exactly two stocking
-- numbers: all three graveyards hold 6 at the count. It asserts the trigger
-- reached the stack and resolved, so a zero cannot mean "nothing happened".
--
-- Every library holds twenty against a maximum draw of two, so CR 104.3c never
-- decides a case before its assertions run.
masterOfLaketownDeathSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
masterOfLaketownDeathSpec s registry =
  let graveyardSize pid gs = length (Game.zoneMembers Zone.Graveyard pid gs)
      librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)
      -- FILTERED out of the offered set, for masterOfLaketownSpec's reason: a
      -- hand-built recipient is dropped at CR 608.2b with no error, and an
      -- answerer that took whatever was legal could aim elsewhere after a
      -- mutation and keep the case green.
      aimedAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimedAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature oid) . snd) sets
        _ -> S.identityAnswer p
      board master mountain swamp bolt aliceBin carolBin =
        let withLand = S.landsFor mountain S.alice 1 S.threePlayerGame
            (masterId, withMaster) = S.addCreature master S.alice withLand
            stockLib pid g = List.foldl' (\b _ -> snd (S.addLibraryCard swamp pid b)) g [1 .. (20 :: Int)]
            stocked = List.foldl' (flip stockLib) withMaster [S.alice, S.bob, S.carol]
            bin pid n g = List.foldl' (\b _ -> snd (S.addGraveyardCard swamp pid b)) g [1 .. (n :: Int)]
            (gs, spellId) = S.handOne bolt (bin S.carol carolBin (bin S.bob 6 (bin S.alice aliceBin stocked)))
         in (masterId, gs, spellId)
      -- diesTriggerSpec's sequence: cast the Bolt, resolve it onto the 3/2,
      -- settle -- CR 704.5g destroys the Master and the CR 117.5 settle's own
      -- scan gathers the dies trigger -- then resolve that trigger.
      kill (masterId, gs, spellId) =
        let cast = S.runPure (aimedAt masterId) gs (S.cast S.alice spellId)
            damaged = S.runPure (aimedAt masterId) cast Stack.resolveTop
            settled = S.runPure (aimedAt masterId) damaged Engine.settleForPriority
         in (settled, S.runPure (aimedAt masterId) settled Stack.resolveTop)
      printings = do
        master <- S.printingOf s registry "The Master of Lake-town"
        mountain <- S.printingOf s registry "Mountain"
        swamp <- S.printingOf s registry "Swamp"
        bolt <- S.printingOf s registry "Lightning Bolt"
        pure (master, mountain, swamp, bolt)
   in Spec.describe s "The Master of Lake-town counts the graveyards as it dies" $ do
        Spec.it s "CR 404.1 two of the three graveyards reach seven, so alice draws two" $ do
          (master, mountain, swamp, bolt) <- printings
          let (settled, after) = kill (board master mountain swamp bolt 5 8)
          -- The board the count actually sees, pinned so a later edit cannot make
          -- two readings agree by accident.
          Spec.assertEqWith s "alice's graveyard: five stocked, the Bolt, the Master" (graveyardSize S.alice settled) 7
          Spec.assertEqWith s "bob's stays one short" (graveyardSize S.bob settled) 6
          Spec.assertEqWith s "carol's is over" (graveyardSize S.carol settled) 8
          Spec.assertEqWith s "the dies trigger reached the stack in that settle" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "so she drew two: her own seven and carol's eight, not bob's six" (S.handSize S.alice after) 2
          Spec.assertEqWith s "off her own library" (librarySize S.alice after) 18
          Spec.assertEqWith s "everything resolved" (GameState.stack after) []
          -- The clause draws its CONTROLLER cards, however many graveyards it
          -- counted -- "for each graveyard" is the number, not the drawer.
          Spec.assertEqWith s "bob drew nothing" (S.handSize S.bob after) 0
          Spec.assertEqWith s "and carol nothing" (S.handSize S.carol after) 0
          Spec.assertEqWith s "bob's library is whole" (librarySize S.bob after) 20
          Spec.assertEqWith s "and carol's" (librarySize S.carol after) 20
        Spec.it s "CR 404.1 no graveyard reaches seven, so she draws nothing" $ do
          (master, mountain, swamp, bolt) <- printings
          let (settled, after) = kill (board master mountain swamp bolt 4 6)
          Spec.assertEqWith s "alice's graveyard: four stocked, the Bolt, the Master" (graveyardSize S.alice settled) 6
          Spec.assertEqWith s "bob's" (graveyardSize S.bob settled) 6
          Spec.assertEqWith s "carol's" (graveyardSize S.carol settled) 6
          Spec.assertEqWith s "the dies trigger still reached the stack" (length (GameState.stack settled)) 1
          Spec.assertEqWith s "and resolved" (GameState.stack after) []
          Spec.assertEqWith s "drawing nothing" (S.handSize S.alice after) 0
          Spec.assertEqWith s "and touching no library" (librarySize S.alice after) 20

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
-- Pawl.ZoneTriggerSpec's permanentDiesSpec is where the counters are HANDED
-- OUT, with Meren of Clan Nel Toth. Nothing counted them until this card: an experience counter is
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
      aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
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
          let staged = S.withEvents [GameEvent.StepBegan (StepBegan.MkStepBegan (Phase.Combat CombatStep.BeginningOfCombat) S.alice)] (atBeginningOfCombat S.alice board)
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
      aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
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
        -- Gatherer, 2016-08-23, on this very card: "Abilities that trigger
        -- whenever you 'become the monarch' trigger only if you aren't already
        -- the monarch. For example, if you are already the monarch as Custodi
        -- Lich enters the battlefield, its last ability won't trigger." So a
        -- crowning of the player who already holds the crown is not an event at
        -- all, and Monarch.crown records nothing for it -- which is also what
        -- keeps this reading and CR 725's exile watch (Palace Jailer's "until an
        -- opponent becomes the monarch") answering the same question the same
        -- way.
        --
        -- The case above is the exact paired control: same card, same seats, same
        -- answerer, and the one difference is who holds the crown as the Lich
        -- enters.
        Spec.it s "CR 725.3 a player who is ALREADY the monarch does not become the monarch, so the Lich's edict stays silent" $ do
          custodiLich <- S.printingOf s registry "Custodi Lich"
          piker <- S.printingOf s registry "Goblin Piker"
          birdMaiden <- S.printingOf s registry "Bird Maiden"
          bogWraith <- S.printingOf s registry "Bog Wraith"
          let base = bystanders piker birdMaiden bogWraith (S.withMonarch S.alice (Setup.emptyGame S.threePlayers))
              (lich, gs) = S.entersWithTrigger custodiLich S.alice base
              after = resolveAll (targetsPlayer S.bob) gs
          Spec.assertEqWith s "alice was the monarch before the Lich entered" (GameState.monarch gs) (Just S.alice)
          Spec.assertEqWith s "CR 701.21a bob, whom the edict would have targeted, kept both of his" (S.creaturesInPlay S.bob after) 2
          Spec.assertEqWith s "carol kept hers" (S.creaturesInPlay S.carol after) 1
          Spec.assertEqWith s "alice still holds the crown, so the entry trigger did resolve" (GameState.monarch after) (Just S.alice)
          Spec.assertBool s (notElem (GameEvent.BecameMonarch S.alice) (S.eventsOf after)) "and recorded no crowning, because nobody became the monarch"
          Spec.assertBool s (S.onBattlefield lich after) "the Lich itself is on the battlefield"
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
    Spec.assertBool s (elem (GameEvent.ControlChanged (ControlChanged.MkControlChanged bobPiker S.alice S.bob)) (S.eventsOf afterCleanup)) "Engine.sampleControl minted CR 603.2's event for the reversion"
  where
    -- Narrows every target slot to one object, `aimedCast`'s filter without its cast
    -- pinning: the board holds two stealable creatures on purpose, so the engine's
    -- first offer is not the one either leg means. Filtering the OFFERED set rather
    -- than naming a Recipient keeps the answer in whatever shape the slot offered.
    aimAtVictim :: ObjectId.ObjectId -> Prompt.Prompt r -> r
    aimAtVictim oid p = case p of
      Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, legal) -> Set.filter ((== Just oid) . Recipient.objectOf) legal) sets
      _ -> S.identityAnswer p

-- Matoya, Archon Elder {2}{U} Legendary Creature -- Human Warlock 1/4, "Whenever
-- you scry or surveil, draw a card" -- CR 603.1b's AnyOf over
-- TriggerCondition.PlayerScries and TriggerCondition.PlayerSurveils, so one card
-- proves both of CR 701.22d and CR 701.25d.
--
-- The two firing sources are DIFFERENT cards already in the pool -- Crystal
-- Ball's "{1}, {T}: Scry 2" and Curate's "Surveil 2. Draw a card." -- which is
-- what keeps the two keyword actions apart: a condition that folded them would
-- fire on the board its own half never touched, and each group below has the
-- other card nowhere near it.
--
-- HAND SIZE is the reading throughout, and always against a PAIRED board that
-- differs only in whether Matoya is on the battlefield. Curate draws a card of
-- its own, so an absolute number would prove nothing about the trigger.
matoyaTriggerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
matoyaTriggerSpec s registry =
  let -- alice's board: four Islands, a Crystal Ball, `stock` cards on her
      -- library (top-first Goblin Piker then Bird Maiden), and Matoya only when
      -- asked for. Her hand starts EMPTY, so every hand card below was drawn.
      scryBoardFor withMatoya stock = do
        island <- S.printingOf s registry "Island"
        crystalBall <- S.printingOf s registry "Crystal Ball"
        matoya <- S.printingOf s registry "Matoya, Archon Elder"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        let (ballId, placed) = S.addCreature crystalBall S.alice (S.landsInPlay island 4)
            watched = if withMatoya then snd (S.addCreature matoya S.alice placed) else placed
            deal g p = snd (S.addLibraryCard p S.alice g)
            stocked = List.foldl' deal watched (reverse (take stock [piker, maiden]))
        pure (ballId, stocked {GameState.priority = Just S.alice})
      -- Activate the Ball and settle: the ability resolves, the scry happens and
      -- any trigger it raised is placed and resolved in the same round. A board
      -- offering any other number of abilities activates none, which fails every
      -- assertion rather than passing one for a reason the case did not choose.
      runBall who ballId gs = case Activate.abilitiesFor ballId gs of
        [ability] ->
          let activated = S.runPure keepAll gs (Activate.activateAbility who ballId ability)
           in S.runPure keepAll activated Engine.priorityLoop
        _ -> gs
      -- Keeps every looked-at card on top, for both keyword actions. Pinned
      -- rather than derived: what this group reads is the TRIGGER, and an
      -- answerer that moved cards about would let a graveyard or a library order
      -- stand in for the draw.
      keepAll :: Prompt.Prompt r -> r
      keepAll p = case p of
        Prompt.ChooseScry _ _ looked -> ([], looked)
        Prompt.ChooseSurveil _ _ looked -> ([], looked)
        _ -> S.identityAnswer p
      -- alice's board for the surveil half: two Islands, Curate in hand, four
      -- cards on her library, Matoya only when asked for.
      surveilBoardFor withMatoya = do
        island <- S.printingOf s registry "Island"
        curate <- S.printingOf s registry "Curate"
        matoya <- S.printingOf s registry "Matoya, Archon Elder"
        piker <- S.printingOf s registry "Goblin Piker"
        maiden <- S.printingOf s registry "Bird Maiden"
        mountain <- S.printingOf s registry "Mountain"
        forest <- S.printingOf s registry "Forest"
        let watched =
              if withMatoya
                then snd (S.addCreature matoya S.alice (S.landsInPlay island 2))
                else S.landsInPlay island 2
            deal g p = snd (S.addLibraryCard p S.alice g)
            stocked = List.foldl' deal watched [forest, mountain, maiden, piker]
            (board, spellId) = S.handOne curate stocked
        pure (spellId, board {GameState.priority = Just S.alice})
      runCurate spellId gs =
        let cast = S.runPure keepAll gs (S.cast S.alice spellId)
         in S.runPure keepAll cast Engine.priorityLoop
   in Spec.describe s "MatoyaKeywordActionTrigger" $ do
        -- CR 701.22d, the whole card on the scry side. The pair differs in
        -- Matoya and in nothing else, so the one extra card in hand is the
        -- trigger and cannot be Crystal Ball's doing -- rule 701.22a moves no
        -- card out of the library at all.
        Spec.it s "CR 701.22d Crystal Ball's scry draws Matoya's card" $ do
          (ballId, board) <- scryBoardFor True 2
          (bareBall, bare) <- scryBoardFor False 2
          let after = runBall S.alice ballId board
              baseline = runBall S.alice bareBall bare
          Spec.assertEqWith s "alice's hand started empty" (S.handSize S.alice board) 0
          Spec.assertBool s (elem (GameEvent.Scried S.alice) (S.eventsOf after)) "CR 701.22d the scry recorded its event"
          Spec.assertEqWith s "Matoya drew her one card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "and without Matoya the same scry draws nothing" (S.handSize S.alice baseline) 0
          Spec.assertEqWith s "the stack is empty, so the trigger really resolved" (GameState.stack after) []
        -- CR 701.22d's "even if some or all of those actions were impossible",
        -- and the case that discriminates WHERE the event is recorded: a library
        -- of exactly one card gives scry 2 nothing to decide -- top and bottom
        -- are one position -- so Resolve.scryOne asks no question and reorders
        -- nothing. The scry happened all the same, and Matoya draws that card.
        --
        -- Recording the event inside scryOne's `decided` guard passes every
        -- assertion in the case above and fails this one.
        Spec.it s "CR 701.22d a scry with nothing to decide still draws Matoya's card" $ do
          (ballId, board) <- scryBoardFor True 1
          (bareBall, bare) <- scryBoardFor False 1
          let after = runBall S.alice ballId board
              baseline = runBall S.alice bareBall bare
          Spec.assertBool s (elem (GameEvent.Scried S.alice) (S.eventsOf after)) "CR 701.22d the scry is still an event"
          Spec.assertEqWith s "Matoya drew the lone card" (S.handSize S.alice after) 1
          Spec.assertEqWith s "so alice's library is empty" (length (Game.zoneMembers Zone.Library S.alice after)) 0
          Spec.assertEqWith s "and without Matoya nothing was drawn" (S.handSize S.alice baseline) 0
          Spec.assertEqWith s "the card stayed on the library instead" (length (Game.zoneMembers Zone.Library S.alice baseline)) 1
        -- CR 603.3a / 109.5: the relation is read against the ABILITY'S
        -- CONTROLLER, so an opponent's scry is silence. The same Crystal Ball
        -- activation as the first case, moved one seat over and nothing else.
        Spec.it s "CR 109.5 bob's scry does not draw for alice's Matoya" $ do
          island <- S.printingOf s registry "Island"
          crystalBall <- S.printingOf s registry "Crystal Ball"
          matoya <- S.printingOf s registry "Matoya, Archon Elder"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (_, withMatoya) = S.addCreature matoya S.alice (S.landsInPlay island 4)
              lands = S.landsFor island S.bob 4 withMatoya
              (ballId, placed) = S.addCreature crystalBall S.bob lands
              deal who g p = snd (S.addLibraryCard p who g)
              -- ALICE's library is stocked too, and that is not decoration: an
              -- inverted relation fires Matoya here, and a draw off an empty
              -- library (CR 121.4) moves no card -- so without these two the
              -- assertion below would pass for a reason this case did not choose.
              stocked = List.foldl' (deal S.alice) (List.foldl' (deal S.bob) placed [maiden, piker]) [maiden, piker]
              board = stocked {GameState.priority = Just S.bob}
              after = runBall S.bob ballId board
          Spec.assertBool s (elem (GameEvent.Scried S.bob) (S.eventsOf after)) "bob really scried, so there was an event to match"
          Spec.assertEqWith s "alice, whose Matoya it is, drew nothing" (S.handSize S.alice after) 0
          Spec.assertEqWith s "and bob drew nothing either, his scry moving no card out of his library" (S.handSize S.bob after) 0
        -- CR 701.25d, the whole card on the surveil side. Curate draws a card
        -- itself, which is exactly why the baseline board is here: two cards in
        -- hand against one is the trigger.
        Spec.it s "CR 701.25d Curate's surveil draws Matoya's card on top of its own" $ do
          (spellId, board) <- surveilBoardFor True
          (bareSpell, bare) <- surveilBoardFor False
          let after = runCurate spellId board
              baseline = runCurate bareSpell bare
          Spec.assertBool s (elem (GameEvent.Surveiled S.alice) (S.eventsOf after)) "CR 701.25d the surveil recorded its event"
          Spec.assertBool s (notElem (GameEvent.Scried S.alice) (S.eventsOf after)) "and a surveil is not a scry"
          Spec.assertEqWith s "Curate's draw plus Matoya's" (S.handSize S.alice after) 2
          Spec.assertEqWith s "against Curate's alone" (S.handSize S.alice baseline) 1
          -- The answerer kept both looked-at cards, so nothing but Curate itself
          -- is in the graveyard: this is #1342's own requirement that a surveil
          -- which binned NOTHING still fires, and the assertion a trigger built
          -- on CR 701.25a's zone changes would fail.
          Spec.assertEqWith s "and nothing was binned, so the trigger is not counting cards moved" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1

-- Aloe Alchemist {1}{G} Creature -- Plant Warlock 3/2, "Trample; When this card
-- becomes plotted, target creature gets +3/+2 and gains trample until end of
-- turn; Plot {1}{G}" -- the pool's producer for TriggerCondition
-- SelfBecomesPlotted (CR 702.170e).
--
-- The one condition in the pool whose bearer is in EXILE when it fires: CR
-- 702.170b's special action exiles the card as it becomes plotted, so
-- Event.zonesTriggeredFrom has to answer Zone.Exile for it and Event.eventTriggers
-- finds the bearer through its standing exile scan.
--
-- Distinct power/toughness on the two creatures (Goblin Piker 2/1, Bird Maiden
-- 1/2) so +3/+2 cannot be read off the wrong one, and the Maiden is bob's, so a
-- payload that hit every creature is visible.
aloeAlchemistSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aloeAlchemistSpec s registry =
  let aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      aimAt oid p = case p of
        Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just oid) . Recipient.objectOf) sets
        _ -> S.identityAnswer p
      sorcerySpeed gs =
        gs
          { GameState.activePlayer = S.alice,
            GameState.phase = Phase.PrecombatMain,
            GameState.priority = Just S.alice
          }
   in Spec.describe s "AloeAlchemistPlotTrigger" $ do
        -- The whole card: alice takes CR 116.2k's special action, the card lands
        -- in exile as a plotted card, and the ability printed on it fires from
        -- there.
        Spec.it s "CR 702.170e plotting Aloe Alchemist pumps the targeted creature" $ do
          forest <- S.printingOf s registry "Forest"
          aloe <- S.printingOf s registry "Aloe Alchemist"
          piker <- S.printingOf s registry "Goblin Piker"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay forest 2)
              (maidenId, g2) = S.addCreature maiden S.bob g1
              (aloeId, g3) = S.addHandCard aloe S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (Plot.plot S.alice aloeId)
              after = S.runPure (aimAt pikerId) plotted Engine.priorityLoop
          Spec.assertEqWith s "the Piker started 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
          Spec.assertBool s (any isPlotted (S.eventsOf after)) "CR 702.170a the plot recorded its event"
          Spec.assertEqWith s "CR 702.170e the targeted Piker is 5/3" (S.powerToughnessOf pikerId after) (Just (5, 3))
          Spec.assertEqWith s "bob's untargeted Maiden is untouched" (S.powerToughnessOf maidenId after) (Just (1, 2))
          Spec.assertEqWith s "the card is in exile" (length (GameState.exile after)) 1
          Spec.assertEqWith s "and the stack is empty, so the trigger resolved" (GameState.stack after) []
        -- The DISCRIMINATING negative: a plot event that names a DIFFERENT card.
        -- Aloe Alchemist sits in exile the whole time -- so
        -- Event.eventTriggers' exile scan really offers it, and the only thing
        -- that keeps it quiet is the id on the event.
        --
        -- Djinn of Fool's Fall {3}{U} is the pool's other plot card and prints no
        -- such trigger, which is what makes it the control.
        Spec.it s "CR 702.170e plotting another card does not fire an exiled Aloe Alchemist" $ do
          island <- S.printingOf s registry "Island"
          aloe <- S.printingOf s registry "Aloe Alchemist"
          djinn <- S.printingOf s registry "Djinn of Fool's Fall"
          piker <- S.printingOf s registry "Goblin Piker"
          let (pikerId, g1) = S.addCreature piker S.alice (S.landsInPlay island 4)
              (_, g2) = S.addExiledCard aloe S.alice g1
              (djinnId, g3) = S.addHandCard djinn S.alice g2
              gs = sorcerySpeed g3
              plotted = S.runPure (aimAt pikerId) gs (Plot.plot S.alice djinnId)
              after = S.runPure (aimAt pikerId) plotted Engine.priorityLoop
          Spec.assertBool s (any isPlotted (S.eventsOf after)) "the Djinn really became plotted, so there was an event to match"
          Spec.assertEqWith s "both cards are in exile, so the Alchemist was there to be offered" (length (GameState.exile after)) 2
          Spec.assertEqWith s "and the Piker is still 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
          Spec.assertEqWith s "with nothing waiting on the stack" (GameState.stack after) []

-- Whether an event is CR 702.170a's plot, whichever card it names. The id is
-- CR 400.7's exile incarnation, which no fixture can predict.
isPlotted :: GameEvent.GameEvent -> Bool
isPlotted event = case event of
  GameEvent.Plotted _ -> True
  _ -> False

-- Wildgrowth Walker {1}{G} Creature -- Elemental 1/3, "Whenever a creature you
-- control explores, put a +1/+1 counter on this creature and you gain 3 life" --
-- the pool's producer for TriggerCondition.PermanentExplores (CR 701.44b).
--
-- Merfolk Branchwalker {1}{G} 2/1, "When this creature enters, it explores", is
-- the firing source and was already in the pool. It takes a +1/+1 counter of its
-- own on the nonland branch, so BOTH creatures are read in every case: a payload
-- that grew the explorer rather than the watcher is otherwise invisible.
--
-- Three seats are not needed and two are: what the Filter says is "you control",
-- and the paired board moves the Branchwalker from alice to bob and changes
-- nothing else.
wildgrowthWalkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
wildgrowthWalkerSpec s registry =
  let -- alice always controls the Walker; `explorer` controls the Branchwalker
      -- and owns the stocked library, since CR 701.44a reveals off the
      -- exploring permanent's controller's library.
      board explorer deck = do
        walker <- S.printingOf s registry "Wildgrowth Walker"
        branchwalker <- S.printingOf s registry "Merfolk Branchwalker"
        printings <- mapM (S.printingOf s registry) deck
        let (walkerId, g1) = S.addCreature walker S.alice (Setup.emptyGame S.bothPlayers)
            deal g p = snd (S.addLibraryCard p explorer g)
            stocked = List.foldl' deal g1 (reverse printings)
            (branchId, g2) = S.entersWithTrigger branchwalker explorer stocked
        pure (walkerId, branchId, g2)
      -- Bins the revealed card, so the explore's own zone change happens too --
      -- which is what keeps this trigger from being read off a graveyard
      -- arrival by accident.
      binIt :: Prompt.Prompt r -> r
      binIt p = case p of
        Prompt.ChooseExplore {} -> OptionalDecision.Exercises
        _ -> S.identityAnswer p
      settle gs = S.runPure binIt gs Engine.priorityLoop
   in Spec.describe s "WildgrowthWalkerExploreTrigger" $ do
        -- The whole card. The Branchwalker's own +1/+1 counter and the Walker's
        -- are read separately, so "a counter went somewhere" cannot pass for
        -- "the counter went on the Walker".
        Spec.it s "CR 701.44b a creature alice controls exploring grows her Walker" $ do
          (walkerId, branchId, gs) <- board S.alice ["Goblin Piker", "Bird Maiden"]
          let after = settle gs
          Spec.assertEqWith s "the Walker started 1/3" (S.powerToughnessOf walkerId gs) (Just (1, 3))
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "CR 701.44b the explore recorded its event"
          Spec.assertEqWith s "the Walker took its +1/+1 counter" (S.powerToughnessOf walkerId after) (Just (2, 4))
          Spec.assertEqWith s "and the Branchwalker took its own, which is a different counter" (S.powerToughnessOf branchId after) (Just (3, 2))
          Spec.assertEqWith s "alice gained 3" (S.lifeOf S.alice after) (Just 23)
          Spec.assertEqWith s "bob gained none" (S.lifeOf S.bob after) (Just 20)
          Spec.assertEqWith s "the stack is empty, so the trigger resolved" (GameState.stack after) []
        -- The Filter's own half, CR 109.5's "you control": the same board with
        -- the Branchwalker one seat over. It still explores -- its counter says
        -- so -- and alice's Walker stays put.
        Spec.it s "CR 109.5 bob's creature exploring does not grow alice's Walker" $ do
          (walkerId, branchId, gs) <- board S.bob ["Goblin Piker", "Bird Maiden"]
          let after = settle gs
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "bob's Branchwalker really explored"
          Spec.assertEqWith s "so it took its own counter" (S.powerToughnessOf branchId after) (Just (3, 2))
          Spec.assertEqWith s "but alice's Walker is still 1/3" (S.powerToughnessOf walkerId after) (Just (1, 3))
          Spec.assertEqWith s "and alice gained no life" (S.lifeOf S.alice after) (Just 20)
        -- CR 701.44b's "even if some or all of those actions were impossible":
        -- an empty library reveals nothing, so nothing is a land card and
        -- nothing is binned. The permanent explored all the same.
        Spec.it s "CR 701.44b an explore off an empty library still grows the Walker" $ do
          (walkerId, branchId, gs) <- board S.alice []
          let after = settle gs
          Spec.assertBool s (elem (GameEvent.Explored branchId) (S.eventsOf after)) "the explore is still an event"
          Spec.assertEqWith s "the Walker grew" (S.powerToughnessOf walkerId after) (Just (2, 4))
          Spec.assertEqWith s "alice gained 3" (S.lifeOf S.alice after) (Just 23)
          Spec.assertEqWith s "and nothing was binned" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 0

-- CR 701.3a's attachment event, read from the HOST's side by
-- TriggerCondition.SelfBecomesAttachedBy.
--
-- Bramble Elemental, {3}{G}{G} Creature -- Elemental 4/4, "Whenever an Aura
-- becomes attached to this creature, create two 1/1 green Saproling creature
-- tokens."
--
-- TWO emit sites, and a leg apiece, because the rules reach the same trigger by
-- two roads: CR 608.3c puts a resolving Aura spell onto the battlefield already
-- attached (Pawl.Engine.Event's zone-change funnel writes the seed), and CR
-- 701.3a moves a permanent that is already there (Event.attach). Deleting either
-- emit leaves the other leg green, which is why neither stands alone.
--
-- NOTHING here goes through Pawl.Support.attach, which writes Object.attachedTo
-- directly and records no event: a leg built on it would read zero before and
-- zero after and could not tell this engine from one that had never heard of the
-- rule.
-- Tokens only, and by SUBTYPE: the Elemental's own board is full of creatures,
-- and counting them would drift the moment a fixture changed.
saprolingsOf :: PlayerId.PlayerId -> GameState.GameState -> Int
saprolingsOf pid gs =
  length
    ( filter
        (\oid -> Set.member Subtype.Saproling (Projection.subtypesOf oid gs) && Projection.controllerOf oid gs == Just pid)
        (S.tokensOf gs)
    )

-- Answers every target slot with the offered recipients that name one object.
--
-- FILTERS the offered set rather than building a Recipient, AuraSpec's
-- aimAtOffered posture: Pacifism's enchant slot pools creatures, and a
-- hand-built recipient of another shape is dropped by CR 608.2b's re-read at
-- resolution with no error to see.
aimAtOffered :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtOffered oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- Both of an attach-moving ability's prompts: its target slot, and CR 701.3a's
-- destination choice.
moveOnto :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
moveOnto subject dest p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just subject) . Recipient.objectOf) . snd) sets
  Prompt.ChooseAttachment {} -> dest
  _ -> S.identityAnswer p

-- The CR 117.5 boundary scans for triggers, then the one it placed resolves.
-- Narrower than the priority loop, which would sweep the rest of the board too.
fireTriggers :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
fireTriggers answer gs =
  let placed = S.runPure answer gs Engine.settleForPriority
   in S.runPure answer placed Stack.resolveTop

-- What is attached to `host`, by whichever tag the attaching permanent's own
-- rules text names it -- Pawl.AuraSpec's attachedTo.
attachmentsOn :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
attachmentsOn host gs =
  filter
    (\oid -> (Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf) == Just host)
    (Set.toList (GameState.battlefield gs))

firstActivatedOf :: Printing.Printing -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card)
firstActivatedOf printing = case Face.activatedAbilities (S.combinedFace printing) of
  ability : _ -> Just ability
  [] -> Nothing

brambleElementalSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
brambleElementalSpec s registry =
  Spec.describe s "CR 701.3a a trigger on becoming attached" $ do
    -- CR 608.3c: the Aura spell resolves and is put onto the battlefield
    -- attached to what it targeted. The attachment is written inside the zone
    -- change, on the CR 400.7 incarnation, so this leg is the entry emit and
    -- reaches Event.attach not at all.
    Spec.it s "CR 608.3c whole card: casting Pacifism on the Elemental creates two Saprolings" $ do
      plains <- S.printingOf s registry "Plains"
      bramble <- S.printingOf s registry "Bramble Elemental"
      pacifism <- S.printingOf s registry "Pacifism"
      let (brambleId, board) = S.addCreature bramble S.alice (S.landsInPlay plains 3)
          (armed, auraSpell) = S.handOne pacifism board
          cast = S.runPure (aimAtOffered brambleId) armed (S.cast S.alice auraSpell)
          entered = S.runPure (aimAtOffered brambleId) cast Stack.resolveTop
          after = fireTriggers (aimAtOffered brambleId) entered
      Spec.assertEqWith s "CR 603.2 two Saprolings once the trigger resolves" (saprolingsOf S.alice after) 2
      Spec.assertEqWith s "and none on the board the Aura was cast from" (saprolingsOf S.alice armed) 0
      -- The precondition the count rests on: an Aura that never landed would
      -- make zero the right answer for the wrong reason.
      Spec.assertEqWith s "the Aura is attached to the Elemental" (length (attachmentsOn brambleId entered)) 1
    -- CR 701.3a's other road: an Aura already on the battlefield MOVES.
    -- Crown of the Ages, "{4}, {T}: Attach target Aura attached to a creature
    -- to another creature" -- Unholy Strength enters on a Piker, where
    -- nothing triggers, and is then moved onto the Elemental.
    --
    -- The Piker half is the pair's other board, one object different: same
    -- Aura, same cast, same resolution, a host that is not the watcher.
    Spec.it s "CR 701.3a whole card: Crown of the Ages moving an Aura onto the Elemental creates two Saprolings" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      bramble <- S.printingOf s registry "Bramble Elemental"
      unholyStrength <- S.printingOf s registry "Unholy Strength"
      crown <- S.printingOf s registry "Crown of the Ages"
      let (pikerId, base1) = S.addCreature piker S.alice (S.landsInPlay swamp 7)
          -- A DECOY creature, so Crown's "another creature" offers two
          -- destinations and Attach.chooseHost really asks rather than
          -- eliding at a single candidate.
          (_, base2) = S.addCreature piker S.alice base1
          (brambleId, base3) = S.addCreature bramble S.alice base2
          (armed, auraSpell) = S.handOne unholyStrength base3
          onPiker = S.runPure (aimAtOffered pikerId) armed (S.cast S.alice auraSpell >> Stack.resolveTop)
          settledOnPiker = fireTriggers (aimAtOffered pikerId) onPiker
      case attachmentsOn pikerId settledOnPiker of
        [] -> Spec.assertFailure s "Unholy Strength should have entered attached to the Piker"
        auraId : _ -> do
          let (withCrown, crownSpell) = S.handOne crown settledOnPiker
              resolved = S.runPure S.identityAnswer withCrown (S.cast S.alice crownSpell >> Stack.resolveTop)
              crownIds = filter (\oid -> Game.cardOf oid resolved == Just (Printing.card crown)) (Set.toList (GameState.battlefield resolved))
          case (crownIds, firstActivatedOf crown) of
            (crownId : _, Just move) -> do
              let ready = resolved {GameState.priority = Just S.alice}
                  activated = S.runPure (moveOnto auraId brambleId) ready (Activate.activateAbility S.alice crownId move)
                  moved = S.runPure (moveOnto auraId brambleId) activated Stack.resolveTop
                  after = fireTriggers (moveOnto auraId brambleId) moved
              Spec.assertEqWith s "CR 603.2 two Saprolings once the move's trigger resolves" (saprolingsOf S.alice after) 2
              -- The other board, one object different: the same Aura entering
              -- on a creature that is not the watcher fires nothing.
              Spec.assertEqWith s "and none while the Aura sat on the Piker" (saprolingsOf S.alice settledOnPiker) 0
              Spec.assertEqWith s "the Aura really moved onto the Elemental" (attachmentsOn brambleId moved) [auraId]
            _ -> Spec.assertFailure s "Crown of the Ages should have resolved onto the battlefield with one activated ability"
    -- "An AURA", the word the Filter carries, on the SAME emit site as the
    -- leg above: Bonesplitter's equip attaches an Equipment to the Elemental
    -- through Event.attach, and nothing happens.
    --
    -- Discriminating only because that leg is the positive on this path --
    -- alone it would pass against an engine with no event at all.
    Spec.it s "CR 702.6a equipping the Elemental with Bonesplitter creates nothing" $ do
      plains <- S.printingOf s registry "Plains"
      bramble <- S.printingOf s registry "Bramble Elemental"
      bonesplitter <- S.printingOf s registry "Bonesplitter"
      let (brambleId, base1) = S.addCreature bramble S.alice (S.landsInPlay plains 3)
          (bladeId, base2) = S.addCreature bonesplitter S.alice base1
          ready = base2 {GameState.priority = Just S.alice}
      case firstActivatedOf bonesplitter of
        Nothing -> Spec.assertFailure s "Bonesplitter should print one activated ability"
        Just equip -> do
          let activated = S.runPure (aimAtOffered brambleId) ready (Activate.activateAbility S.alice bladeId equip)
              equipped = S.runPure (aimAtOffered brambleId) activated Stack.resolveTop
              after = fireTriggers (aimAtOffered brambleId) equipped
          Spec.assertEqWith s "no Saproling: an Equipment is not an Aura" (saprolingsOf S.alice after) 0
          -- Without this the zero says nothing: an equip that never happened
          -- would read the same.
          Spec.assertEqWith s "though the Equipment really did become attached" (attachmentsOn brambleId equipped) [bladeId]
          Spec.assertEqWith s "which CR 301.5f's +2/+0 confirms" (S.powerToughnessOf brambleId equipped) (Just (6, 4))

-- CR 613.1f layer 6, the TRIGGERED half of the grant: Sixth Sense ({G}
-- Enchantment -- Aura, "Enchant creature / Enchanted creature has 'Whenever this
-- creature deals combat damage to a player, you may draw a card.'", checked
-- against Scryfall on 2026-08-20) is the cheapest printing whose whole text box
-- is one quoted triggered ability, so nothing but the grant is under test.
--
-- Presence of Gond (Pawl.ActivateSpec) is the activated half of the same
-- Modification arm. What this group adds is the other side of the fold: a
-- granted ability has to be found by the CR 603.2 scan, not only by the
-- projection, and Pawl.Engine.Event.eventTriggers reads
-- ProjectedCharacteristics.triggeredAbilities to do it.
--
-- Three seats, and the two that matter are DIFFERENT players: alice controls the
-- enchanted attacker, carol controls the Aura, bob is the defending player. CR
-- 113.7 makes the enchanted creature the granted ability's source and CR 603.3a
-- makes its controller the trigger's controller, so the "you" that draws is
-- alice. A granter-anchored reading would draw for carol, and the two hands are
-- what tell those readings apart -- one seat could not.
sixthSenseSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sixthSenseSpec s registry = Spec.describe s "CR 613.1f a granted triggered ability" $ do
  -- The gameplay-level proof, and the pair's positive half.
  Spec.it s "CR 603.3a whole card: the enchanted creature connects and ITS controller draws" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case sixthSenseBoard piker sense mountain island True of
        ([attackerId], _, gs) -> do
          let after = S.runCombat sixthSenseAnswer gs
          Spec.assertEqWith s "alice drew the one card her library held" (handNames S.alice after) ["Mountain"]
          Spec.assertEqWith s "and the Aura's controller drew nothing" (handNames S.carol after) []
          Spec.assertEqWith s "CR 510.1b the Piker's 2 damage reached bob" (S.lifeOf S.bob after) (Just 18)
          Spec.assertBool s (S.onBattlefield attackerId after) "the unblocked attacker survived combat"
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"
  -- The pair's other half: the same board, the same combat, the Aura sitting on
  -- carol's battlefield unattached. Nothing else differs, so a draw here would
  -- mean the trigger came from somewhere other than the grant.
  Spec.it s "CR 303.4m an unattached Sixth Sense grants nothing and nobody draws" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case sixthSenseBoard piker sense mountain island False of
        ([_], _, gs) -> do
          let after = S.runCombat sixthSenseAnswer gs
          Spec.assertEqWith s "alice's hand is still empty" (handNames S.alice after) []
          Spec.assertEqWith s "and so is carol's" (handNames S.carol after) []
          Spec.assertEqWith s "the same combat still happened" (S.lifeOf S.bob after) (Just 18)
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"
  -- Where the ability ends up, CR 113.7: on the RECEIVER, and not on the Aura
  -- that prints the words.
  Spec.it s "CR 113.7 the enchanted creature has the trigger and the Aura does not" $ do
    ps <- traverse (S.printingOf s registry) ["Goblin Piker", "Sixth Sense", "Mountain", "Island"]
    case ps of
      [piker, sense, mountain, island] -> case (sixthSenseBoard piker sense mountain island True, sixthSenseBoard piker sense mountain island False) of
        (([attackerId], senseId, enchanted), ([bareId], _, unenchanted)) -> do
          Spec.assertEqWith s "one triggered ability on the enchanted creature" (length (Projection.triggeredAbilitiesOf attackerId enchanted)) 1
          Spec.assertEqWith s "the Piker prints none of its own" (length (Projection.triggeredAbilitiesOf bareId unenchanted)) 0
          Spec.assertEqWith s "and the granter does not have what it grants" (length (Projection.triggeredAbilitiesOf senseId enchanted)) 0
        _ -> Spec.assertFailure s "fixture should give alice exactly one attacker"
      _ -> Spec.assertFailure s "four printings"

-- alice attacks with one settled Goblin Piker, carol holds the Aura, bob defends
-- with nothing. Both libraries hold exactly one card, and DIFFERENT cards, so
-- "who drew" is answerable by name; stocking carol's as well keeps CR 104.3c out
-- of the negative reading, where a wrongly-controlled trigger would otherwise
-- deck her instead of drawing.
sixthSenseBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Bool -> ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
sixthSenseBoard piker sense mountain island attached =
  let (base, mine, _, _) = S.threePlayerCombat [piker] [] []
      stocked = snd (S.addLibraryCard island S.carol (snd (S.addLibraryCard mountain S.alice base)))
      (senseId, withAura) = S.addCreature sense S.carol stocked
      board = case mine of
        [attackerId] | attached -> S.attach senseId attackerId withAura
        _ -> withAura
   in (mine, senseId, board)

-- Attacks bob with everything and takes every "may". CR 507.1 leaves the
-- defending player to the active player's choice on a three-seat board, so it
-- has to be pinned.
sixthSenseAnswer :: Prompt.Prompt r -> r
sixthSenseAnswer p = case p of
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.attackTo S.bob p

-- The names in a player's hand, sorted. Names rather than a count, because the
-- two libraries hold different cards and which one moved is the question.
handNames :: PlayerId.PlayerId -> GameState.GameState -> [String]
handNames pid gs =
  List.sort
    [ Text.unpack (CardName.unwrap (S.soleFaceName oid gs))
    | oid <- Game.zoneMembers Zone.Hand pid gs
    ]

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Trigger" $ do
  discardTriggerSpec s registry
  cyclesTriggerSpec s registry
  selfDiscardTriggerSpec s registry
  drawTriggerSpec s registry
  miracleSpec s registry
  controllerAtTriggerSpec s registry
  counterTriggerSpec s registry
  lifeGainTriggerSpec s registry
  abilitiesWhenTriggeredSpec s registry
  lifeGainAmountSpec s registry
  falseCureSpec s registry
  enrageSpec s registry
  belltowerSphinxSpec s registry
  lifeLossTriggerSpec s registry
  mindcrankSpec s registry
  masterOfLaketownSpec s registry
  masterOfLaketownDeathSpec s registry
  anafenzaAttackSpec s registry
  ezuriExperienceSpec s registry
  youngPyromancerSpec s registry
  whisperingWizardSpec s registry
  clarionSpiritSpec s registry
  desolationTwinSpec s registry
  presenceOfTheMasterSpec s registry
  kambalSpec s registry
  brinebornCutthroatSpec s registry
  handOfThePraetorsSpec s registry
  monarchTriggerSpec s registry
  matoyaTriggerSpec s registry
  aloeAlchemistSpec s registry
  wildgrowthWalkerSpec s registry
  rayOfCommandSpec s registry
  brambleElementalSpec s registry
  sixthSenseSpec s registry
