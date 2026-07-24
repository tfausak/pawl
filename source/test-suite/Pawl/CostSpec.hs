{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Cost and the three types it cases on (Pawl.Type.Cost,
-- Pawl.Type.CostComponent, Pawl.Type.Payment), plus the two prompts the axis
-- adds. CR 118: what a cost IS, what it takes to pay one, and the alternative
-- and additional costs that change the answer.
--
-- The three gate cards: Greed (an amount-bearing component), Village Rites (a
-- mandatory spell-side additional cost) and Fireblast (an alternative cost with
-- no mana in it at all).
module Pawl.CostSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Registry as Registry
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as Action.Type
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Payment as Payment
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Card.Type.spell (Printing.card p))

doorTests :: Registry.Type.Registry -> Tasty.TestTree
doorTests registry =
  Tasty.testGroup
    "Door"
    [ -- CR 118.3's own second example: "a permanent that's already tapped can't
      -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
      HU.testCase "CR 107.5 TapThis is payable only while the permanent is untapped" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        let (oid, gs) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
            tapped = S.tapObject oid gs
        HU.assertBool "untapped pays" (Cost.canPayComponent S.alice oid CostComponent.TapThis gs)
        HU.assertBool "tapped does not" (not (Cost.canPayComponent S.alice oid CostComponent.TapThis tapped)),
      -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
      -- or something that's a permanent they don't control."
      HU.testCase "CR 701.21a SacrificeThis needs a permanent this player controls" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            (inHand, gs1) = S.addHandCard piker S.alice gs0
        HU.assertBool "a controlled permanent pays" (Cost.canPayComponent S.alice onField CostComponent.SacrificeThis gs1)
        HU.assertBool "a card in hand does not" (not (Cost.canPayComponent S.alice inHand CostComponent.SacrificeThis gs1))
        HU.assertBool "another player's permanent does not" (not (Cost.canPayComponent S.bob onField CostComponent.SacrificeThis gs1)),
      -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
      -- unpayable cost; an empty ManaCost is {0} and is payable.
      HU.testCase "CR 118.6 an unpayable cost can never be paid" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 5
        HU.assertBool
          "Nothing is unpayable"
          (not (Cost.canPay S.alice S.noSource (Cost.Type.MkCost Nothing []) gs)),
      HU.testCase "CR 118.5a a {0} cost is payable" $
        let gs = Setup.emptyGame S.bothPlayers
         in HU.assertBool
              "an empty ManaCost is {0}"
              (Cost.canPay S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs),
      -- CR 118.6a: "If an unpayable cost is increased by an effect or an
      -- additional cost is imposed, the cost is still unpayable." total maps over
      -- the Maybe, so there is no special case to get wrong.
      HU.testCase "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let base = S.landsInPlay mountain 5
            (_, gs) = S.addCreature thalia S.alice base
            (bolt, withBolt) = S.addHandCard lightningBolt S.alice gs
        HU.assertEqual
          "still Nothing"
          Nothing
          (Cost.Type.mana (Cost.total S.alice bolt (Cost.Type.MkCost Nothing []) withBolt)),
      -- The classification Pawl.Activate reads instead of matching a constructor.
      HU.testCase "CR 302.6 requiresTapSymbol classifies a cost, and Greed's counterpart proves it" $ do
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        drudgeSkeletons <- Registry.printing registry "Drudge Skeletons"
        let elves = ActivatedAbility.cost (theAbility llanowarElves)
            skeletons = ActivatedAbility.cost (theAbility drudgeSkeletons)
        HU.assertBool "Llanowar Elves' {T} cost requires the tap symbol" (Cost.requiresTapSymbol elves)
        HU.assertBool "Drudge Skeletons' {B} regenerate cost does not" (not (Cost.requiresTapSymbol skeletons)),
      -- Departure 1: Pawl.Activate does NOT route an ability cost through
      -- Cost.total. PlayerEffect.matchesSpell classifies an OBJECT, not a spell,
      -- so a noncreature PERMANENT matches Thalia's Not (HasCardType Creature)
      -- filter -- and Thalia taxes noncreature SPELLS, never abilities. Four Mountains
      -- must still afford Mindslaver's printed {4}; a fifth would be needed if
      -- the tax wrongly reached the activation (#90).
      HU.testCase "CR 613.11 Thalia does not tax a noncreature permanent's activated ability" $ do
        mountain <- Registry.printing registry "Mountain"
        mindslaver <- Registry.printing registry "Mindslaver"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        let base = S.landsInPlay mountain 4
            (slaver, gs1) = S.addCreature mindslaver S.alice base
            (_, gs2) = S.addCreature thalia S.alice gs1
        HU.assertBool
          "four Mountains still pay {4}"
          (Activate.activatable S.alice slaver (theAbility mindslaver) gs2),
      -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
      HU.testCase "CR 118.6 paying an unpayable cost changes nothing" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 3
            (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay S.alice S.noSource (Cost.Type.MkCost Nothing []))
        HU.assertEqual "Unpaid" Payment.Unpaid outcome
        HU.assertEqual "no land tapped" 0 (S.tappedCount S.alice after),
      -- CR 701.21a: enough controlled permanents matching the criterion.
      HU.testCase "CR 118.3 a Sacrifice component counts matching permanents this player controls" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs = S.landsInPlay mountain 2
            two = CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)
            three = CostComponent.Sacrifice 3 (Filter.Type.HasSubtype Subtype.Mountain)
            islands = CostComponent.Sacrifice 1 (Filter.Type.HasSubtype Subtype.Island)
        HU.assertBool "two Mountains pay for two" (Cost.canPayComponent S.alice S.noSource two gs)
        HU.assertBool "but not for three" (not (Cost.canPayComponent S.alice S.noSource three gs))
        HU.assertBool "and not for an Island" (not (Cost.canPayComponent S.alice S.noSource islands gs))
        HU.assertBool "and bob controls none of them" (not (Cost.canPayComponent S.bob S.noSource two gs)),
      -- CR 118.6: unpayable below the count, payable at or above it -- the same
      -- shape CR 118.3's Sacrifice test above takes, for the SPENT direction of
      -- the player-counter substrate (P10 #37 GainPlayerCounters is the ADD
      -- direction).
      HU.testCase "CR 118.6 PayEnergy is unpayable below the count and payable at or above" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            two = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
            one = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice gs0
        HU.assertBool "two energy pays PayEnergy 2" (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) two)
        HU.assertBool "one energy cannot" (not (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) one)),
      -- CR 107.14: paying energy removes exactly that many counters.
      HU.testCase "CR 107.14 paying PayEnergy removes that many energy counters" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
            three = S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice gs0
            after = S.runPure S.identityAnswer three (Monad.void (Cost.payComponent S.alice oid (CostComponent.PayEnergy 2)))
        HU.assertEqual "one energy left" 1 (S.playerCounterOf PlayerCounterKind.Energy S.alice after)
    ]

-- Greed {3}{B} Enchantment: "{B}, Pay 2 life: Draw a card."
--
-- Scryfall returned no rulings for this card; CR 118.3's own worked example is
-- the specification of the discriminating test.
-- alice controls Greed and one untapped Swamp, with three cards in her
-- library so a draw is never a CR 121.3 loss, and priority in her own
-- precombat main phase. Loaded fresh inside each case that needs it --
-- equivalent because loading is deterministic and cached (batch-recipe.md).
greedBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Integer -> (ObjectId.ObjectId, GameState.GameState)
greedBoard swamp greed piker life =
  let base = S.landsInPlay swamp 1
      (greedId, gs1) = S.addCreature greed S.alice base
      (_, gs2) = S.addLibraryCard piker S.alice gs1
      (_, gs3) = S.addLibraryCard piker S.alice gs2
      (_, gs4) = S.addLibraryCard piker S.alice gs3
   in ( greedId,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice,
            GameState.players = Map.adjust (\p -> p {Player.life = life}) S.alice (GameState.players gs4)
          }
      )

greedTests :: Registry.Type.Registry -> Tasty.TestTree
greedTests registry =
  let isActivate a = case a of
        Action.Type.Activate _ _ -> True
        _ -> False
   in Tasty.testGroup
        "Greed"
        [ HU.testCase "CR 119.4 activating draws a card and subtracts the life" $ do
            swamp <- Registry.printing registry "Swamp"
            greed <- Registry.printing registry "Greed"
            piker <- Registry.printing registry "Goblin Piker"
            let (greedId, gs) = greedBoard swamp greed piker 20
                activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
                resolved = S.runPure S.identityAnswer activated Stack.resolveTop
            HU.assertEqual "life 20 - 2" (Just 18) (S.lifeOf S.alice resolved)
            HU.assertEqual "one card drawn" 1 (S.handSize S.alice resolved)
            HU.assertEqual "the Swamp is tapped" 1 (S.tappedCount S.alice resolved),
          -- CR 118.3: "A player can't pay a cost without having the necessary
          -- resources to pay it fully. For example, a player with only 1 life
          -- can't pay a cost of 2 life." THE discriminating test: a payability
          -- check that ignores the amount passes the case above and fails here.
          HU.testCase "CR 118.3 at 1 life the ability is not offered" $ do
            swamp <- Registry.printing registry "Swamp"
            greed <- Registry.printing registry "Greed"
            piker <- Registry.printing registry "Goblin Piker"
            let (greedId, gs) = greedBoard swamp greed piker 1
            HU.assertBool
              "not activatable"
              (not (Activate.activatable S.alice greedId (theAbility greed) gs))
            HU.assertBool "no Activate action offered" (not (any isActivate (Action.legalActions S.alice gs))),
          HU.testCase "CR 119.4b at 2 life the ability IS offered" $ do
            swamp <- Registry.printing registry "Swamp"
            greed <- Registry.printing registry "Greed"
            piker <- Registry.printing registry "Goblin Piker"
            let (greedId, gs) = greedBoard swamp greed piker 2
            HU.assertBool
              "activatable"
              (Activate.activatable S.alice greedId (theAbility greed) gs),
          -- CR 704.5a: "If a player has 0 or less life, that player loses the
          -- game." Paying life is a real life-total change, and a cost may
          -- legally kill its payer.
          HU.testCase "CR 704.5a paying the last 2 life is legal and loses the game" $ do
            swamp <- Registry.printing registry "Swamp"
            greed <- Registry.printing registry "Greed"
            piker <- Registry.printing registry "Goblin Piker"
            let (greedId, gs) = greedBoard swamp greed piker 2
                activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
                settled = S.settleSba activated
            HU.assertEqual "life 0" (Just 0) (S.lifeOf S.alice activated)
            HU.assertEqual
              "alice has lost"
              (Just (Status.Departed Departure.Lost))
              (fmap Player.status (Map.lookup S.alice (GameState.players settled))),
          -- Greed has no {T} in its cost, so CR 302.6 never applies -- the
          -- counterpart to Llanowar Elves, whose cost is Just [] plus TapThis.
          HU.testCase "CR 302.6 Greed's cost requires no tap symbol" $ do
            greed <- Registry.printing registry "Greed"
            HU.assertBool
              "no {T}"
              (not (Cost.requiresTapSymbol (ActivatedAbility.cost (theAbility greed))))
        ]

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- forced and correctly elided). The Pawl.ReplacementSpec shape.
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

wasAskedToSacrifice :: [Response.Response] -> Bool
wasAskedToSacrifice responses =
  let isSacrifice r = case r of
        Response.ChoseSacrifices _ -> True
        _ -> False
   in any isSacrifice responses

wasAskedToChooseCost :: [Response.Response] -> Bool
wasAskedToChooseCost responses =
  let isCost r = case r of
        Response.ChoseCost _ -> True
        _ -> False
   in any isCost responses

-- Village Rites {B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- Its one ruling: "You must sacrifice exactly one creature to cast this spell;
-- you can't cast it without sacrificing a creature, and you can't sacrifice
-- additional creatures."
-- alice controls one untapped Swamp and `n` Pikers, holds one Village Rites,
-- and has three cards in her library so the draw is never a CR 121.3 loss.
-- Loaded fresh inside each case that needs it -- equivalent because loading
-- is deterministic and cached (batch-recipe.md).
villageRitesBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
villageRitesBoard swamp piker villageRites n =
  let base = S.landsInPlay swamp 1
      addPiker (ids, gs) _ = let (oid, gs') = S.addCreature piker S.alice gs in (ids <> [oid], gs')
      (pikers, withPikers) = List.foldl' addPiker ([], base) [1 .. n]
      (rites, gs1) = S.addHandCard villageRites S.alice withPikers
      (_, gs2) = S.addLibraryCard piker S.alice gs1
      (_, gs3) = S.addLibraryCard piker S.alice gs2
      (_, gs4) = S.addLibraryCard piker S.alice gs3
   in ( rites,
        pikers,
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Village Rites {B} Instant: "As an additional cost to cast this spell,
-- sacrifice a creature. Draw two cards."
--
-- Its one ruling: "You must sacrifice exactly one creature to cast this spell;
-- you can't cast it without sacrificing a creature, and you can't sacrifice
-- additional creatures."
villageRitesTests :: Registry.Type.Registry -> Tasty.TestTree
villageRitesTests registry =
  Tasty.testGroup
    "Village Rites"
    [ HU.testCase "CR 118.8 the additional cost is paid and the spell resolves" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        villageRites <- Registry.printing registry "Village Rites"
        let (rites, pikers, gs) = villageRitesBoard swamp piker villageRites 1
            cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice rites)
            resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        HU.assertEqual "no creature left on the battlefield" 0 (S.creaturesInPlay S.alice resolved)
        -- Plan-bug fix: CR 400.7 gives the sacrificed permanent a NEW
        -- object id in the graveyard (Pawl.Event.changeZone), so the
        -- brief's own membership check (the OLD battlefield id inside
        -- Zone.Graveyard) is unsatisfiable by construction -- it fails
        -- even against correct code, matching Pawl.TriggerSpec's own
        -- "a sacrificed permanent goes to its owner's graveyard" (a
        -- COUNT, never an id match). Counting preserves the assertion's
        -- intent (a Piker was sacrificed into the graveyard). The +1 is
        -- Village Rites itself: CR 608.2n, as the final part of an
        -- instant's resolution the spell is put into its owner's
        -- graveyard.
        HU.assertEqual
          "the sacrificed Piker(s) and the resolved instant are now in alice's graveyard"
          (length pikers + 1)
          (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        HU.assertEqual "two cards drawn" 2 (S.handSize S.alice resolved),
      -- The ruling's second clause, and CR 601.2f's placement of an
      -- additional cost INSIDE the total cost: an implementation that pays
      -- additional costs after announcement offers this cast.
      HU.testCase "CR 601.2f with no creature to sacrifice the spell is not castable" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        villageRites <- Registry.printing registry "Village Rites"
        let (rites, _, gs) = villageRitesBoard swamp piker villageRites 0
        HU.assertBool "not castable" (not (Cast.castable S.alice rites gs))
        HU.assertEqual "and not offered" [] (filter (isCastOf rites) (Action.legalActions S.alice gs)),
      -- The cost payment went through Event.sacrifice, the CR 701.21 funnel,
      -- so the turn history saw it. A direct zone poke passes both cases
      -- above and fails this one. The settle/resolve shape is
      -- Pawl.TriggerSpec's historyTests, verbatim.
      HU.testCase "CR 608.2i Khabál Ghoul counts a creature sacrificed to pay a cost" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        villageRites <- Registry.printing registry "Village Rites"
        khabalGhoul <- Registry.printing registry "Khabál Ghoul"
        let (rites, _, gs0) = villageRitesBoard swamp piker villageRites 1
            (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
            cast = S.runPure S.identityAnswer gs1 (Cast.castSpell S.alice rites)
            endStep = Phase.Ending EndingStep.EndStep
            beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
            settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
            resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
            atEnd = resolveAll (settle (beginEndStep (settle cast)))
            counters = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject ghoul atEnd)
        HU.assertEqual "one +1/+1 counter for the sacrificed Piker" 1 counters,
      -- CR 701.21a lets the player choose which of their permanents dies, so
      -- two candidates is a real choice; one is not, and where the rules
      -- leave nothing to ask, don't prompt.
      HU.testCase "CR 701.21a two creatures raise ChooseSacrifices; one elides it" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        villageRites <- Registry.printing registry "Village Rites"
        let (ritesTwo, _, twoPikers) = villageRitesBoard swamp piker villageRites 2
            (ritesOne, _, onePiker) = villageRitesBoard swamp piker villageRites 1
            askedTwo = answersFor S.identityAnswer twoPikers (Cast.castSpell S.alice ritesTwo)
            askedOne = answersFor S.identityAnswer onePiker (Cast.castSpell S.alice ritesOne)
        HU.assertBool "asked with two" (wasAskedToSacrifice askedTwo)
        HU.assertBool "not asked with one" (not (wasAskedToSacrifice askedOne)),
      -- CR 115.1 makes a target only what the word "target" names: a
      -- sacrifice choice is not one, so it must not travel as a target.
      HU.testCase "CR 115.1 the sacrifice choice is not a target choice" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        villageRites <- Registry.printing registry "Village Rites"
        let (rites, _, gs) = villageRitesBoard swamp piker villageRites 2
            asked = answersFor S.identityAnswer gs (Cast.castSpell S.alice rites)
            isTargets r = case r of
              Response.ChoseTargets _ -> True
              _ -> False
        HU.assertBool "no ChooseTargets was raised" (not (any isTargets asked))
    ]

isCastOf :: ObjectId.ObjectId -> Action.Type.Action -> Bool
isCastOf oid a = case a of
  Action.Type.Cast o -> o == oid
  _ -> False

-- alice controls `n` Mountains (all tapped when `tap` is True) and holds one
-- Fireblast, with priority in her own precombat main phase. Loaded fresh
-- inside each case that needs it -- equivalent because loading is
-- deterministic and cached (batch-recipe.md).
fireblastBoard :: Printing.Printing -> Printing.Printing -> Int -> Bool -> (ObjectId.ObjectId, GameState.GameState)
fireblastBoard mountain fireblastPrinting n tap =
  let base = S.landsInPlay mountain n
      tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
      tapped = if tap then tapAll base else base
      (fireblast, gs1) = S.addHandCard fireblastPrinting S.alice tapped
   in ( fireblast,
        gs1
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Fireblast {4}{R}{R} Instant: "You may sacrifice two Mountains rather than pay
-- this spell's mana cost. Fireblast deals 4 damage to any target."
--
-- Scryfall returned no rulings for this card.
fireblastTests :: Registry.Type.Registry -> Tasty.TestTree
fireblastTests registry =
  Tasty.testGroup
    "Fireblast"
    [ -- The headline test: the printed cost is unaffordable and the spell is
      -- castable anyway. Kills "castability is mana affordability" and "an
      -- alternative cost is a different ManaCost" at once.
      HU.testCase "CR 118.9 two TAPPED Mountains and an empty pool still cast it, and it deals 4" $ do
        mountain <- Registry.printing registry "Mountain"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
            cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice fireblast)
            resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        HU.assertBool "castable" (Cast.castable S.alice fireblast gs)
        HU.assertEqual "both Mountains sacrificed" 0 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
        HU.assertEqual "alice took 4 (identityAnswer targets the lowest recipient)" (Just 16) (S.lifeOf S.alice resolved),
      -- CR 118.9b: an alternative cost is optional, so a player who can
      -- afford both is really choosing.
      HU.testCase "CR 118.9b both costs payable raises ChooseCost; one payable elides it" $ do
        mountain <- Registry.printing registry "Mountain"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let (both, sixUntapped) = fireblastBoard mountain fireblastPrinting 6 False
            (onlyAlternative, twoTapped) = fireblastBoard mountain fireblastPrinting 2 True
            askedBoth = answersFor S.identityAnswer sixUntapped (Cast.castSpell S.alice both)
            askedOne = answersFor S.identityAnswer twoTapped (Cast.castSpell S.alice onlyAlternative)
        HU.assertBool "asked when both are payable" (wasAskedToChooseCost askedBoth)
        HU.assertBool "not asked when only one is" (not (wasAskedToChooseCost askedOne)),
      -- CR 118.9a: "Only one alternative cost can be applied to any one spell
      -- as it's being cast" -- the list-of-candidates shape itself. The
      -- printed cost is offered FIRST.
      HU.testCase "CR 118.9a costsFor offers the printed cost first, then each alternative" $ do
        mountain <- Registry.printing registry "Mountain"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
            candidates = Cost.costsFor fireblast gs
            red = ManaSymbol.OfType (ManaType.Colored Color.Red)
        HU.assertEqual "two candidates" 2 (length candidates)
        HU.assertEqual
          "the printed one first"
          [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), Just (ManaCost.MkManaCost [])]
          (fmap Cost.Type.mana candidates),
      -- CR 701.21a again, on the alternative's own component.
      HU.testCase "CR 701.21a three Mountains raise ChooseSacrifices; exactly two elide it" $ do
        mountain <- Registry.printing registry "Mountain"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let (three, threeMountains) = fireblastBoard mountain fireblastPrinting 3 True
            (two, twoMountains) = fireblastBoard mountain fireblastPrinting 2 True
            askedThree = answersFor S.identityAnswer threeMountains (Cast.castSpell S.alice three)
            askedTwo = answersFor S.identityAnswer twoMountains (Cast.castSpell S.alice two)
        HU.assertBool "asked with three" (wasAskedToSacrifice askedThree)
        HU.assertBool "not asked with exactly two" (not (wasAskedToSacrifice askedTwo)),
      HU.testCase "CR 118.3 one Mountain pays neither cost" $ do
        mountain <- Registry.printing registry "Mountain"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 1 True
        HU.assertBool "not castable" (not (Cast.castable S.alice fireblast gs))
    ]

-- The two cross-checks: Fireblast's alternative cost against the projection
-- (Blood Moon, CR 613 layer 4) and against P7's cost modification (Thalia, CR
-- 118.9d).
crossCheckWithPriority :: GameState.GameState -> GameState.GameState
crossCheckWithPriority gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = S.alice,
      GameState.priority = Just S.alice
    }

crossCheckTests :: Registry.Type.Registry -> Tasty.TestTree
crossCheckTests registry =
  Tasty.testGroup
    "CrossChecks"
    [ -- Blood Moon: "Nonbasic lands are Mountains." Evolving Wilds is a
      -- nonbasic land, so layer 4 makes it a Mountain and it may be
      -- sacrificed to Fireblast's alternative. The pair is what
      -- discriminates: WITHOUT Blood Moon the same board has one Mountain
      -- and the spell is not castable.
      --
      -- Blood Moon affects only NONBASIC lands, which is why the second
      -- permanent is Evolving Wilds and not an Island.
      HU.testCase "CR 613.1d PermanentOfSubtype reads the projection, not the printed type line" $ do
        mountain <- Registry.printing registry "Mountain"
        evolvingWilds <- Registry.printing registry "Evolving Wilds"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        bloodMoon <- Registry.printing registry "Blood Moon"
        let base = S.landsInPlay mountain 1
            (wilds, gs1) = S.addCreature evolvingWilds S.alice base
            (fireblast, gs2) = S.addHandCard fireblastPrinting S.alice gs1
            withoutMoon = crossCheckWithPriority gs2
            (_, gs3) = S.addCreature bloodMoon S.alice gs2
            withMoon = crossCheckWithPriority gs3
            cast = S.runPure S.identityAnswer withMoon (Cast.castSpell S.alice fireblast)
        HU.assertBool
          "without Blood Moon, Evolving Wilds is not a Mountain and one Mountain is not two"
          (not (Cast.castable S.alice fireblast withoutMoon))
        HU.assertBool "with Blood Moon it is castable" (Cast.castable S.alice fireblast withMoon)
        HU.assertBool "and Evolving Wilds was sacrificed as a Mountain" (not (Set.member wilds (GameState.battlefield cast))),
      -- CR 118.9d: "If an alternative cost is being paid to cast a spell,
      -- any additional costs, cost increases, and cost reductions that
      -- affect that spell are applied to that alternative cost." Fireblast
      -- is an instant, so Thalia's noncreature tax reaches it, and the
      -- alternative's ABSENT mana component is a real, taxable {0} raised to
      -- {1}. This is the test that requires Just [] rather than Nothing.
      HU.testCase "CR 118.9d Thalia raises the alternative cost's {0} to {1}" $ do
        mountain <- Registry.printing registry "Mountain"
        thalia <- Registry.printing registry "Thalia, Guardian of Thraben"
        fireblastPrinting <- Registry.printing registry "Fireblast"
        let tapAll gs = List.foldl' (flip S.tapObject) gs (Set.toList (GameState.battlefield gs))
            twoTapped = tapAll (S.landsInPlay mountain 2)
            (_, taxedTwo) = S.addCreature thalia S.alice twoTapped
            (fireblastTwo, gsTwo) = S.addHandCard fireblastPrinting S.alice taxedTwo
            -- The same board plus one UNTAPPED Mountain, which can pay the {1}.
            (_, threeMountains) = S.addCreature mountain S.alice twoTapped
            (_, taxedThree) = S.addCreature thalia S.alice threeMountains
            (fireblastThree, gsThree) = S.addHandCard fireblastPrinting S.alice taxedThree
            alternativeOf oid gs = case Cost.costsFor oid gs of
              _ : alt : _ -> Just (Cost.Type.mana (Cost.total S.alice oid alt gs))
              _ -> Nothing
        HU.assertEqual
          "the alternative's {0} is taxed to {1}"
          (Just (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])))
          (alternativeOf fireblastTwo (crossCheckWithPriority gsTwo))
        HU.assertBool
          "with nothing untapped the taxed alternative is unpayable, so Fireblast is not castable"
          (not (Cast.castable S.alice fireblastTwo (crossCheckWithPriority gsTwo)))
        HU.assertBool
          "a third, untapped Mountain pays the {1} and it is castable again"
          (Cast.castable S.alice fireblastThree (crossCheckWithPriority gsThree))
    ]

-- Longtusk Cub, the P10 capstone: an energy trigger (CR 603.2 / 509-510) that
-- feeds an energy-paid pump (CR 118 / 122.6). The ability is extracted via the
-- file-local total `theAbility` (no partial functions); the card-characteristics
-- case guards that the extraction sees a real ability.
longtuskCubTests :: Registry.Type.Registry -> Tasty.TestTree
longtuskCubTests registry =
  Tasty.testGroup
    "LongtuskCub"
    [ HU.testCase "Longtusk Cub is a {1}{G} 2/2 Cat with a pay-energy ability" $ do
        longtuskCub <- Registry.printing registry "Longtusk Cub"
        HU.assertEqual "name" (Text.pack "Longtusk Cub") (Card.Type.name (Printing.card longtuskCub))
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power (Printing.card longtuskCub))
        HU.assertEqual "one activated ability" 1 (length (Card.Type.activatedAbilities (Printing.card longtuskCub))),
      HU.testCase "CR 118.6 the pay-energy ability is payable at two energy, not at one, and grows the Cub" $ do
        longtuskCub <- Registry.printing registry "Longtusk Cub"
        let (cubId, base) = S.addCreature longtuskCub S.alice (Setup.emptyGame S.bothPlayers)
            ability = theAbility longtuskCub
            withTwo = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice base
            withOne = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice base
            activated = S.runPure S.identityAnswer withTwo (Activate.activateAbility S.alice cubId ability)
            resolved = S.runPure S.identityAnswer activated Stack.resolveTop
        HU.assertBool "payable at two" (Activate.activatable S.alice cubId ability withTwo)
        HU.assertBool "unpayable at one" (not (Activate.activatable S.alice cubId ability withOne))
        HU.assertEqual "energy spent" 0 (S.playerCounterOf PlayerCounterKind.Energy S.alice activated)
        HU.assertEqual "Cub grew a +1/+1 counter" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject cubId resolved)),
      HU.testCase "CR 603.2 Longtusk Cub gains two energy when it connects" $ do
        longtuskCub <- Registry.printing registry "Longtusk Cub"
        let (gs, _, _) = S.combatBoardOf [longtuskCub] []
            after = S.runCombat S.aggressiveAnswer gs
        HU.assertEqual "alice gained two energy" 2 (S.playerCounterOf PlayerCounterKind.Energy S.alice after)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.Cost" [doorTests registry, greedTests registry, villageRitesTests registry, fireblastTests registry, crossCheckTests registry, longtuskCubTests registry]
