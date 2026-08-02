{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Cost and the three types it cases on (Pawl.Types.Cost,
-- Pawl.Types.CostComponent, Pawl.Types.Payment), plus the two prompts the axis
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
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Departure as Departure
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The single activated ability of a printing. Total: the fallback is unreachable
-- in these fixtures. Duplicated per this suite's convention of group-local
-- helpers (ActivateSpec and ReplacementSpec each carry their own).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Card.Type.spell (Printing.card p)) ActivationTiming.AnyTime

doorSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
doorSpec s registry =
  Spec.describe s "Door" $ do
    -- CR 118.3's own second example: "a permanent that's already tapped can't
    -- be tapped to pay a cost" (CR 107.5 says the same for the {T} symbol).
    Spec.it s "CR 107.5 TapThis is payable only while the permanent is untapped" $ do
      prodigalSorcerer <- S.printingOf s registry "Prodigal Sorcerer"
      let (oid, gs) = S.addCreature prodigalSorcerer S.alice (Setup.emptyGame S.bothPlayers)
          tapped = S.tapObject oid gs
      Spec.assertBool s (Cost.canPayComponent S.alice oid CostComponent.TapThis gs) "untapped pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice oid CostComponent.TapThis tapped)) "tapped does not"
    -- CR 701.21a: "A player can't sacrifice something that isn't a permanent,
    -- or something that's a permanent they don't control."
    Spec.it s "CR 701.21a SacrificeThis needs a permanent this player controls" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice onField CostComponent.SacrificeThis gs1) "a controlled permanent pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice inHand CostComponent.SacrificeThis gs1)) "a card in hand does not"
      Spec.assertBool s (not (Cost.canPayComponent S.bob onField CostComponent.SacrificeThis gs1)) "another player's permanent does not"
    -- CR 702.29a's "Discard this card", the exact mirror of SacrificeThis
    -- above: one names a permanent its controller owns the choice of, the other
    -- names a card in a hand. Asked of the ZONE and the OWNER, because CR 108.4
    -- gives a card in a hand no controller for a control-shaped gate to read.
    --
    -- Tested directly rather than only through cycling, because the two gates
    -- an activation passes -- this one and Activate.abilitiesFor's -- would
    -- otherwise cover for each other, and either alone would look correct.
    Spec.it s "CR 702.29a DiscardThis needs the card in this player's hand" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (onField, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          (inHand, gs1) = S.addHandCard piker S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice inHand CostComponent.DiscardThis gs1) "a card in hand pays"
      Spec.assertBool s (not (Cost.canPayComponent S.alice onField CostComponent.DiscardThis gs1)) "a permanent does not"
      Spec.assertBool s (not (Cost.canPayComponent S.bob inHand CostComponent.DiscardThis gs1)) "and it is not the other player's to discard"
    -- CR 701.9a through Event.changeZone, the CR 400.7 funnel: the discarded
    -- card lands in its owner's graveyard as a new incarnation, so the old id
    -- is gone rather than moved.
    Spec.it s "CR 701.9a paying DiscardThis puts that card in the graveyard" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (inHand, gs0) = S.addHandCard piker S.alice (Setup.emptyGame S.bothPlayers)
          after = S.runPure S.identityAnswer gs0 (Cost.payComponent S.alice inHand CostComponent.DiscardThis)
      Spec.assertEqWith s "the hand is empty" (length (Game.zoneMembers Zone.Hand S.alice after)) 0
      Spec.assertEqWith s "and the card is in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
    -- CR 118.6 vs CR 118.5a: the distinction the Maybe carries. Nothing is an
    -- unpayable cost; an empty ManaCost is {0} and is payable.
    Spec.it s "CR 118.6 an unpayable cost can never be paid" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 5
      Spec.assertBool
        s
        (not (Cost.canPay S.alice S.noSource (Cost.Type.MkCost Nothing []) gs))
        "Nothing is unpayable"
    Spec.it s "CR 118.5a a {0} cost is payable" $
      let gs = Setup.emptyGame S.bothPlayers
       in Spec.assertBool
            s
            (Cost.canPay S.alice S.noSource (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) gs)
            "an empty ManaCost is {0}"
    -- CR 118.6a: "If an unpayable cost is increased by an effect or an
    -- additional cost is imposed, the cost is still unpayable." total maps over
    -- the Maybe, so there is no special case to get wrong.
    Spec.it s "CR 118.6a Thalia's increase leaves an unpayable cost unpayable" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      lightningBolt <- S.printingOf s registry "Lightning Bolt"
      let base = S.landsInPlay mountain 5
          (_, gs) = S.addCreature thalia S.alice base
          (bolt, withBolt) = S.addHandCard lightningBolt S.alice gs
      Spec.assertEqWith
        s
        "still Nothing"
        (Cost.Type.mana (Cost.total S.alice bolt (Cost.Type.MkCost Nothing []) withBolt))
        Nothing
    -- The classification Pawl.Engine.Activate reads instead of matching a constructor.
    Spec.it s "CR 302.6 requiresSicknessCheck classifies a cost, and Greed's counterpart proves it" $ do
      llanowarElves <- S.printingOf s registry "Llanowar Elves"
      drudgeSkeletons <- S.printingOf s registry "Drudge Skeletons"
      let elves = ActivatedAbility.cost (theAbility llanowarElves)
          skeletons = ActivatedAbility.cost (theAbility drudgeSkeletons)
      Spec.assertBool s (Cost.requiresSicknessCheck elves) "Llanowar Elves' {T} cost requires the tap symbol"
      Spec.assertBool s (not (Cost.requiresSicknessCheck skeletons)) "Drudge Skeletons' {B} regenerate cost does not"
    -- Departure 1: Pawl.Engine.Activate does NOT route an ability cost through
    -- Cost.total. PlayerEffect.matchesSpell classifies an OBJECT, not a spell,
    -- so a noncreature PERMANENT matches Thalia's Not (HasCardType Creature)
    -- filter -- and Thalia taxes noncreature SPELLS, never abilities. Four Mountains
    -- must still afford Mindslaver's printed {4}; a fifth would be needed if
    -- the tax wrongly reached the activation (#90).
    Spec.it s "CR 613.11 Thalia does not tax a noncreature permanent's activated ability" $ do
      mountain <- S.printingOf s registry "Mountain"
      mindslaver <- S.printingOf s registry "Mindslaver"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      let base = S.landsInPlay mountain 4
          (slaver, gs1) = S.addCreature mindslaver S.alice base
          (_, gs2) = S.addCreature thalia S.alice gs1
      Spec.assertBool
        s
        (Activate.activatable S.alice slaver (theAbility mindslaver) gs2)
        "four Mountains still pay {4}"
    -- Departure 2: an Unpaid payment is a complete no-op, never a partial one.
    Spec.it s "CR 118.6 paying an unpayable cost changes nothing" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 3
          (outcome, after) = S.runPureWith S.identityAnswer gs (Cost.pay S.alice S.noSource (Cost.Type.MkCost Nothing []))
      Spec.assertEqWith s "Unpaid" outcome Payment.Unpaid
      Spec.assertEqWith s "no land tapped" (S.tappedCount S.alice after) 0
    -- CR 701.21a: enough controlled permanents matching the criterion.
    Spec.it s "CR 118.3 a Sacrifice component counts matching permanents this player controls" $ do
      mountain <- S.printingOf s registry "Mountain"
      let gs = S.landsInPlay mountain 2
          two = CostComponent.Sacrifice 2 (Filter.Type.HasSubtype Subtype.Mountain)
          three = CostComponent.Sacrifice 3 (Filter.Type.HasSubtype Subtype.Mountain)
          islands = CostComponent.Sacrifice 1 (Filter.Type.HasSubtype Subtype.Island)
      Spec.assertBool s (Cost.canPayComponent S.alice S.noSource two gs) "two Mountains pay for two"
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource three gs)) "but not for three"
      Spec.assertBool s (not (Cost.canPayComponent S.alice S.noSource islands gs)) "and not for an Island"
      Spec.assertBool s (not (Cost.canPayComponent S.bob S.noSource two gs)) "and bob controls none of them"
    -- CR 118.6: unpayable below the count, payable at or above it -- the same
    -- shape CR 118.3's Sacrifice test above takes, for the SPENT direction of
    -- the player-counter substrate (P10 #37 GainPlayerCounters is the ADD
    -- direction).
    Spec.it s "CR 118.6 PayEnergy is unpayable below the count and payable at or above" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          two = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice gs0
          one = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice gs0
      Spec.assertBool s (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) two) "two energy pays PayEnergy 2"
      Spec.assertBool s (not (Cost.canPayComponent S.alice oid (CostComponent.PayEnergy 2) one)) "one energy cannot"
    -- CR 107.14: paying energy removes exactly that many counters.
    Spec.it s "CR 107.14 paying PayEnergy removes that many energy counters" $ do
      piker <- S.printingOf s registry "Goblin Piker"
      let (oid, gs0) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
          three = S.addPlayerCounter PlayerCounterKind.Energy 3 S.alice gs0
          after = S.runPure S.identityAnswer three (Monad.void (Cost.payComponent S.alice oid (CostComponent.PayEnergy 2)))
      Spec.assertEqWith s "one energy left" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 1

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

greedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
greedSpec s registry =
  Spec.describe s "Greed" $ do
    let isActivate a = case a of
          Action.Type.Activate _ _ -> True
          _ -> False
    Spec.it s "CR 119.4 activating draws a card and subtracts the life" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 20
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "life 20 - 2" (S.lifeOf S.alice resolved) (Just 18)
      Spec.assertEqWith s "one card drawn" (S.handSize S.alice resolved) 1
      Spec.assertEqWith s "the Swamp is tapped" (S.tappedCount S.alice resolved) 1
    -- CR 118.3: "A player can't pay a cost without having the necessary
    -- resources to pay it fully. For example, a player with only 1 life
    -- can't pay a cost of 2 life." THE discriminating test: a payability
    -- check that ignores the amount passes the case above and fails here.
    Spec.it s "CR 118.3 at 1 life the ability is not offered" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 1
      Spec.assertBool
        s
        (not (Activate.activatable S.alice greedId (theAbility greed) gs))
        "not activatable"
      Spec.assertBool s (not (any isActivate (Action.legalActions S.alice gs))) "no Activate action offered"
    Spec.it s "CR 119.4b at 2 life the ability IS offered" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 2
      Spec.assertBool
        s
        (Activate.activatable S.alice greedId (theAbility greed) gs)
        "activatable"
    -- CR 704.5a: "If a player has 0 or less life, that player loses the
    -- game." Paying life is a real life-total change, and a cost may
    -- legally kill its payer.
    Spec.it s "CR 704.5a paying the last 2 life is legal and loses the game" $ do
      swamp <- S.printingOf s registry "Swamp"
      greed <- S.printingOf s registry "Greed"
      piker <- S.printingOf s registry "Goblin Piker"
      let (greedId, gs) = greedBoard swamp greed piker 2
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice greedId (theAbility greed))
          settled = S.settleSba activated
      Spec.assertEqWith s "life 0" (S.lifeOf S.alice activated) (Just 0)
      Spec.assertEqWith
        s
        "alice has lost"
        (fmap Player.status (Map.lookup S.alice (GameState.players settled)))
        (Just (Status.Departed Departure.Lost))
    -- Greed has no {T} in its cost, so CR 302.6 never applies -- the
    -- counterpart to Llanowar Elves, whose cost is Just [] plus TapThis.
    Spec.it s "CR 302.6 Greed's cost requires no tap symbol" $ do
      greed <- S.printingOf s registry "Greed"
      Spec.assertBool
        s
        (not (Cost.requiresSicknessCheck (ActivatedAbility.cost (theAbility greed))))
        "no {T}"

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
villageRitesSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
villageRitesSpec s registry =
  Spec.describe s "Village Rites" $ do
    Spec.it s "CR 118.8 the additional cost is paid and the spell resolves" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, pikers, gs) = villageRitesBoard swamp piker villageRites 1
          cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice rites)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertEqWith s "no creature left on the battlefield" (S.creaturesInPlay S.alice resolved) 0
      -- Plan-bug fix: CR 400.7 gives the sacrificed permanent a NEW
      -- object id in the graveyard (Pawl.Engine.Event.changeZone), so the
      -- brief's own membership check (the OLD battlefield id inside
      -- Zone.Graveyard) is unsatisfiable by construction -- it fails
      -- even against correct code, matching Pawl.TriggerSpec's own
      -- "a sacrificed permanent goes to its owner's graveyard" (a
      -- COUNT, never an id match). Counting preserves the assertion's
      -- intent (a Piker was sacrificed into the graveyard). The +1 is
      -- Village Rites itself: CR 608.2n, as the final part of an
      -- instant's resolution the spell is put into its owner's
      -- graveyard.
      Spec.assertEqWith
        s
        "the sacrificed Piker(s) and the resolved instant are now in alice's graveyard"
        (length (Game.zoneMembers Zone.Graveyard S.alice resolved))
        (length pikers + 1)
      Spec.assertEqWith s "two cards drawn" (S.handSize S.alice resolved) 2
    -- The ruling's second clause, and CR 601.2f's placement of an
    -- additional cost INSIDE the total cost: an implementation that pays
    -- additional costs after announcement offers this cast.
    Spec.it s "CR 601.2f with no creature to sacrifice the spell is not castable" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, _, gs) = villageRitesBoard swamp piker villageRites 0
      Spec.assertBool s (not (Cast.castable S.alice rites gs)) "not castable"
      Spec.assertEqWith s "and not offered" (filter (isCastOf rites) (Action.legalActions S.alice gs)) []
    -- The cost payment went through Event.sacrifice, the CR 701.21 funnel,
    -- so the turn history saw it. A direct zone poke passes both cases
    -- above and fails this one. The settle/resolve shape is
    -- Pawl.TriggerSpec's historySpec, verbatim.
    Spec.it s "CR 608.2i Khabál Ghoul counts a creature sacrificed to pay a cost" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      khabalGhoul <- S.printingOf s registry "Khabál Ghoul"
      let (rites, _, gs0) = villageRitesBoard swamp piker villageRites 1
          (ghoul, gs1) = S.addCreature khabalGhoul S.alice gs0
          cast = S.runPure S.identityAnswer gs1 (Cast.castSpell S.alice rites)
          endStep = Phase.Ending EndingStep.EndStep
          beginEndStep gs = Event.recordEvent (GameEvent.StepBegan endStep S.alice) (gs {GameState.phase = endStep})
          settle gs = S.runPure S.identityAnswer gs Engine.settleForPriority
          resolveAll gs = S.runPure S.identityAnswer gs Engine.priorityLoop
          atEnd = resolveAll (settle (beginEndStep (settle cast)))
          counters = maybe 0 (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject ghoul atEnd)
      Spec.assertEqWith s "one +1/+1 counter for the sacrificed Piker" counters 1
    -- CR 701.21a lets the player choose which of their permanents dies, so
    -- two candidates is a real choice; one is not, and where the rules
    -- leave nothing to ask, don't prompt.
    Spec.it s "CR 701.21a two creatures raise ChooseSacrifices; one elides it" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (ritesTwo, _, twoPikers) = villageRitesBoard swamp piker villageRites 2
          (ritesOne, _, onePiker) = villageRitesBoard swamp piker villageRites 1
          askedTwo = answersFor S.identityAnswer twoPikers (Cast.castSpell S.alice ritesTwo)
          askedOne = answersFor S.identityAnswer onePiker (Cast.castSpell S.alice ritesOne)
      Spec.assertBool s (wasAskedToSacrifice askedTwo) "asked with two"
      Spec.assertBool s (not (wasAskedToSacrifice askedOne)) "not asked with one"
    -- CR 115.1 makes a target only what the word "target" names: a
    -- sacrifice choice is not one, so it must not travel as a target.
    Spec.it s "CR 115.1 the sacrifice choice is not a target choice" $ do
      swamp <- S.printingOf s registry "Swamp"
      piker <- S.printingOf s registry "Goblin Piker"
      villageRites <- S.printingOf s registry "Village Rites"
      let (rites, _, gs) = villageRitesBoard swamp piker villageRites 2
          asked = answersFor S.identityAnswer gs (Cast.castSpell S.alice rites)
          isTargets r = case r of
            Response.ChoseTargets _ -> True
            _ -> False
      Spec.assertBool s (not (any isTargets asked)) "no ChooseTargets was raised"

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
fireblastSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
fireblastSpec s registry =
  Spec.describe s "Fireblast" $ do
    -- The headline test: the printed cost is unaffordable and the spell is
    -- castable anyway. Kills "castability is mana affordability" and "an
    -- alternative cost is a different ManaCost" at once.
    Spec.it s "CR 118.9 two TAPPED Mountains and an empty pool still cast it, and it deals 4" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
          cast = S.runPure S.identityAnswer gs (Cast.castSpell S.alice fireblast)
          resolved = S.runPure S.identityAnswer cast Stack.resolveTop
      Spec.assertBool s (Cast.castable S.alice fireblast gs) "castable"
      Spec.assertEqWith s "both Mountains sacrificed" (length (Game.zoneMembers Zone.Battlefield S.alice resolved)) 0
      Spec.assertEqWith s "alice took 4 (identityAnswer targets the lowest recipient)" (S.lifeOf S.alice resolved) (Just 16)
    -- CR 118.9b: an alternative cost is optional, so a player who can
    -- afford both is really choosing.
    Spec.it s "CR 118.9b both costs payable raises ChooseCost; one payable elides it" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (both, sixUntapped) = fireblastBoard mountain fireblastPrinting 6 False
          (onlyAlternative, twoTapped) = fireblastBoard mountain fireblastPrinting 2 True
          askedBoth = answersFor S.identityAnswer sixUntapped (Cast.castSpell S.alice both)
          askedOne = answersFor S.identityAnswer twoTapped (Cast.castSpell S.alice onlyAlternative)
      Spec.assertBool s (wasAskedToChooseCost askedBoth) "asked when both are payable"
      Spec.assertBool s (not (wasAskedToChooseCost askedOne)) "not asked when only one is"
    -- CR 118.9a: "Only one alternative cost can be applied to any one spell
    -- as it's being cast" -- the list-of-candidates shape itself. The
    -- printed cost is offered FIRST.
    Spec.it s "CR 118.9a costsFor offers the printed cost first, then each alternative" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 2 True
          candidates = Cost.costsFor fireblast gs
          red = ManaSymbol.OfType (ManaType.Colored Color.Red)
      Spec.assertEqWith s "two candidates" (length candidates) 2
      Spec.assertEqWith
        s
        "the printed one first"
        (fmap Cost.Type.mana candidates)
        [Just (ManaCost.MkManaCost [ManaSymbol.Generic 4, red, red]), Just (ManaCost.MkManaCost [])]
    -- CR 701.21a again, on the alternative's own component.
    Spec.it s "CR 701.21a three Mountains raise ChooseSacrifices; exactly two elide it" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (three, threeMountains) = fireblastBoard mountain fireblastPrinting 3 True
          (two, twoMountains) = fireblastBoard mountain fireblastPrinting 2 True
          askedThree = answersFor S.identityAnswer threeMountains (Cast.castSpell S.alice three)
          askedTwo = answersFor S.identityAnswer twoMountains (Cast.castSpell S.alice two)
      Spec.assertBool s (wasAskedToSacrifice askedThree) "asked with three"
      Spec.assertBool s (not (wasAskedToSacrifice askedTwo)) "not asked with exactly two"
    Spec.it s "CR 118.3 one Mountain pays neither cost" $ do
      mountain <- S.printingOf s registry "Mountain"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      let (fireblast, gs) = fireblastBoard mountain fireblastPrinting 1 True
      Spec.assertBool s (not (Cast.castable S.alice fireblast gs)) "not castable"

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

crossCheckSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crossCheckSpec s registry =
  Spec.describe s "CrossChecks" $ do
    -- Blood Moon: "Nonbasic lands are Mountains." Evolving Wilds is a
    -- nonbasic land, so layer 4 makes it a Mountain and it may be
    -- sacrificed to Fireblast's alternative. The pair is what
    -- discriminates: WITHOUT Blood Moon the same board has one Mountain
    -- and the spell is not castable.
    --
    -- Blood Moon affects only NONBASIC lands, which is why the second
    -- permanent is Evolving Wilds and not an Island.
    Spec.it s "CR 613.1d PermanentOfSubtype reads the projection, not the printed type line" $ do
      mountain <- S.printingOf s registry "Mountain"
      evolvingWilds <- S.printingOf s registry "Evolving Wilds"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
      bloodMoon <- S.printingOf s registry "Blood Moon"
      let base = S.landsInPlay mountain 1
          (wilds, gs1) = S.addCreature evolvingWilds S.alice base
          (fireblast, gs2) = S.addHandCard fireblastPrinting S.alice gs1
          withoutMoon = crossCheckWithPriority gs2
          (_, gs3) = S.addCreature bloodMoon S.alice gs2
          withMoon = crossCheckWithPriority gs3
          cast = S.runPure S.identityAnswer withMoon (Cast.castSpell S.alice fireblast)
      Spec.assertBool
        s
        (not (Cast.castable S.alice fireblast withoutMoon))
        "without Blood Moon, Evolving Wilds is not a Mountain and one Mountain is not two"
      Spec.assertBool s (Cast.castable S.alice fireblast withMoon) "with Blood Moon it is castable"
      Spec.assertBool s (not (Set.member wilds (GameState.battlefield cast))) "and Evolving Wilds was sacrificed as a Mountain"
    -- CR 118.9d: "If an alternative cost is being paid to cast a spell,
    -- any additional costs, cost increases, and cost reductions that
    -- affect that spell are applied to that alternative cost." Fireblast
    -- is an instant, so Thalia's noncreature tax reaches it, and the
    -- alternative's ABSENT mana component is a real, taxable {0} raised to
    -- {1}. This is the test that requires Just [] rather than Nothing.
    Spec.it s "CR 118.9d Thalia raises the alternative cost's {0} to {1}" $ do
      mountain <- S.printingOf s registry "Mountain"
      thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
      fireblastPrinting <- S.printingOf s registry "Fireblast"
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
      Spec.assertEqWith
        s
        "the alternative's {0} is taxed to {1}"
        (alternativeOf fireblastTwo (crossCheckWithPriority gsTwo))
        (Just (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])))
      Spec.assertBool
        s
        (not (Cast.castable S.alice fireblastTwo (crossCheckWithPriority gsTwo)))
        "with nothing untapped the taxed alternative is unpayable, so Fireblast is not castable"
      Spec.assertBool
        s
        (Cast.castable S.alice fireblastThree (crossCheckWithPriority gsThree))
        "a third, untapped Mountain pays the {1} and it is castable again"

-- Longtusk Cub, the P10 capstone: an energy trigger (CR 603.2 / 509-510) that
-- feeds an energy-paid pump (CR 118 / 122.6). The ability is extracted via the
-- file-local total `theAbility` (no partial functions); the card-characteristics
-- case guards that the extraction sees a real ability.
longtuskCubSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
longtuskCubSpec s registry =
  Spec.describe s "LongtuskCub" $ do
    Spec.it s "Longtusk Cub is a {1}{G} 2/2 Cat with a pay-energy ability" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      Spec.assertEqWith s "name" (Card.Type.name (Printing.card longtuskCub)) (CardName.MkCardName $ Text.pack "Longtusk Cub")
      Spec.assertEqWith s "power" (Card.Type.power (Printing.card longtuskCub)) (Just (Power.MkPower (Quantity.Type.Literal 2)))
      Spec.assertEqWith s "one activated ability" (length (Card.Type.activatedAbilities (Printing.card longtuskCub))) 1
    Spec.it s "CR 118.6 the pay-energy ability is payable at two energy, not at one, and grows the Cub" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      let (cubId, base) = S.addCreature longtuskCub S.alice (Setup.emptyGame S.bothPlayers)
          ability = theAbility longtuskCub
          withTwo = S.addPlayerCounter PlayerCounterKind.Energy 2 S.alice base
          withOne = S.addPlayerCounter PlayerCounterKind.Energy 1 S.alice base
          activated = S.runPure S.identityAnswer withTwo (Activate.activateAbility S.alice cubId ability)
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertBool s (Activate.activatable S.alice cubId ability withTwo) "payable at two"
      Spec.assertBool s (not (Activate.activatable S.alice cubId ability withOne)) "unpayable at one"
      Spec.assertEqWith s "energy spent" (S.playerCounterOf PlayerCounterKind.Energy S.alice activated) 0
      Spec.assertEqWith s "Cub grew a +1/+1 counter" (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject cubId resolved)) (Just 1)
    Spec.it s "CR 603.2 Longtusk Cub gains two energy when it connects" $ do
      longtuskCub <- S.printingOf s registry "Longtusk Cub"
      let (gs, _, _) = S.combatBoardOf [longtuskCub] []
          after = S.runCombat S.aggressiveAnswer gs
      Spec.assertEqWith s "alice gained two energy" (S.playerCounterOf PlayerCounterKind.Energy S.alice after) 2

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Cost" $ do
  doorSpec s registry
  greedSpec s registry
  villageRitesSpec s registry
  catharticReunionSpec s registry
  safeholdSentrySpec s registry
  fireblastSpec s registry
  crossCheckSpec s registry
  longtuskCubSpec s registry

-- alice controls a Safehold Sentry and three Plains, all settled. `tapped` says
-- whether the Sentry itself starts tapped -- which for a {Q} cost is the payable
-- state, the exact inverse of every {T} fixture in this file.
sentryBoard :: Printing.Printing -> Printing.Printing -> Bool -> (ObjectId.ObjectId, GameState.GameState)
sentryBoard plains safeholdSentry tapped =
  let base = S.landsInPlay plains 3
      (sentry, gs1) = S.addCreature safeholdSentry S.alice base
      turnTapped o = if tapped then o {Object.tapped = TapState.Tapped} else o
   in ( sentry,
        gs1
          { GameState.objects = Map.adjust turnTapped sentry (GameState.objects gs1),
            GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Safehold Sentry {1}{W} Creature -- Elf Warrior 2/2: "{2}{W}, {Q}: This creature
-- gets +0/+2 until end of turn." The card CR 107.6's untap symbol was waiting for.
safeholdSentrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
safeholdSentrySpec s registry =
  Spec.describe s "Safehold Sentry" $ do
    Spec.it s "CR 107.6 whole card: a TAPPED Sentry untaps to pay {Q} and gets +0/+2" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry True
          activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice sentry (theAbility safeholdSentry))
          resolved = S.runPure S.identityAnswer activated Stack.resolveTop
      Spec.assertEqWith s "paying the cost UNTAPPED it" (fmap Object.tapped (Game.lookupObject sentry activated)) (Just TapState.Untapped)
      Spec.assertEqWith s "the three Plains paid the mana" (S.tappedCount S.alice activated) 3
      Spec.assertEqWith s "toughness 2 + 2" (Projection.toughnessOf sentry resolved) (Just 4)
      Spec.assertEqWith s "power unchanged" (Projection.powerOf sentry resolved) (Just 2)
    -- CR 107.6's second sentence, and the exact inverse of TapThis: "A permanent
    -- that's already untapped can't be untapped again to pay the cost."
    Spec.it s "CR 107.6 an UNTAPPED Sentry cannot pay {Q}, so the ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry False
      Spec.assertBool s (not (Cost.canPay S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) gs)) "canPay says no"
      Spec.assertBool s (not (any isActivateAction (Action.legalActions S.alice gs))) "and no Activate is offered"
    -- The issue itself (#204): CR 302.6 names the tap symbol AND the untap
    -- symbol, and only the first half had a producer. A summoning-sick Sentry
    -- must not be able to activate a {Q} ability.
    Spec.it s "CR 302.6 a summoning-sick Sentry's {Q} ability is not offered" $ do
      plains <- S.printingOf s registry "Plains"
      safeholdSentry <- S.printingOf s registry "Safehold Sentry"
      let (sentry, gs) = sentryBoard plains safeholdSentry True
          sick = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) sentry (GameState.objects gs)}
      Spec.assertBool s (Cost.canPay S.alice sentry (ActivatedAbility.cost (theAbility safeholdSentry)) sick) "the cost itself is still payable -- it is tapped"
      Spec.assertBool s (not (any isActivateAction (Action.legalActions S.alice sick))) "but CR 302.6 withholds the ability"

isActivateAction :: Action.Type.Action -> Bool
isActivateAction a = case a of
  Action.Type.Activate _ _ -> True
  _ -> False

-- alice has two untapped Mountains, holds one Cathartic Reunion plus `n` other
-- cards, and has four cards in her library so the draw of three is never a
-- CR 104.3c loss. The Village Rites board's shape, on the hand axis instead of
-- the battlefield one.
catharticBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId.ObjectId, GameState.GameState)
catharticBoard mountain piker catharticReunion n =
  let base = S.landsInPlay mountain 2
      (reunion, gs1) = S.addHandCard catharticReunion S.alice base
      withHand = List.foldl' (\g _ -> snd (S.addHandCard piker S.alice g)) gs1 [1 .. n]
      withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) withHand [1 .. (4 :: Int)]
   in ( reunion,
        withLibrary
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- Answers ChooseDiscard with nothing at all, and everything else normally. Two
-- different jobs in this group: it proves the forced case is never ASKED (an
-- empty answer would otherwise discard nothing), and it drives the
-- reject-not-repair case where the prompt is real.
noDiscardAnswer :: Prompt.Prompt r -> r
noDiscardAnswer p = case p of
  Prompt.ChooseDiscard {} -> []
  _ -> S.identityAnswer p

-- Cathartic Reunion {1}{R} Sorcery: "As an additional cost to cast this spell,
-- discard two cards. Draw three cards." The card CR 601.2f's "discarding cards"
-- clause was waiting for.
catharticReunionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
catharticReunionSpec s registry =
  Spec.describe s "Cathartic Reunion" $ do
    Spec.it s "CR 118.8 whole card: the two cards are discarded and three are drawn" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      -- Exactly two other cards, so CR 701.9b has no choice to offer and the
      -- prompt is elided -- which noDiscardAnswer proves, since an answer of
      -- [] would discard nothing if the prompt were actually raised.
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 2
          cast = S.runPure noDiscardAnswer gs (Cast.castSpell S.alice reunion)
          resolved = S.runPure noDiscardAnswer cast Stack.resolveTop
      Spec.assertEqWith s "the hand emptied as the cost was paid" (S.handSize S.alice cast) 0
      Spec.assertEqWith s "two discarded cards in the graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 2
      Spec.assertEqWith s "three cards drawn" (S.handSize S.alice resolved) 3
      -- CR 608.2n: the sorcery joins the two discards on resolution.
      Spec.assertEqWith s "and the spell itself is there too" (length (Game.zoneMembers Zone.Graveyard S.alice resolved)) 3
    Spec.it s "CR 601.2f with only one other card in hand the spell is not castable" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 1
          isCastOfReunion a = case a of
            Action.Type.Cast oid -> oid == reunion
            _ -> False
      -- Read through costsFor, so the assertion is against the cost the engine
      -- would actually offer (mana cost plus the printed additional cost),
      -- never a hand-built one.
      Spec.assertBool s (not (any (\c -> Cost.canPay S.alice reunion c gs) (Cost.costsFor reunion gs))) "no offered cost is payable"
      Spec.assertBool s (not (any isCastOfReunion (Action.legalActions S.alice gs))) "and no Cast is offered"
    Spec.it s "CR 601.2h an undersized answer leaves the whole cast unpaid, not partly paid" $ do
      -- The COST path's reject-not-repair, and deliberately the opposite of what
      -- the Discard EFFECT does after #245: a cost may go unpaid, so Pawl.Engine.Cost.pay
      -- restores the entry state and nothing at all happened. Three other cards
      -- makes the prompt real (hand > count), unlike the forced case above.
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 3
          cast = S.runPure noDiscardAnswer gs (Cast.castSpell S.alice reunion)
      Spec.assertEqWith s "nothing was discarded" (length (Game.zoneMembers Zone.Graveyard S.alice cast)) 0
      Spec.assertEqWith s "the hand is untouched, Reunion included" (S.handSize S.alice cast) 4
      Spec.assertEqWith s "nothing reached the stack" (length (GameState.stack cast)) 0
      Spec.assertEqWith s "and the Mountains are untapped again" (S.tappedCount S.alice cast) 0
    -- Where the reject boundary actually falls, stated rather than inferred.
    -- The answer is read as a SET, so a duplicate is normalised, not repaired:
    -- naming two distinct cards across three entries pays, and naming one card
    -- twice does not. Prompt.ChooseSacrifices is answered with a Set, so an
    -- interpreter meaning [a,a,b] there builds {a,b} and the Sacrifice arm
    -- accepts it -- this keeps the two components answering alike.
    Spec.it s "CR 601.2h the answer is read as a set: [a,a,b] pays for two, [a,a] does not" $ do
      mountain <- S.printingOf s registry "Mountain"
      piker <- S.printingOf s registry "Goblin Piker"
      catharticReunion <- S.printingOf s registry "Cathartic Reunion"
      let (reunion, gs) = catharticBoard mountain piker catharticReunion 3
          -- Repeat the first offered card, then add a second distinct one.
          duplicateThenDistinct q = case q of
            Prompt.ChooseDiscard _ _ ids _ -> case ids of
              a : b : _ -> [a, a, b]
              _ -> []
            _ -> S.identityAnswer q
          -- The same card twice and nothing else: one distinct card for a
          -- count of two.
          sameCardTwice q = case q of
            Prompt.ChooseDiscard _ _ ids _ -> concat (replicate 2 (take 1 ids))
            _ -> S.identityAnswer q
          paid = S.runPure duplicateThenDistinct gs (Cast.castSpell S.alice reunion)
          unpaid = S.runPure sameCardTwice gs (Cast.castSpell S.alice reunion)
      Spec.assertEqWith s "[a,a,b] names two distinct cards, so the cost is paid" (length (Game.zoneMembers Zone.Graveyard S.alice paid)) 2
      Spec.assertEqWith s "and the spell is on the stack" (length (GameState.stack paid)) 1
      Spec.assertEqWith s "[a,a] names one, so nothing is discarded" (length (Game.zoneMembers Zone.Graveyard S.alice unpaid)) 0
      Spec.assertEqWith s "and nothing reached the stack" (length (GameState.stack unpaid)) 0
