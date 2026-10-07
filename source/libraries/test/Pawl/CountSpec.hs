{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Count, Pawl.Types.Count, Pawl.Types.Scope, Pawl.Types.PlayerRef,
-- Pawl.Types.EventShape and Pawl.Types.Aggregation. Unit-level: the fold is driven
-- against a stubbed ViewOf so the evaluator is tested apart from the projection
-- that supplies it (Pawl.PowerToughnessSpec covers the wiring). Two exceptions,
-- each of which says so where it sits: the Aggregation.Greatest case that folds
-- a PROJECTED power, since a stub has no power to read; and the groups at the
-- foot of the module, which are gameplay level because what they prove is that
-- a count folds what the engine actually recorded.
module Pawl.CountSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.CastRestrictionSpec as CastSpec
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.OutsideDestination as OutsideDestination
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StepBegan as StepBegan
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

swampsYouControl :: Count.Type.Count Quantity.Type.Quantity
swampsYouControl =
  Count.Type.MkCount
    (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
    (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
    Aggregation.Members

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Count" $ do
  Spec.it s "Members counts the matching members of a zone" $ do
    -- Two Swamps Alice controls, one Bob controls; ControlledBy You keeps
    -- Alice's two.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addPermanent swampPrinting S.alice gs0
        (a2, gs2) = S.addPermanent swampPrinting S.alice gs1
        (b1, gs) = S.addPermanent swampPrinting S.bob gs2
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (a2, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob)
            ]
    Spec.assertEq s (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs swampsYouControl) $ Just 2

  Spec.it s "CR 208.2a DistinctCardTypes counts the union, not the objects" $ do
    -- Three graveyard cards, two of them Creatures: the answer is 2 types,
    -- not 3 objects.
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let gs0 = Setup.emptyGame S.bothPlayers
        (g1, gs1) = S.addGraveyardCard piker S.alice gs0
        (g2, gs2) = S.addGraveyardCard piker S.alice gs1
        (g3, gs) = S.addGraveyardCard lightningBolt S.alice gs2
        viewOf =
          S.stubView
            [ (g1, Set.singleton CardType.Creature, Set.empty, Nothing),
              (g2, Set.singleton CardType.Creature, Set.empty, Nothing),
              (g3, Set.singleton CardType.Instant, Set.empty, Nothing)
            ]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Graveyard PlayerRef.EachPlayer))
            (Filter.Type.And [])
            Aggregation.DistinctCardTypes
    Spec.assertEqWith s "two types" (S.countOf viewOf (Filter.contextFor Teams.none Nothing Nothing) gs count) $ Just 2

  -- A GRAVEYARD rather than the battlefield, here and in the three-seat case
  -- below, and CR 400.1 is why: a graveyard is one player's, so "whose copy" is
  -- a question it has an answer to, while the battlefield is shared and
  -- Pawl.Codec.InZone refuses to decode a scope dividing it (see #161). These
  -- two cases are about the REFERENCE rather than about the zone, so they are
  -- written over a zone a card may pair one with.
  Spec.it s "CR 102.2 Relative Opponent excludes the perspective" $ do
    -- Read from Bob's perspective: his opponent Alice has one Swamp in her
    -- graveyard, and his own does not count.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addGraveyardCard swampPrinting S.alice gs0
        (b1, gs) = S.addGraveyardCard swampPrinting S.bob gs1
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        -- CR 108.4: a card in a graveyard has no controller, so the stub answers
        -- Nothing for one. The filter below asks about a subtype, not about a
        -- player, so the fold is the reference's work alone.
        viewOf = S.stubView [(a1, land, swamp, Nothing), (b1, land, swamp, Nothing)]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.Opponent)))
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "Alice's one" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.bob) Nothing) gs count) $ Just 1

  Spec.it s "CR 806.1 at three seats Relative Opponent folds BOTH opponents' zones" $ do
    -- From alice's perspective, a count of Swamps in an opponent's graveyard
    -- must fold bob's zone and carol's. DISCRIMINATING: the answer is 3, and
    -- every wrong reading gives a different number -- one opponent gives 1 or
    -- 2, and including the perspective gives 4. A two-seat board cannot
    -- separate those, which is why the sibling case above tops out at 1.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.threePlayers
        (a1, gs1) = S.addGraveyardCard swampPrinting S.alice gs0
        (b1, gs2) = S.addGraveyardCard swampPrinting S.bob gs1
        (c1, gs3) = S.addGraveyardCard swampPrinting S.carol gs2
        (c2, gs) = S.addGraveyardCard swampPrinting S.carol gs3
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Nothing),
              (b1, land, swamp, Nothing),
              (c1, land, swamp, Nothing),
              (c2, land, swamp, Nothing)
            ]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Graveyard (PlayerRef.Relative PlayerRelation.Opponent)))
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "bob's one plus carol's two, and none of alice's" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs count) $ Just 3

  -- CR 102.1 / CR 800.4a (#279). Asserted against playersFor directly AND
  -- through a Scope.OverPlayers count: through Scope.InZone the two cannot
  -- disagree observably, since a departing player's objects leave the game
  -- with them (CR 800.4a) and Game.zoneMembers already answered [] for every
  -- zone of theirs, but a scope that folds the PLAYERS charges one apiece.
  Spec.it s "CR 800.4a neither EachPlayer nor Opponent names a player who has left the game" $ do
    let gs = S.departs Departure.Type.Conceded S.carol S.threePlayerGame
        countOver ref = S.countOf (S.stubView []) (Filter.contextFor Teams.none (Just S.alice) Nothing) gs (Count.Type.MkCount (Scope.OverPlayers ref) (Filter.Type.And []) Aggregation.Members)
    Spec.assertEqWith
      s
      "EachPlayer names the two still in the game"
      (Count.playersFor (S.stubView []) (Filter.contextFor Teams.none Nothing Nothing) gs PlayerRef.EachPlayer)
      (Just [S.alice, S.bob])
    Spec.assertEqWith
      s
      "and from alice, carol is not an opponent either"
      (Count.playersFor (S.stubView []) (Filter.contextFor Teams.none (Just S.alice) Nothing) gs (PlayerRef.Relative PlayerRelation.Opponent))
      (Just [S.bob])
    -- The seating roster still has three (CR 800.5), so a count off it would
    -- say 3 and 2. These are the numbers only the still-playing reading gives.
    Spec.assertEqWith s "a count of the players in the game is 2" (countOver PlayerRef.EachPlayer) (Just 2)
    Spec.assertEqWith s "and of alice's opponents, 1" (countOver (PlayerRef.Relative PlayerRelation.Opponent)) (Just 1)

  Spec.it s "CR 102.1 OverPlayers folds the players themselves, not their objects" $ do
    -- The same three seats with NOBODY departed, which is what makes the case
    -- above a departure test rather than an arithmetic one: 3 and 2 here, 2
    -- and 1 there. The board is deckless and empty, so an OverPlayers arm that
    -- had folded a zone would answer 0 for every reference.
    let gs = S.threePlayerGame
        countOver ref = S.countOf (S.stubView []) (Filter.contextFor Teams.none (Just S.alice) Nothing) gs (Count.Type.MkCount (Scope.OverPlayers ref) (Filter.Type.And []) Aggregation.Members)
    Spec.assertEqWith s "three players in the game" (countOver PlayerRef.EachPlayer) (Just 3)
    Spec.assertEqWith s "CR 806.1 two of them are alice's opponents" (countOver (PlayerRef.Relative PlayerRelation.Opponent)) (Just 2)
    Spec.assertEqWith s "CR 109.5 and one of them is alice" (countOver (PlayerRef.Relative PlayerRelation.You)) (Just 1)
    -- Nothing rather than 0, the posture the InZone arm takes for the same
    -- unresolvable reference: who "you" are is unanswered, not answered empty.
    Spec.assertEqWith
      s
      "CR 109.5 with no perspective the reference is undeterminable"
      (S.countOf (S.stubView []) (Filter.contextFor Teams.none Nothing Nothing) gs (Count.Type.MkCount (Scope.OverPlayers (PlayerRef.Relative PlayerRelation.You)) (Filter.Type.And []) Aggregation.Members))
      Nothing

  Spec.it s "CR 109.5 Relative with no perspective is undeterminable" $ do
    let gs = Setup.emptyGame S.bothPlayers
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.Relative PlayerRelation.You)))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEq s (S.countOf (S.stubView []) (Filter.contextFor Teams.none Nothing Nothing) gs count) Nothing

  Spec.it s "CR 700.4 InHistory counts deaths from the event snapshot" $ do
    -- A battlefield -> graveyard move whose SNAPSHOT is a creature counts,
    -- and a graveyard -> exile move does not, whatever its snapshot says.
    let gs0 = Setup.emptyGame S.bothPlayers
        creatureSnapshot = S.emptyCharacteristics {PC.cardTypes = Set.singleton CardType.Creature}
        died = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Battlefield Zone.Graveyard) creatureSnapshot)
        exiled = GameEvent.Moved (Moved.moved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Graveyard Zone.Exile) creatureSnapshot)
        gs = S.withEvents [died, exiled] gs0
        count =
          Count.Type.MkCount
            (Scope.InHistory (EventShape.MovedBetween (MovedBetween.MkMovedBetween Zone.Battlefield Zone.Graveyard)))
            (Filter.Type.HasCardType CardType.Creature)
            Aggregation.Members
    Spec.assertEqWith s "one death" (S.countOf (S.stubView []) (Filter.contextFor Teams.none Nothing Nothing) gs count) $ Just 1

  Spec.it s "EachPlayer folds every player's copy" $ do
    -- The first case's board with the ControlledBy conjunct dropped: all
    -- three Swamps across both players count.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addPermanent swampPrinting S.alice gs0
        (a2, gs2) = S.addPermanent swampPrinting S.alice gs1
        (b1, gs) = S.addPermanent swampPrinting S.bob gs2
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (a2, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob)
            ]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "three" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs count) $ Just 3

  Spec.it s "Relative You resolves against the perspective" $ do
    -- The first case's board, read from Bob's perspective: Bob's own one
    -- Swamp.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addPermanent swampPrinting S.alice gs0
        (a2, gs2) = S.addPermanent swampPrinting S.alice gs1
        (b1, gs) = S.addPermanent swampPrinting S.bob gs2
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (a2, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob)
            ]
    Spec.assertEqWith s "one" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.bob) Nothing) gs swampsYouControl) $ Just 1

  Spec.it s "InSlot reads the bound player" $ do
    -- A source object binds Bob under the "target" slot (Sudden Impact's
    -- "that player's hand"); the count reads Bob's hand through the slot.
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs1) = S.addPermanent piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        bind obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer S.bob) (Object.bindings obj)}
        gs2 = gs1 {GameState.objects = Map.adjust bind srcId (GameState.objects gs1)}
        (h1, gs) = S.addHandCard lightningBolt S.bob gs2
        viewOf = S.stubView [(h1, Set.singleton CardType.Instant, Set.empty, Just S.bob)]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Hand (PlayerRef.InSlot slot)))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEqWith s "one card" (S.countOf viewOf (Filter.contextFor Teams.none Nothing (Just srcId)) gs count) $ Just 1

  -- The three cases below are Aggregation.Greatest at the fold, where the
  -- answers Pawl.ResolveSpec's One with the Machine cases cannot tell apart
  -- are visible: an undeterminable maximum is Nothing there and 0 draws
  -- here, but Nothing and Just 0 are different values.
  Spec.it s "CR 208.2a Greatest over an EMPTY matched set is Nothing, not 0" $ do
    -- No rule in the CR gives a maximum over nothing a value. CR 208.2a's
    -- "if the ability needs to use a number that can't be determined ... use
    -- 0 instead" is scoped to a characteristic-defining ability, and it is
    -- applied THERE (Pawl.Engine.Quantity.determine) rather than in this fold;
    -- where the CR does want an empty maximum to be 0 it otherwise legislates
    -- it card-shape by card-shape (CR 714.2d, a Saga with no chapter
    -- abilities). So the honest answer here is the one this codebase
    -- propagates everywhere else, and Pawl.PowerToughnessSpec's Monstrous
    -- War-Leech is where it becomes a 0. THE FALSIFIER for reaching for
    -- `maximum (0 : values)`.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (b1, gs) = S.addPermanent swampPrinting S.bob gs0
        land = Set.singleton CardType.Land
        viewOf = S.stubView [(b1, land, Set.singleton Subtype.Swamp, Just S.bob)]
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
            (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
            (Aggregation.Greatest Quantity.Type.ManaValue)
    -- Alice keeps none of Bob's one Swamp, so the fold has no members. The
    -- same count with Aggregation.Members is Just 0 -- a count of nothing IS
    -- zero -- which is exactly the answer a maximum must NOT borrow.
    Spec.assertEqWith s "no maximum" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs count) Nothing
    Spec.assertEqWith s "though counting the same empty set is 0" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs swampsYouControl) (Just 0)

  Spec.it s "CR 208.1 Greatest reads the PROJECTED power, not the printed one" $ do
    -- The one case here driven against a real projection rather than
    -- S.stubView, because the quantity being FOLDED is one only the
    -- projection supplies. Alice's board is a 1/1 Llanowar Elves, a 2/1
    -- Goblin Piker and a 3/3 War Mammoth: printed, the greatest power is 3.
    -- Two +1/+1 counters on the Piker (CR 122.1a / 613.4c) make it a 4/3,
    -- and the answer moves to 4 -- which no reading of the printed boxes
    -- gives.
    elves <- S.printingOf s registry "Llanowar Elves"
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, gs1) = S.addPermanent elves S.alice gs0
        (pikerId, gs2) = S.addPermanent piker S.alice gs1
        (_, printed) = S.addPermanent mammoth S.alice gs2
        pumped = S.addCounter CounterKind.PlusOnePlusOne 2 pikerId printed
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
            (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
            (Aggregation.Greatest Quantity.Type.Power)
        greatestPower g =
          S.countOf (\oid -> Just (Projection.viewOfObject oid g)) (Filter.contextFor Teams.none (Just S.alice) Nothing) g count
    Spec.assertEqWith s "the Mammoth's 3" (greatestPower printed) $ Just 3
    Spec.assertEqWith s "and the pumped Piker's 4" (greatestPower pumped) $ Just 4

  Spec.it s "a member whose quantity cannot be determined makes the whole maximum Nothing" $ do
    -- The Filter and the folded quantity are independent, so a card can ask
    -- for a value a kept member has no answer for -- here a power read off a
    -- Swamp (CR 208.3: a noncreature permanent has no power). Nothing
    -- propagates rather than the member being silently dropped, which would
    -- report the maximum of a set the card never named.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs) = S.addPermanent swampPrinting S.alice gs0
        count =
          Count.Type.MkCount
            (Scope.InZone (InZone.MkInZone Zone.Battlefield PlayerRef.EachPlayer))
            (Filter.Type.ControlledBy PlayerRelation.You)
            (Aggregation.Greatest Quantity.Type.Power)
        viewOf = S.stubView [(a1, Set.singleton CardType.Land, Set.singleton Subtype.Swamp, Just S.alice)]
    Spec.assertEqWith s "undeterminable" (S.countOf viewOf (Filter.contextFor Teams.none (Just S.alice) Nothing) gs count) Nothing

  approachSpec s registry
  tobiasSpec s registry
  charnelTallySpec s registry
  tyranidInvasionSpec s registry
  oreskosExplorerSpec s registry
  surveyorsScopeSpec s registry
  keeningStoneSpec s registry
  priceOfKnowledgeSpec s registry
  ebonyOwlNetsukeSpec s registry
  leftBattlefieldSpec s registry
  ownershipLedgerSpec s registry

-- CR 608.2i's look-back over a whole GAME rather than a turn
-- (EventShape.SpellCastThisGame). Approach of the Second Sun, {6}{W} Sorcery: "If
-- this spell was cast from your hand and you've cast another spell named
-- Approach of the Second Sun this game, you win the game. Otherwise, put
-- Approach of the Second Sun into its owner's library seventh from the top and
-- you gain 7 life." (Oracle text verified against Scryfall 2026-09-28.)
--
-- Every second cast happens on a LATER turn than the first, across real
-- handoffs, so a count that read only this turn's log answers 1 and loses the
-- win. Three seats, so bob's Approach is an opponent's and not "the other
-- player's". Future Sight sits under alice on every board, so the library-cast
-- case differs from the winning one only in where the second copy is.
approachSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
approachSpec s registry =
  let -- alice: fourteen Plains (two casts, no untap step between them), eight
      -- Plains in her library, Future Sight. bob: seven Plains. alice's main
      -- phase.
      board plains sight =
        let withLands = S.landsFor plains S.bob 7 (S.landsFor plains S.alice 14 S.threePlayerGame)
            stocked = List.foldl' (\g _ -> snd (S.addLibraryCard plains S.alice g)) withLands [1 .. (8 :: Int)]
            (_, withSight) = S.addPermanent sight S.alice stocked
         in mainOf S.alice withSight
      mainOf pid gs = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = pid, GameState.priority = Just pid}
      castAndResolve pid oid gs = S.runPure S.identityAnswer (S.runPure S.identityAnswer gs (S.cast pid oid)) Engine.priorityLoop
      -- One real turn handoff, then the main phase of whoever it reached.
      nextTurn gs =
        let handed = S.runPure S.identityAnswer gs Engine.handoffTurn
         in mainOf (GameState.activePlayer handed) handed
      -- 1-based, from the top: where each Approach sits in that library. By
      -- name, since CR 400.7 gives the card a new id as it moves.
      approachesIn pid gs =
        fmap (+ 1) (List.findIndices (\oid -> fmap S.nameOf (Game.cardOf oid gs) == Just approachName) (Game.zoneMembers Zone.Library pid gs))
      approachName = CardName.MkCardName (Text.pack "Approach of the Second Sun")
      printings = do
        plains <- S.printingOf s registry "Plains"
        sight <- S.printingOf s registry "Future Sight"
        approach <- S.printingOf s registry "Approach of the Second Sun"
        pure (plains, sight, approach)
   in Spec.describe s "Approach of the Second Sun" $ do
        Spec.it s "CR 601.2a the second, cast from the LIBRARY, does not win" $ do
          (plains, sight, approach) <- printings
          let (first, g1) = S.addHandCard approach S.alice (board plains sight)
              turnOne = castAndResolve S.alice first g1
              (second, g2) = S.addLibraryCard approach S.alice turnOne
              turnFour = nextTurn (nextTurn (nextTurn g2))
              after = castAndResolve S.alice second turnFour
          Spec.assertEqWith s "it is alice's turn again" (GameState.activePlayer turnFour) S.alice
          Spec.assertBool s (S.castable S.alice second turnFour) "Future Sight offers the top card"
          Spec.assertEqWith s "nobody has won" (GameState.result after) Nothing
          Spec.assertEqWith s "it went seventh from the top, above the first at eighth" (approachesIn S.alice after) [7, 8 :: Int]
          Spec.assertEqWith s "and alice gained a second 7" (S.lifeOf S.alice after) (Just 34)

-- CR 608.2i read over CR 608.2h's record of a ZONE CHANGE: "for each nontoken
-- creature you controlled that died this turn". GAMEPLAY LEVEL, because what it
-- proves is that the count reads what the move funnel filed as each permanent
-- ceased, so a stubbed ViewOf would prove nothing about the wiring.
--
-- Tobias, Doomed Conqueror, {2}{W}{U} Legendary Creature -- Human Soldier 3/2
-- with Flash: "When Tobias dies, for each nontoken creature you controlled that
-- died this turn, create a 2/2 black Zombie creature token."
--
-- BOTH halves of the filter are what this unit built: `ControlledBy You` needs
-- the controller CR 109.3 keeps out of the characteristics, and `Not IsToken`
-- needs the tokenhood CR 111.6 likewise does. Neither rides the snapshot; both
-- come off the CR 608.2h record filed under the departing id.
--
-- TOBIAS COUNTS ITSELF. Its own death is recorded before the ability it
-- triggers is put on the stack, let alone resolved, so "died this turn"
-- includes it -- which is why every expected count below is one more than the
-- deaths the case sets up.
--
-- THREE seats, so "you controlled" and "anyone controlled" are different
-- sentences, and alice's deaths outnumber bob's so the two readings cannot
-- coincide: 3 against 4 in the first case, 2 against 1 in the third.
tobiasSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tobiasSpec s registry =
  let zombie = CardName.MkCardName (Text.pack "Zombie Token")
      board tobias =
        let (tid, gs) = S.addPermanent tobias S.alice S.threePlayerGame
         in ( tid,
              gs
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- Lethal damage and a run of the priority loop, which settles CR 704.5g
      -- first and then resolves whatever the death triggered. One helper for
      -- every death here, so the Zombie-making death takes the same route as
      -- the deaths it counts.
      kill oid gs = S.runPure S.identityAnswer (S.markDamage oid 9 gs) Engine.priorityLoop
      zombies = S.countOnBattlefieldByName zombie S.alice
   in Spec.describe s "Tobias, Doomed Conqueror" $ do
        Spec.it s "CR 608.2h a look-back count reads who CONTROLLED each creature that died" $ do
          tobias <- S.printingOf s registry "Tobias, Doomed Conqueror"
          piker <- S.printingOf s registry "Goblin Piker"
          giant <- S.printingOf s registry "Hill Giant"
          let (tid, gs0) = board tobias
              (a1, gs1) = S.addPermanent piker S.alice gs0
              (a2, gs2) = S.addPermanent piker S.alice gs1
              (b1, gs3) = S.addPermanent giant S.bob gs2
              dead = kill b1 (kill a2 (kill a1 gs3))
              after = kill tid dead
          Spec.assertEqWith s "no Zombie before Tobias dies" (zombies dead) 0
          Spec.assertEqWith s "alice's two Pikers plus Tobias, and NOT bob's Giant" (zombies after) 3
          Spec.assertEqWith s "and bob gets none" (S.countOnBattlefieldByName zombie S.bob after) 0
          -- CR 111.4: the tokens are what the ability names, not just
          -- permanents. The length assertion keeps the two traversals from
          -- passing over an empty list.
          Spec.assertEqWith s "three tokens and nothing else" (length (S.tokensOf after)) 3
          mapM_ (\oid -> Spec.assertEqWith s "2/2" (S.powerToughnessOf oid after) (Just (2, 2))) (S.tokensOf after)
          mapM_ (\oid -> Spec.assertEqWith s "black" (Projection.colorsOf oid after) (Set.singleton Color.Black)) (S.tokensOf after)
        -- CR 111.6: "A token isn't a card." Doomed Traveler dies, its Spirit
        -- token is created and dies too, so alice has three deaths of which
        -- only two are nontoken -- 3 Zombies, not 4.
        Spec.it s "CR 111.6 a TOKEN that died is not counted" $ do
          tobias <- S.printingOf s registry "Tobias, Doomed Conqueror"
          piker <- S.printingOf s registry "Goblin Piker"
          traveler <- S.printingOf s registry "Doomed Traveler"
          let (tid, gs0) = board tobias
              (a1, gs1) = S.addPermanent piker S.alice gs0
              (a2, gs2) = S.addPermanent traveler S.alice gs1
              travelerDead = kill a2 (kill a1 gs2)
              spirit = S.tokensOf travelerDead
              dead = List.foldl' (flip kill) travelerDead spirit
              after = kill tid dead
          Spec.assertEqWith s "Doomed Traveler left exactly one Spirit token" (length spirit) 1
          Spec.assertEqWith s "no Spirit survives" (S.tokensOf dead) []
          Spec.assertEqWith s "Piker, Doomed Traveler and Tobias -- the Spirit is not counted" (zombies after) 3
        -- CR 110.2 / 613.1b: the PROJECTED controller as the object left, which
        -- is not its owner. bob's Giant dies under alice's control and counts
        -- for her; reading the owner instead gives 1.
        Spec.it s "CR 613.1b a creature STOLEN from bob counts for alice, who controlled it as it died" $ do
          tobias <- S.printingOf s registry "Tobias, Doomed Conqueror"
          giant <- S.printingOf s registry "Hill Giant"
          let (tid, gs0) = board tobias
              (b1, gs1) = S.addPermanent giant S.bob gs0
              stolen = S.giveControl b1 S.alice gs1
              after = kill tid (kill b1 stolen)
          Spec.assertEqWith s "bob's Giant and Tobias" (zombies after) 2

-- CR 122.1 / CR 608.2i: the counters an object HAD, read off a look-back count's
-- snapshot. CR 122.2 destroyed them as it left the battlefield and CR 613.4c had
-- already consumed them into the projected power the snapshot records, so the
-- only surviving record is CR 608.2h's, which Count.snapshotView's Moved arm
-- reads. GAMEPLAY LEVEL, since a stub has no CR 608.2h record to read.
--
-- Synthetic. Scryfall on 2026-08-27 finds no printing that counts the counters
-- on things that died: o:"died this turn" o:"counters on" returns eight cards,
-- every one of which PUTS counters, and o:"greatest number of counters" returns
-- none. Felisa, Fang of Silverquill and Angelic Sleuth read "the number of
-- counters it had on it" off the TRIGGER's own object, which reaches
-- Projection.viewWithLastKnownAnywhere through Binding.departedPermanent
-- instead -- a different question from this fold over the whole turn's deaths.
--
-- Synthetic Charnel Tally, {2}{B} Creature -- Zombie 2/2: "When this creature
-- dies, you gain X life, where X is the greatest number of counters among
-- creatures that died this turn."
--
-- THREE seats, and the biggest tally is on BOB's creature, so "creatures" and
-- "creatures you control" are different sentences. Every reading of the fold is
-- a different number: greatest 4, greatest among alice's 1, sum 5, members 3.
charnelTallySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
charnelTallySpec s registry =
  let withCounters n oid gs =
        gs
          { GameState.objects =
              Map.adjust
                (\obj -> obj {Object.counters = Map.insert CounterKind.PlusOnePlusOne n (Object.counters obj)})
                oid
                (GameState.objects gs)
          }
      board tally piker =
        let (tid, gs0) = S.addPermanent tally S.alice S.threePlayerGame
            (a1, gs1) = S.addPermanent piker S.alice gs0
            (b1, gs2) = S.addPermanent piker S.bob gs1
         in ( tid,
              a1,
              b1,
              gs2
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice,
                  GameState.priority = Just S.alice
                }
            )
      -- Tobias' helper, and for its reason: one route to every death here, so
      -- the tallying death takes the same road as the deaths it counts.
      kill oid gs = S.runPure S.identityAnswer (S.markDamage oid 9 gs) Engine.priorityLoop
   in Spec.describe s "Synthetic Charnel Tally" $ do
        Spec.it s "CR 608.2h X is the GREATEST counter tally among the creatures that died" $ do
          tally <- S.printingOf s registry "Synthetic Charnel Tally"
          piker <- S.printingOf s registry "Goblin Piker"
          let (tid, a1, b1, gs0) = board tally piker
              counted = withCounters 4 b1 (withCounters 1 a1 gs0)
              dead = kill b1 (kill a1 counted)
              after = kill tid dead
          -- CR 613.4c on the board, which is what makes the counters real rather
          -- than a field the fixture wrote and nothing reads: a 2/1 Piker under
          -- four +1/+1 counters is a 6/5.
          Spec.assertEqWith s "bob's Piker is a 6/5 under its four counters" (S.powerToughnessOf b1 counted) (Just (6, 5))
          Spec.assertEqWith s "no life gained before the Tally dies" (S.lifeOf S.alice dead) (Just 20)
          Spec.assertEqWith s "4, not 1, 3 or 5 -- bob's four counters, the greatest tally" (S.lifeOf S.alice after) (Just 24)
          Spec.assertEqWith s "the Tally's death triggered once" (length (filter isAbilityTriggered (S.eventsOf after))) 1
          Spec.assertEqWith s "three creatures died, so a member count would read 3" (length (filter isDeath (S.zoneChangesOf after))) 3
        -- The control the case above cannot be read without: a fold that answered
        -- some fixed nonzero tally would pass there and fail here. Same board,
        -- same three deaths, same trigger -- the counters are the one difference.
        Spec.it s "CR 122.1 creatures that died with NO counters tally 0" $ do
          tally <- S.printingOf s registry "Synthetic Charnel Tally"
          piker <- S.printingOf s registry "Goblin Piker"
          let (tid, a1, b1, gs0) = board tally piker
              dead = kill b1 (kill a1 gs0)
              after = kill tid dead
          Spec.assertEqWith s "no life at all, not the 4 the counted board gains" (S.lifeOf S.alice after) (Just 20)
          Spec.assertEqWith s "and the trigger did fire, so the 0 is the fold's answer" (length (filter isAbilityTriggered (S.eventsOf after))) 1
          Spec.assertEqWith s "the same three deaths" (length (filter isDeath (S.zoneChangesOf after))) 3

isDeath :: ZoneChange.ZoneChange -> Bool
isDeath zc = ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard

isAbilityTriggered :: GameEvent.GameEvent -> Bool
isAbilityTriggered event = case event of
  GameEvent.AbilityTriggered {} -> True
  _ -> False

-- CR 102.1 read as a NUMBER: the first count whose scope folds players rather
-- than objects. GAMEPLAY LEVEL, because what it proves is that a printed card
-- reaches the fold and that the fold reaches the seats the engine actually has.
--
-- Tyranid Invasion, {3}{G} Sorcery: "Create a number of 3/3 green Tyranid
-- Warrior creature tokens with trample equal to the number of opponents you
-- have." The whole card is one Create whose count is the scope under test, so
-- nothing else can move the answer.
--
-- THREE SEATS, and the number is OBSERVABLE as a pile of tokens rather than
-- computed. Two seats would answer 1 for the reading under test, for the
-- reading that ignores CR 800.4a, and for a literal alike. Three seats plus a
-- departure separate all three, and no two of these columns agree on both
-- rows:
--
--                      opponents still playing   players in the game   every seat but yours
--   nobody departed              2                       3                     2
--   carol conceded               1                       2                     2
--
-- So the two cases below differ in exactly one thing -- carol's concession --
-- and only the still-playing reading of CR 800.4a gives 2 then 1. The second
-- row is what a literal fails on, which is the mutation this pair was checked
-- against.
tyranidInvasionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
tyranidInvasionSpec s registry =
  let -- alice has four Forests and nothing else; bob and carol have empty
      -- boards, so every token on the battlefield afterwards is the spell's.
      board forest =
        let withLands = List.foldl' (\g _ -> snd (S.addPermanent forest S.alice g)) S.threePlayerGame [1 .. (4 :: Int)]
         in withLands
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castInvasion invasion gs =
        let (oid, gs1) = S.addHandCard invasion S.alice gs
            cast = S.runPure S.identityAnswer gs1 (S.cast S.alice oid)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
   in Spec.describe s "Tyranid Invasion" $ do
        -- The discriminating case for CR 800.4a. The seating roster is still
        -- three (CR 800.5) and carol's Status.Departed is the only difference
        -- from the board above, so a count that named seats rather than players
        -- would mint two again.
        Spec.it s "CR 800.4a a player who has conceded is not one of alice's opponents" $ do
          invasion <- S.printingOf s registry "Tyranid Invasion"
          forest <- S.printingOf s registry "Forest"
          let base = S.departs Departure.Type.Conceded S.carol (board forest)
              after = castInvasion invasion base
          Spec.assertEqWith s "one opponent left, so one token" (length (S.tokensOf after)) 1
          Spec.assertEqWith s "the spell resolved" (GameState.stack after) []

-- CR 110.2 compared against CR 109.5's "you": the first Filter that RE-FRAMES the
-- perspective, asking a question about a candidate player's own board rather than
-- how that player stands to you. GAMEPLAY LEVEL for the Tyranid Invasion group's
-- reason and for one more of its own -- the atom is answered by a rewrite inside
-- Pawl.Engine.Count.bakePerspective rather than by Pawl.Engine.Filter.matches, so
-- a unit-level match would read the vacuous False and prove nothing.
--
-- Oreskos Explorer, {1}{W} Cat Scout: "When this creature enters, search your
-- library for up to X Plains cards, where X is the number of players who control
-- more lands than you. Reveal those cards, put them into your hand, then
-- shuffle."
--
-- THREE SEATS with DISTINCT land counts, one seat above alice and one below, and
-- the pair of boards differs in exactly one thing -- carol's third land. No two
-- readings agree on both rows:
--
--                              CR 110.2 (>)   non-strict (>=)   every opponent   nobody
--   alice 2, bob 4, carol 3          2               3                2             0
--   alice 2, bob 4, carol 2          1               3                2             0
--
-- The tied row is the one that tells strict from non-strict, which is invisible
-- on a board where no seat ties you; the first row is what tells either from a
-- count of opponents. Alice's own 2 is distinct from both other seats, so a
-- reading that folded the wrong seat's board would not land on the right number
-- by coincidence.
--
-- X is OBSERVED as the size of alice's hand afterwards: her library holds three
-- Plains and one Forest, so the search is never capped by what is there to find
-- (a fourth reading, "as many as the library holds", would answer 3), and the
-- Forest is what proves the filter still ran.
oreskosExplorerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
oreskosExplorerSpec s registry =
  let board plains forest island carolLands =
        let withLands = S.landsFor island S.carol carolLands (S.landsFor forest S.bob 4 (S.landsFor plains S.alice 2 S.threePlayerGame))
            withLibrary = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) withLands [plains, plains, plains, forest]
         in withLibrary
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      castExplorer explorer gs =
        let (oid, gs1) = S.addHandCard explorer S.alice gs
            cast = S.runPure findsWhatItCan gs1 (S.cast S.alice oid)
         in S.runPure findsWhatItCan cast Engine.priorityLoop
      -- Alice cast the Explorer out of an otherwise empty hand, so what is in her
      -- hand afterwards is exactly what the search found.
      inHand gs = length (Game.zoneMembers Zone.Hand S.alice gs)
      plainsName = Set.singleton (CardName.MkCardName (Text.pack "Plains"))
   in Spec.describe s "Oreskos Explorer" $ do
        Spec.it s "CR 110.2 X counts the one seat with more lands than alice and the one with fewer" $ do
          explorer <- S.printingOf s registry "Oreskos Explorer"
          plains <- S.printingOf s registry "Plains"
          forest <- S.printingOf s registry "Forest"
          island <- S.printingOf s registry "Island"
          let after = castExplorer explorer (board plains forest island 3)
          Spec.assertEqWith s "bob and carol are both ahead, so two cards found" (inHand after) 2
          Spec.assertEqWith s "both were Plains, revealed" (S.revealsOf after) [(S.alice, plainsName), (S.alice, plainsName)]
          Spec.assertEqWith s "and the Forest and the third Plains stayed behind" (length (Game.zoneMembers Zone.Library S.alice after)) 2
          Spec.assertEqWith s "everything resolved" (GameState.stack after) []
        -- The discriminating case. Carol's board is the only difference, and CR
        -- 110.2's comparison is STRICT, so a seat level with alice is not one that
        -- controls more lands than she does.
        Spec.it s "CR 110.2 a seat TIED with alice controls no more lands than she does" $ do
          explorer <- S.printingOf s registry "Oreskos Explorer"
          plains <- S.printingOf s registry "Plains"
          forest <- S.printingOf s registry "Forest"
          island <- S.printingOf s registry "Island"
          let after = castExplorer explorer (board plains forest island 2)
          Spec.assertEqWith s "only bob is ahead, so one card found" (inHand after) 1
          Spec.assertEqWith s "and it was a Plains, revealed" (S.revealsOf after) [(S.alice, plainsName)]
          Spec.assertEqWith s "three cards left in the library" (length (Game.zoneMembers Zone.Library S.alice after)) 3
          Spec.assertEqWith s "everything resolved" (GameState.stack after) []

-- Finds as many matching cards as the search allows, off the head of the offered
-- list. It reads the engine's own cap, which is the number under test, rather
-- than searching for a card by name -- an answerer that picked by name would find
-- the same Plains again after a mutation and repair the assertion.
findsWhatItCan :: Prompt.Prompt r -> r
findsWhatItCan p = case p of
  Prompt.Search _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p

-- CR 110.2 with a MARGIN: the Oreskos Explorer group's atom asked for "at least
-- two more" rather than "more". GAMEPLAY LEVEL for that group's reason.
--
-- Surveyor's Scope, {2} Artifact: "{T}, Exile this artifact: Search your library
-- for up to X basic land cards, where X is the number of players who control at
-- least two more lands than you. Put those cards onto the battlefield, then
-- shuffle."
--
-- THREE SEATS: alice 2 lands, bob 4, carol 3 or 4, the pair of boards differing
-- in carol's fourth land alone:
--
--                              margin 2 (>=+2)   margin ignored (>)   nobody
--   alice 2, bob 4, carol 3          1                  2               0
--   alice 2, bob 4, carol 4          2                  2               0
--
-- Carol one ahead is the row that tells the margin from Oreskos's strict "more".
-- X is OBSERVED as the Forests alice controls afterwards: her library holds three
-- Forests and an Evolving Wilds, so the cap is never the library's, and the
-- nonbasic Wilds staying behind proves the basic filter still ran.
surveyorsScopeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
surveyorsScopeSpec s registry =
  let forestName = CardName.MkCardName (Text.pack "Forest")
      board carolLands = do
        scope <- S.printingOf s registry "Surveyor's Scope"
        plains <- S.printingOf s registry "Plains"
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        swamp <- S.printingOf s registry "Swamp"
        wilds <- S.printingOf s registry "Evolving Wilds"
        let withLands = S.landsFor swamp S.carol carolLands (S.landsFor island S.bob 4 (S.landsFor plains S.alice 2 S.threePlayerGame))
            withLibrary = List.foldl' (\g p -> snd (S.addLibraryCard p S.alice g)) withLands [forest, forest, forest, wilds]
            (scopeId, withScope) = S.addPermanent scope S.alice withLibrary
        pure (scopeId, withScope {GameState.priority = Just S.alice}, Face.activatedAbilities (S.combinedFace scope))
      fire scopeId ability gs = S.runPure findsWhatItCan gs (do Activate.activateAbility S.alice scopeId ability; Stack.resolveTop)
      forests = S.countOnBattlefieldByName forestName S.alice
      inLibrary gs = length (Game.zoneMembers Zone.Library S.alice gs)
   in Spec.describe s "Surveyor's Scope" $ do
        -- The discriminating case: carol is ahead by one, which Oreskos's strict
        -- "more" would count and "at least two more" does not.
        Spec.it s "CR 110.2 a seat ONE land ahead is not two more; the seat two ahead is" $ do
          (scopeId, ready, abilities) <- board 3
          case abilities of
            [] -> Spec.assertFailure s "Surveyor's Scope should declare one activated ability"
            ability : _ -> do
              let after = fire scopeId ability ready
              Spec.assertEqWith s "only bob is two ahead, so one Forest found" (forests after) 1
              Spec.assertEqWith s "the Wilds and two Forests stayed behind" (inLibrary after) 3
              Spec.assertEqWith s "everything resolved" (GameState.stack after) []
        Spec.it s "CR 110.2 two seats each two lands ahead fetch two" $ do
          (scopeId, ready, abilities) <- board 4
          case abilities of
            [] -> Spec.assertFailure s "Surveyor's Scope should declare one activated ability"
            ability : _ -> do
              let after = fire scopeId ability ready
              Spec.assertEqWith s "bob and carol are both two ahead, so two Forests found" (forests after) 2
              Spec.assertEqWith s "the Wilds and one Forest stayed behind" (inLibrary after) 2

-- CR 113.7 / CR 608.2c: a count whose SCOPE names the player an ABILITY's slot
-- bound. Pawl.Engine.Resolve.Slots.effectContext frames the resolution on the
-- ability's SOURCE, while its target is stamped on the ability object on the
-- stack, so Count.playersFor reading the source's own bindings found nothing
-- and the count came back unanswered -- a mill of nothing. A SPELL cannot show
-- it: its source and its stack object are one object. GAMEPLAY LEVEL, because
-- what is on trial is which object the resolution's slots are read off, which a
-- hand-built context would decide for the engine.
--
-- Keening Stone, {6} Artifact: "{5}, {T}: Target player mills X cards, where X is
-- the number of cards in that player's graveyard." Nothing else on the card, so
-- the target's graveyard is the only thing a case can be reading.
--
-- THREE SEATS with THREE DIFFERENT graveyards, because "that player", "you" and
-- "each player" are three sentences a two-seat board or an equal graveyard
-- collapses onto one. alice activates and holds 2 cards in her graveyard, bob --
-- the target -- 3 and carol 1, so the mill reads:
--
--   bob, the target       X = 3   graveyard 3 -> 6, library 10 -> 7
--   alice, the activator  X = 2
--   carol                 X = 1
--   every player at once  X = 6
--   unanswered            X = 0   graveyard 3,      library 10
--
-- Both milled libraries are stocked past the deepest of those readings, so the
-- library assertion measures the mill rather than CR 701.17b's floor at "as many
-- as possible".
keeningStoneSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
keeningStoneSpec s registry =
  let inZone zone pid gs = length (Game.zoneMembers zone pid gs)
      stock add printing pid n gs = List.foldl' (\g _ -> snd (add printing pid g)) gs [1 .. (n :: Int)]
      board = do
        stone <- S.printingOf s registry "Keening Stone"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        let (stoneId, withStone) = S.addPermanent stone S.alice (S.landsFor swamp S.alice 5 S.threePlayerGame)
            staged =
              stock S.addGraveyardCard piker S.carol 1
                . stock S.addGraveyardCard piker S.bob 3
                . stock S.addGraveyardCard piker S.alice 2
                . stock S.addLibraryCard piker S.alice 10
                $ stock S.addLibraryCard piker S.bob 10 withStone
        pure (stoneId, staged {GameState.priority = Just S.alice}, Face.activatedAbilities (S.combinedFace stone))
      fire stoneId ability target gs = S.runPure (aimedAtPlayer target) gs (do Activate.activateAbility S.alice stoneId ability; Stack.resolveTop)
   in Spec.describe s "Keening Stone" $ do
        Spec.it s "CR 113.7 X counts the graveyard of the ABILITY's target, not of its source's bindings" $ do
          (stoneId, ready, abilities) <- board
          case abilities of
            [] -> Spec.assertFailure s "Keening Stone should declare one activated ability"
            ability : _ -> do
              let after = fire stoneId ability S.bob ready
              Spec.assertEqWith s "bob's graveyard took the three cards the count read" (inZone Zone.Graveyard S.bob after) 6
              Spec.assertEqWith s "and his library is three shallower" (inZone Zone.Library S.bob after) 7
              Spec.assertEqWith s "alice's two-card graveyard was not the one counted" (inZone Zone.Graveyard S.alice after) 2
              Spec.assertEqWith s "nor carol's one" (inZone Zone.Graveyard S.carol after) 1
        -- The TARGET axis, on a board differing from the one above in the target
        -- alone: aimed at alice the same ability mills two, her graveyard's depth
        -- and not bob's. A count read off the source would answer the same number
        -- on both boards.
        Spec.it s "CR 113.7 the same ability aimed at alice mills TWO, her own graveyard's depth" $ do
          (stoneId, ready, abilities) <- board
          case abilities of
            [] -> Spec.assertFailure s "Keening Stone should declare one activated ability"
            ability : _ -> do
              let after = fire stoneId ability S.alice ready
              Spec.assertEqWith s "alice's graveyard took two" (inZone Zone.Graveyard S.alice after) 4
              Spec.assertEqWith s "and her library is two shallower" (inZone Zone.Library S.alice after) 8
              Spec.assertEqWith s "bob, untargeted, milled nothing" (inZone Zone.Graveyard S.bob after) 3

-- CR 601.2c: fill every target slot with the candidate naming `pid`. The offered
-- set is FILTERED rather than answered with a hand-built recipient, so CR 608.2b's
-- re-read at resolution still finds the target.
aimedAtPlayer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimedAtPlayer pid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring (== Recipient.ToPlayer pid) sets
  _ -> S.identityAnswer p

-- CR 603.2b / CR 113.7: the TRIGGERED half of the Keening Stone group above --
-- a count whose scope names the player the trigger's own event bound. The
-- binding is stamped on the ability object on the stack (Binding.triggerPlayer),
-- never on the enchantment CR 113.7 makes the source, so a read off the source
-- answered nothing here exactly as it did for an activated ability.
--
-- Price of Knowledge, {6}{B} Enchantment: "Players have no maximum hand size. /
-- At the beginning of each opponent's upkeep, this enchantment deals damage to
-- that player equal to the number of cards in that player's hand." The first
-- clause is a PlayerEffect and nothing here reads it.
--
-- THREE SEATS with THREE DIFFERENT hands, the Keening Stone group's reason:
-- alice controls the enchantment and holds 2 cards, bob takes the upkeep and
-- holds 3, carol holds 1. bob takes 3 and every other reading is a different
-- number -- 2 for the controller's hand, 1 for carol's, 6 for the whole table, 0
-- for a count that never answered.
--
-- Every library is stocked, since the upkeep runs through the priority loop and a
-- CR 104.3c decking would end the game before the assertion.
priceOfKnowledgeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
priceOfKnowledgeSpec s registry =
  let stock printing pid n gs = List.foldl' (\g _ -> snd (S.addHandCard printing pid g)) gs [1 .. (n :: Int)]
      library printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      board = do
        price <- S.printingOf s registry "Price of Knowledge"
        piker <- S.printingOf s registry "Goblin Piker"
        let (_, withPrice) = S.addPermanent price S.alice S.threePlayerGame
        pure
          ( library piker S.carol 10
              . library piker S.bob 10
              . library piker S.alice 10
              . stock piker S.carol 1
              . stock piker S.bob 3
              $ stock piker S.alice 2 withPrice
          )
      upkeepOf pid gs =
        let upkeep = Phase.Beginning BeginningStep.Upkeep
            began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = S.runPure S.identityAnswer began Engine.settleForPriority
         in S.runPure S.identityAnswer settled Engine.priorityLoop
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Price of Knowledge" $ do
        Spec.it s "CR 603.2b the damage counts the hand of the player the TRIGGER bound" $ do
          gs <- board
          let after = upkeepOf S.bob gs
          Spec.assertEqWith s "bob took 3: his own three cards, not alice's two" (lives after) (Just 20, Just 17, Just 20)
          Spec.assertEqWith s "bob still holds the three the count read" (S.handSize S.bob after) 3
          Spec.assertEqWith s "alice's two are nobody's business here" (S.handSize S.alice after) 2
          Spec.assertEqWith s "nor carol's one" (S.handSize S.carol after) 1

-- CR 603.4 with CR 113.7: priceOfKnowledgeSpec's question asked of the
-- INTERVENING "if" rather than of the resolution -- a condition whose count
-- scopes over the hand of the player the trigger's own event bound. Both checks
-- of the clause read it, CR 603.4's at trigger time
-- (Pawl.Engine.Event.Trigger.interveningHolds) and CR 608.2a's re-check as the
-- ability resolves (Pawl.Engine.Stack.interveningStillHolds), so a board that
-- reaches resolution drives both.
--
-- Ebony Owl Netsuke, {2} Artifact: "At the beginning of each opponent's upkeep,
-- if that player has seven or more cards in hand, this artifact deals 4 damage
-- to that player."
--
-- THREE SEATS and TWO BOARDS differing only in the hands, because the reading
-- this exists to exclude is the SOURCE's controller: the artifact is alice's,
-- and on each board alice's hand and bob's answer the clause differently. The
-- first holds bob over the threshold while alice is under it, the second the
-- other way round, and the 4 damage lands on bob in the first and on nobody in
-- the second. A read that never answered at all is a third reading, and it is
-- the first board that excludes it.
--
-- Every library is stocked, priceOfKnowledgeSpec's CR 104.3c reason.
ebonyOwlNetsukeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ebonyOwlNetsukeSpec s registry =
  let stock printing pid n gs = List.foldl' (\g _ -> snd (S.addHandCard printing pid g)) gs [1 .. (n :: Int)]
      library printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      boardWith aliceHand bobHand = do
        netsuke <- S.printingOf s registry "Ebony Owl Netsuke"
        piker <- S.printingOf s registry "Goblin Piker"
        let (_, withNetsuke) = S.addPermanent netsuke S.alice S.threePlayerGame
        pure
          ( library piker S.carol 12
              . library piker S.bob 12
              . library piker S.alice 12
              . stock piker S.carol 2
              . stock piker S.bob bobHand
              $ stock piker S.alice aliceHand withNetsuke
          )
      upkeepOf pid gs =
        let upkeep = Phase.Beginning BeginningStep.Upkeep
            began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan upkeep pid)) (gs {GameState.phase = upkeep, GameState.activePlayer = pid})
            settled = S.runPure S.identityAnswer began Engine.settleForPriority
         in S.runPure S.identityAnswer settled Engine.priorityLoop
      lives gs = (S.lifeOf S.alice gs, S.lifeOf S.bob gs, S.lifeOf S.carol gs)
   in Spec.describe s "Ebony Owl Netsuke" $ do
        Spec.it s "CR 603.4 the intervening if reads the hand of the player the TRIGGER bound" $ do
          gs <- boardWith 3 9
          let after = upkeepOf S.bob gs
          Spec.assertEqWith s "bob took 4: his own nine cards cleared the threshold, though alice's three do not" (lives after) (Just 20, Just 16, Just 20)
          Spec.assertEqWith s "bob still holds the nine the count read" (S.handSize S.bob after) 9
          Spec.assertEqWith s "and alice the three that would have answered the other way" (S.handSize S.alice after) 3

-- CR 108.3's owner over CR 608.2i's cast log, which is not CR 405.4's
-- controller.
--
-- Synthetic Ownership Ledger, {2}{B} Creature -- Zombie 1/3: "{T}: You gain 1
-- life for each spell you don't own that was cast this turn." SYNTHETIC because
-- no printing asks an owner question of a PAST cast: Scryfall's o:"spell you
-- don't own", 2026-09-14, returns seven cards and every one of them is a
-- "whenever you cast" TRIGGER, which Pawl.Engine.Event.Match answers off the live
-- spell instead. Tasha, the Witch Queen is the card that would refute this if her
-- clause were a count.
--
-- Both cases run on Pawl.CastRestrictionSpec's Dire Fleet Daredevil boards,
-- reused for the reason Pawl.DepartureSpec reuses one: on them a spell's caster
-- and its owner are different players, and three seats keep the two apart.
--
-- The first board logs two casts and alice made both -- the Daredevil out of her
-- own hand, and bob's Renewed Faith out of bob's graveyard -- so the life the
-- Ledger gains reads:
--
--   owner, the rule            1   only the Faith is bob's
--   controller instead         0   alice cast both
--   no owner at all            0   Filter.OwnedBy vacuously False, the bug
--   neither conjunct           2   every cast counted
--
-- The second case is castOwner's other road, and the board it needs is the one
-- with a second Renewed Faith in alice's OWN hand: a spell still on the stack has
-- no CR 608.2h record filed under its id, so a fold that reads only records
-- would answer Nothing for it and count a spell alice owns.
ownershipLedgerSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
ownershipLedgerSpec s registry =
  let printings = do
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        daredevil <- S.printingOf s registry "Dire Fleet Daredevil"
        faith <- S.printingOf s registry "Renewed Faith"
        ledger <- S.printingOf s registry "Synthetic Ownership Ledger"
        pure (mountain, plains, daredevil, faith, ledger)
      -- The Ledger put onto alice's battlefield and tapped for its own ability,
      -- which S.addPermanent settles so CR 302.6 does not refuse the cost.
      tallied ledger gs =
        let (ledgerId, withLedger) = S.addPermanent ledger S.alice gs
         in case Projection.abilitiesOf ledgerId withLedger of
              [ability] -> Right (S.lifeOf S.alice (S.runPure S.identityAnswer withLedger (do Activate.activateAbility S.alice ledgerId ability; Stack.resolveTop)))
              other -> Left (length other)
      owners gs = fmap (\oid -> fmap Object.owner (Game.lookupObject oid gs)) (GameState.stack gs)
   in Spec.describe s "Synthetic Ownership Ledger" $ do
        -- The other road, on the board that adds a SECOND Renewed Faith to alice's
        -- own hand: she casts bob's and lets it resolve, then casts hers and
        -- leaves it on the stack. A spell still on the stack has no CR 608.2h
        -- record filed under its id, so the one alice owns is excluded only by
        -- the live read -- and the board would answer 28 without it.
        Spec.it s "CR 108.3 a spell still on the stack is read off the object, not off a record" $ do
          (mountain, plains, daredevil, faith, ledger) <- printings
          let (handFaithId, board) = CastSpec.daredevilTwoFaiths mountain plains daredevil faith
          case CastSpec.exiledNamed CastSpec.renewedFaithName board of
            [exiledId] -> do
              let resolved = S.runPure S.identityAnswer (S.runPure S.identityAnswer board (S.cast S.alice exiledId)) Stack.resolveTop
                  pending = S.runPure S.identityAnswer resolved (S.cast S.alice handFaithId)
              Spec.assertEqWith s "CR 108.3 alice's own spell on the stack is not one she doesn't own, so still 1" (tallied ledger pending) (Right (Just 27))
              -- The proxies, after the behaviour: her Faith really is on the
              -- stack unresolved, and bob's really did resolve.
              Spec.assertEqWith s "setup: one spell on the stack, and alice owns it" (owners pending) [Just S.alice]
              Spec.assertEqWith s "setup: alice is at 26 from bob's Faith alone" (S.lifeOf S.alice pending) (Just 26)
            other -> Spec.assertFailure s ("expected exactly one exiled Faith, got " <> show (length other))

-- CR 603.6c's origin with no destination: "left the battlefield this turn", read
-- over EventShape.MovedFrom. Every victim here goes to its owner's HAND, so a
-- shape that still named a graveyard destination would see none of them.
--
-- Insatiable Skittermaw {2}{B} Creature -- Insect Horror 2/2 (Oracle text checked
-- on Scryfall, 2026-09-25): "Void -- At the beginning of your end step, if a
-- nonland permanent left the battlefield this turn or a spell was warped this
-- turn, put a +1/+1 counter on this creature." Pawl.CastSpec's Warp group drives
-- the second disjunct. Its boards differ in which permanent leaves: bob's Hill
-- Giant or his Mountain, bounced, or carol's Hill Giant as carol concedes (CR
-- 800.4a, which is CR 603.6c's other road off the battlefield).
--
-- Minthara, Merciless Soul (Oracle text checked on Scryfall, 2026-09-25): "At the
-- beginning of your end step, if a permanent you controlled left the battlefield
-- this turn, you get an experience counter." Its pair differs in who CONTROLS
-- bob's Hill Giant as it is bounced, so an owner read answers neither case.
leftBattlefieldSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
leftBattlefieldSpec s registry =
  let endStep = Phase.Ending EndingStep.EndStep
      library printing pid n gs = List.foldl' (\g _ -> snd (S.addLibraryCard printing pid g)) gs [1 .. (n :: Int)]
      -- As Raphael's (data/scenarios/count): the step's event recorded beside
      -- the phase.
      endStepOf gs =
        let began = Event.recordEvent (GameEvent.StepBegan (StepBegan.MkStepBegan endStep S.alice)) (gs {GameState.phase = endStep, GameState.activePlayer = S.alice})
            settled = S.runPure S.identityAnswer began Engine.settleForPriority
         in S.runPure S.identityAnswer settled Engine.priorityLoop
      bounce oid gs = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Hand)
      board producer = do
        watcher <- S.printingOf s registry producer
        hillGiant <- S.printingOf s registry "Hill Giant"
        mountain <- S.printingOf s registry "Mountain"
        let (watcherId, withWatcher) = S.addPermanent watcher S.alice S.threePlayerGame
            (theirGiant, withTheirs) = S.addPermanent hillGiant S.bob withWatcher
            (theirMountain, withMountain) = S.addPermanent mountain S.bob withTheirs
            (_, withCarols) = S.addPermanent hillGiant S.carol withMountain
            stocked = library mountain S.carol 5 (library mountain S.bob 5 (library mountain S.alice 5 withCarols))
            ready = stocked {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
        pure (watcherId, theirGiant, theirMountain, ready)
   in Spec.describe s "left the battlefield this turn" $ do
        Spec.it s "CR 603.6c Insatiable Skittermaw counts a nonland permanent bounced or leaving the game with its owner, and not a land" $ do
          (skittermawId, theirGiant, theirMountain, ready) <- board "Insatiable Skittermaw"
          let counters = S.counterOf CounterKind.PlusOnePlusOne skittermawId . endStepOf
              concede gs = S.runPure S.identityAnswer gs (Departure.leaveGame Departure.Type.Conceded S.carol)
          Spec.assertEqWith
            s
            "CR 603.6c bob's Hill Giant bounced and carol's leaving the game each make the counter; bob's Mountain bounced does not"
            (counters (bounce theirGiant ready), counters (concede ready), counters (bounce theirMountain ready))
            (1, 1, 0)
          Spec.assertEqWith s "setup: both went to bob's hand" (length (Game.zoneMembers Zone.Hand S.bob (bounce theirMountain (bounce theirGiant ready)))) 2
        -- CR 111.3 / 603.6c: a token ENTERING is logged as a Battlefield to
        -- Battlefield move (Event.recordMintedEntry), and nothing left.
        Spec.it s "CR 603.6c a nonland token entering is no permanent leaving, so Insatiable Skittermaw gets no counter" $ do
          (skittermawId, _, _, ready) <- board "Insatiable Skittermaw"
          hillGiant <- S.printingOf s registry "Hill Giant"
          let minted = S.runPure S.identityAnswer ready (Event.createTokens S.bob (Printing.card hillGiant) Nothing 1 TapState.Untapped Map.empty Nothing)
          Spec.assertEqWith s "CR 603.6c a Hill Giant token entered and nothing left, so no counter" (S.counterOf CounterKind.PlusOnePlusOne skittermawId (endStepOf minted)) 0
          Spec.assertEqWith s "setup: the token is on the battlefield beside bob's Hill Giant" (length (Game.zoneMembers Zone.Battlefield S.bob minted)) 3
        Spec.it s "CR 603.6c Minthara counts a permanent alice controlled leaving, not the same one while bob controlled it" $ do
          (_, theirGiant, _, ready) <- board "Minthara, Merciless Soul"
          let experience = S.playerCounterOf PlayerCounterKind.Experience S.alice . endStepOf
              stolen = S.giveControl theirGiant S.alice ready
          Spec.assertEqWith
            s
            "CR 603.6c bob's Hill Giant under alice's control gets her an experience counter as it leaves; under bob's it does not"
            (experience (bounce theirGiant stolen), experience (bounce theirGiant ready))
            (1, 0)
        -- CR 729.4a's crossing records the zone the card left, and only a
        -- battlefield one is a permanent leaving the battlefield. The pair differs
        -- in where bob's Hill Giant sits as a subgame bob plays takes it.
        Spec.it s "CR 603.6c/729.4a Insatiable Skittermaw counts a Hill Giant a subgame takes from the battlefield, and not one it takes from a graveyard" $ do
          (skittermawId, theirGiant, _, ready) <- board "Insatiable Skittermaw"
          hillGiant <- S.printingOf s registry "Hill Giant"
          let (buriedGiant, withBuried) = S.addGraveyardCard hillGiant S.bob ready
              cross oid gs = Setup.applyCrossings (snd (Event.bringInFrom OutsideDestination.Hand S.bob oid (Setup.subgameStateFrom S.bob gs))) gs
              counters = S.counterOf CounterKind.PlusOnePlusOne skittermawId . endStepOf
          Spec.assertEqWith
            s
            "CR 603.6c the battlefield Hill Giant leaving makes the counter; the graveyard one does not"
            (counters (cross theirGiant withBuried), counters (cross buriedGiant withBuried))
            (1, 0)
          Spec.assertEqWith s "setup: each crossing took its card out of the main game" (Map.member theirGiant (GameState.objects (cross theirGiant withBuried)), Map.member buriedGiant (GameState.objects (cross buriedGiant withBuried))) (False, False)
