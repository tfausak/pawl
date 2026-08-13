{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Count, Pawl.Types.Count, Pawl.Types.Scope, Pawl.Types.PlayerRef,
-- Pawl.Types.EventShape and Pawl.Types.Aggregation. Unit-level: the fold is driven
-- against a stubbed ViewOf so the evaluator is tested apart from the projection
-- that supplies it (Pawl.PowerToughnessSpec covers the wiring). Two exceptions,
-- each of which says so where it sits: the Aggregation.Greatest case that folds
-- a PROJECTED power, since a stub has no power to read; and the Aetherflux
-- Reservoir group at the foot of the module, which is gameplay level because
-- what it proves is that a count folds what Pawl.Engine.Cast actually recorded.
module Pawl.CountSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

swampsYouControl :: Count.Type.Count Quantity.Type.Quantity
swampsYouControl =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
    Aggregation.Members

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Count" $ do
  Spec.it s "Members counts the matching members of a zone" $ do
    -- Two Swamps Alice controls, one Bob controls; ControlledBy You keeps
    -- Alice's two.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addCreature swampPrinting S.alice gs0
        (a2, gs2) = S.addCreature swampPrinting S.alice gs1
        (b1, gs) = S.addCreature swampPrinting S.bob gs2
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (a2, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob)
            ]
    Spec.assertEq s (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs swampsYouControl) $ Just 2

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
            (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
            (Filter.Type.And [])
            Aggregation.DistinctCardTypes
    Spec.assertEqWith s "two types" (S.countOf viewOf (Filter.contextFor Nothing Nothing) gs count) $ Just 2

  Spec.it s "CR 102.2 Relative Opponent excludes the perspective" $ do
    -- The same board as the first case, read from Bob's perspective: his
    -- opponent Alice controls two Swamps.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addCreature swampPrinting S.alice gs0
        (b1, gs) = S.addCreature swampPrinting S.bob gs1
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf = S.stubView [(a1, land, swamp, Just S.alice), (b1, land, swamp, Just S.bob)]
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "Alice's one" (S.countOf viewOf (Filter.contextFor (Just S.bob) Nothing) gs count) $ Just 1

  Spec.it s "CR 806.1 at three seats Relative Opponent folds BOTH opponents' zones" $ do
    -- Nightmare's shape (a count of Swamps you control) read from the OTHER
    -- side: from alice's perspective, a count of Swamps an opponent controls
    -- must fold bob's zone and carol's. DISCRIMINATING: the answer is 3, and
    -- every wrong reading gives a different number -- one opponent gives 1 or
    -- 2, and including the perspective gives 4. A two-seat board cannot
    -- separate those, which is why the sibling case above tops out at 1.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.threePlayers
        (a1, gs1) = S.addCreature swampPrinting S.alice gs0
        (b1, gs2) = S.addCreature swampPrinting S.bob gs1
        (c1, gs3) = S.addCreature swampPrinting S.carol gs2
        (c2, gs) = S.addCreature swampPrinting S.carol gs3
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob),
              (c1, land, swamp, Just S.carol),
              (c2, land, swamp, Just S.carol)
            ]
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "bob's one plus carol's two, and none of alice's" (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs count) $ Just 3

  -- CR 102.1 / CR 800.4a (#279). Asserted against playersFor directly AND
  -- through a Scope.OverPlayers count: through Scope.InZone the two cannot
  -- disagree observably, since a departing player's objects leave the game
  -- with them (CR 800.4a) and Game.zoneMembers already answered [] for every
  -- zone of theirs, but a scope that folds the PLAYERS charges one apiece.
  Spec.it s "CR 800.4a neither EachPlayer nor Opponent names a player who has left the game" $ do
    let gs = Departure.depart Departure.Type.Conceded S.carol S.threePlayerGame
        countOver ref = S.countOf (S.stubView []) (Filter.contextFor (Just S.alice) Nothing) gs (Count.Type.MkCount (Scope.OverPlayers ref) (Filter.Type.And []) Aggregation.Members)
    Spec.assertEqWith
      s
      "EachPlayer names the two still in the game"
      (Count.playersFor (Filter.contextFor Nothing Nothing) gs PlayerRef.EachPlayer)
      (Just [S.alice, S.bob])
    Spec.assertEqWith
      s
      "and from alice, carol is not an opponent either"
      (Count.playersFor (Filter.contextFor (Just S.alice) Nothing) gs (PlayerRef.Relative PlayerRelation.Opponent))
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
        countOver ref = S.countOf (S.stubView []) (Filter.contextFor (Just S.alice) Nothing) gs (Count.Type.MkCount (Scope.OverPlayers ref) (Filter.Type.And []) Aggregation.Members)
    Spec.assertEqWith s "three players in the game" (countOver PlayerRef.EachPlayer) (Just 3)
    Spec.assertEqWith s "CR 806.1 two of them are alice's opponents" (countOver (PlayerRef.Relative PlayerRelation.Opponent)) (Just 2)
    Spec.assertEqWith s "CR 109.5 and one of them is alice" (countOver (PlayerRef.Relative PlayerRelation.You)) (Just 1)
    -- Nothing rather than 0, the posture the InZone arm takes for the same
    -- unresolvable reference: who "you" are is unanswered, not answered empty.
    Spec.assertEqWith
      s
      "CR 109.5 with no perspective the reference is undeterminable"
      (S.countOf (S.stubView []) (Filter.contextFor Nothing Nothing) gs (Count.Type.MkCount (Scope.OverPlayers (PlayerRef.Relative PlayerRelation.You)) (Filter.Type.And []) Aggregation.Members))
      Nothing

  Spec.it s "CR 109.5 Relative with no perspective is undeterminable" $ do
    let gs = Setup.emptyGame S.bothPlayers
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEq s (S.countOf (S.stubView []) (Filter.contextFor Nothing Nothing) gs count) Nothing

  Spec.it s "CR 700.4 InHistory counts deaths from the event snapshot" $ do
    -- A battlefield -> graveyard move whose SNAPSHOT is a creature counts,
    -- and a graveyard -> exile move does not, whatever its snapshot says.
    let gs0 = Setup.emptyGame S.bothPlayers
        creatureSnapshot = S.emptyCharacteristics {PC.cardTypes = Set.singleton CardType.Creature}
        died = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Battlefield Zone.Graveyard) creatureSnapshot)
        exiled = GameEvent.Moved (Moved.MkMoved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Graveyard Zone.Exile) creatureSnapshot)
        gs = S.withEvents [died, exiled] gs0
        count =
          Count.Type.MkCount
            (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
            (Filter.Type.HasCardType CardType.Creature)
            Aggregation.Members
    Spec.assertEqWith s "one death" (S.countOf (S.stubView []) (Filter.contextFor Nothing Nothing) gs count) $ Just 1

  Spec.it s "EachPlayer folds every player's copy" $ do
    -- The first case's board with the ControlledBy conjunct dropped: all
    -- three Swamps across both players count.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addCreature swampPrinting S.alice gs0
        (a2, gs2) = S.addCreature swampPrinting S.alice gs1
        (b1, gs) = S.addCreature swampPrinting S.bob gs2
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
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.HasSubtype Subtype.Swamp)
            Aggregation.Members
    Spec.assertEqWith s "three" (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs count) $ Just 3

  Spec.it s "Relative You resolves against the perspective" $ do
    -- The first case's board, read from Bob's perspective: Bob's own one
    -- Swamp.
    swampPrinting <- S.printingOf s registry "Swamp"
    let gs0 = Setup.emptyGame S.bothPlayers
        (a1, gs1) = S.addCreature swampPrinting S.alice gs0
        (a2, gs2) = S.addCreature swampPrinting S.alice gs1
        (b1, gs) = S.addCreature swampPrinting S.bob gs2
        swamp = Set.singleton Subtype.Swamp
        land = Set.singleton CardType.Land
        viewOf =
          S.stubView
            [ (a1, land, swamp, Just S.alice),
              (a2, land, swamp, Just S.alice),
              (b1, land, swamp, Just S.bob)
            ]
    Spec.assertEqWith s "one" (S.countOf viewOf (Filter.contextFor (Just S.bob) Nothing) gs swampsYouControl) $ Just 1

  Spec.it s "InSlot reads the bound player" $ do
    -- A source object binds Bob under the "target" slot (Sudden Impact's
    -- "that player's hand"); the count reads Bob's hand through the slot.
    piker <- S.printingOf s registry "Goblin Piker"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let gs0 = Setup.emptyGame S.bothPlayers
        (srcId, gs1) = S.addCreature piker S.alice gs0
        slot = SlotName.MkSlotName (Text.pack "target")
        bind obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer S.bob) (Object.bindings obj)}
        gs2 = gs1 {GameState.objects = Map.adjust bind srcId (GameState.objects gs1)}
        (h1, gs) = S.addHandCard lightningBolt S.bob gs2
        viewOf = S.stubView [(h1, Set.singleton CardType.Instant, Set.empty, Just S.bob)]
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Hand (PlayerRef.InSlot slot))
            (Filter.Type.And [])
            Aggregation.Members
    Spec.assertEqWith s "one card" (S.countOf viewOf (Filter.contextFor Nothing (Just srcId)) gs count) $ Just 1

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
        (b1, gs) = S.addCreature swampPrinting S.bob gs0
        land = Set.singleton CardType.Land
        viewOf = S.stubView [(b1, land, Set.singleton Subtype.Swamp, Just S.bob)]
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
            (Aggregation.Greatest Quantity.Type.ManaValue)
    -- Alice keeps none of Bob's one Swamp, so the fold has no members. The
    -- same count with Aggregation.Members is Just 0 -- a count of nothing IS
    -- zero -- which is exactly the answer a maximum must NOT borrow.
    Spec.assertEqWith s "no maximum" (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs count) Nothing
    Spec.assertEqWith s "though counting the same empty set is 0" (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs swampsYouControl) (Just 0)

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
        (_, gs1) = S.addCreature elves S.alice gs0
        (pikerId, gs2) = S.addCreature piker S.alice gs1
        (_, printed) = S.addCreature mammoth S.alice gs2
        pumped = S.addCounter CounterKind.PlusOnePlusOne 2 pikerId printed
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.And [Filter.Type.HasCardType CardType.Creature, Filter.Type.ControlledBy PlayerRelation.You])
            (Aggregation.Greatest Quantity.Type.Power)
        greatestPower g =
          S.countOf (\oid -> Just (Projection.viewOfObject oid g)) (Filter.contextFor (Just S.alice) Nothing) g count
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
        (a1, gs) = S.addCreature swampPrinting S.alice gs0
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
            (Filter.Type.ControlledBy PlayerRelation.You)
            (Aggregation.Greatest Quantity.Type.Power)
        viewOf = S.stubView [(a1, Set.singleton CardType.Land, Set.singleton Subtype.Swamp, Just S.alice)]
    Spec.assertEqWith s "undeterminable" (S.countOf viewOf (Filter.contextFor (Just S.alice) Nothing) gs count) Nothing

  aetherfluxReservoirSpec s registry
  tobiasSpec s registry
  roothaSpec s registry
  tyranidInvasionSpec s registry
  oreskosExplorerSpec s registry

-- CR 608.2i read over CR 601.2i's event: "for each spell you've cast this
-- turn", the first count whose scope is a shape of event that is NOT a zone
-- change. GAMEPLAY LEVEL, unlike the rest of this module -- the whole point is
-- that the count sees what Pawl.Engine.Cast recorded, so a stubbed ViewOf would
-- prove nothing about the wiring, and the trigger, the log and the fold have to
-- meet.
--
-- Aetherflux Reservoir, {4} Artifact: "Whenever you cast a spell, you gain 1 life
-- for each spell you've cast this turn." Its second ability (Pay 50 life: this
-- artifact deals 50 damage to any target) is on the card and deliberately never
-- activated here -- S.identityAnswer takes no action at all -- so a stray
-- activation would show up as a 50-point swing rather than hiding.
--
-- WHY THE COUNT IS CUMULATIVE AND NOT FLAT. 1 + 2 + 3 = 6, and so does a flat
-- 2-per-cast; the RUNNING TOTAL after each cast is what tells the two apart, so
-- every case below asserts after every cast rather than at the end.
--
-- THE TRIGGERING SPELL COUNTS ITSELF. CR 601.2i records the cast and fires the
-- trigger in that order -- the spell "becomes cast", THEN abilities that trigger
-- on a cast trigger -- so the event is already in the log before the ability is
-- even put on the stack, let alone resolved. The first cast gaining 1 rather
-- than 0 is the assertion that proves it.
--
-- Fog, {G} Instant, is the spell cast: one mana, no targets, and its CR 615
-- combat-damage prevention has nothing to act on outside combat, so nothing but
-- the life total moves. THREE seats, so "an opponent cast it" and "bob cast it"
-- are different sentences (Pawl.TriggerSpec's Young Pyromancer group makes the
-- same argument).
aetherfluxReservoirSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
aetherfluxReservoirSpec s registry =
  let -- alice has the Reservoir and six Forests, bob two Forests, carol nothing.
      -- Six covers the four Fogs the turn-boundary case casts, none of which
      -- untaps: no untap step runs between them.
      board forest reservoir =
        let addLands pid n g = List.foldl' (\g' _ -> snd (S.addCreature forest pid g')) g [1 .. (n :: Int)]
            withLands = addLands S.bob 2 (addLands S.alice 6 S.threePlayerGame)
            (_, withReservoir) = S.addCreature reservoir S.alice withLands
         in withReservoir
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      -- One Fog into `caster`'s hand, cast, and the stack run down -- which
      -- resolves both the Reservoir trigger and the Fog itself.
      castFog fog caster gs =
        let (oid, gs1) = S.addHandCard fog caster gs
            cast = S.runPure S.identityAnswer gs1 (S.cast caster oid)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      lifeChange base gs pid = do
        before <- S.lifeOf pid base
        after <- S.lifeOf pid gs
        pure (after - before)
   in Spec.describe s "Aetherflux Reservoir" $ do
        Spec.it s "CR 608.2i three casts in one turn gain 1, then 2, then 3" $ do
          forest <- S.printingOf s registry "Forest"
          reservoir <- S.printingOf s registry "Aetherflux Reservoir"
          fog <- S.printingOf s registry "Fog"
          let base = board forest reservoir
              one = castFog fog S.alice base
              two = castFog fog S.alice one
              three = castFog fog S.alice two
              gained = lifeChange base
          Spec.assertEqWith s "the first cast counts ITSELF, so 1" (gained one S.alice) (Just 1)
          Spec.assertEqWith s "the second sees two casts, so 3 in total" (gained two S.alice) (Just 3)
          Spec.assertEqWith s "the third sees three casts, so 6 in total" (gained three S.alice) (Just 6)
        -- The "you" half of BOTH filters -- the trigger's and the count's --
        -- and they need separating, because each is invisible where the other
        -- is being read. alice casts FIRST so that a Reservoir that wrongly
        -- triggered on bob's cast would have something to count: with an empty
        -- log the wrong trigger gains 0 and hides.
        --
        --   after alice's cast   1  (the trigger's filter says nothing yet)
        --   after bob's cast     1  -- a trigger that ignored "you cast" makes it 2
        --   after alice's second 3  -- a count that ignored "you've cast" makes it 4
        --
        -- carol is the third seat: she is neither the caster nor the ability's
        -- controller, so "an opponent cast it" and "bob cast it" are different
        -- sentences here.
        Spec.it s "CR 601.2a a spell an OPPONENT cast neither triggers nor counts" $ do
          forest <- S.printingOf s registry "Forest"
          reservoir <- S.printingOf s registry "Aetherflux Reservoir"
          fog <- S.printingOf s registry "Fog"
          let base = board forest reservoir
              byAlice = castFog fog S.alice base
              thenByBob = castFog fog S.bob byAlice
              thenByAliceAgain = castFog fog S.alice thenByBob
              gained = lifeChange base
          Spec.assertEqWith s "alice's own cast gains 1" (gained byAlice S.alice) (Just 1)
          Spec.assertEqWith s "bob's cast fires nothing, so alice is still at 1" (gained thenByBob S.alice) (Just 1)
          Spec.assertEqWith s "and gains bob nothing" (gained thenByBob S.bob) (Just 0)
          Spec.assertEqWith s "and gains carol nothing" (gained thenByBob S.carol) (Just 0)
          Spec.assertEqWith s "alice's second counts only her own two, so 3 in total" (gained thenByAliceAgain S.alice) (Just 3)
        -- "This turn", moved on its own. Without this a lifetime tally passes
        -- every assertion above.
        Spec.it s "CR 608.2i the count is THIS turn's: it resets at the handoff" $ do
          forest <- S.printingOf s registry "Forest"
          reservoir <- S.printingOf s registry "Aetherflux Reservoir"
          fog <- S.printingOf s registry "Fog"
          let base = board forest reservoir
              three = castFog fog S.alice (castFog fog S.alice (castFog fog S.alice base))
              -- The turn passes to bob. alice keeps her Forests -- no untap step
              -- runs -- and casts an instant on his turn, so the only thing that
              -- changed is which turn it is.
              handed = S.runPure S.identityAnswer three Engine.handoffTurn
              onBobsTurn = handed {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
              fourth = castFog fog S.alice onBobsTurn
              gained = lifeChange base
          Spec.assertEqWith s "six over alice's own turn" (gained three S.alice) (Just 6)
          Spec.assertEqWith s "the handoff itself gains nothing" (gained handed S.alice) (Just 6)
          Spec.assertEqWith s "and the next turn's first cast gains 1, not 4" (gained fourth S.alice) (Just 7)

-- CR 608.2i read over CR 608.2h's record of a ZONE CHANGE: "for each nontoken
-- creature you controlled that died this turn". GAMEPLAY LEVEL for the
-- Aetherflux group's reason -- what it proves is that the count reads what the
-- move funnel filed as each permanent ceased, so a stubbed ViewOf would prove
-- nothing about the wiring.
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
        let (tid, gs) = S.addCreature tobias S.alice S.threePlayerGame
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
              (a1, gs1) = S.addCreature piker S.alice gs0
              (a2, gs2) = S.addCreature piker S.alice gs1
              (b1, gs3) = S.addCreature giant S.bob gs2
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
              (a1, gs1) = S.addCreature piker S.alice gs0
              (a2, gs2) = S.addCreature traveler S.alice gs1
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
              (b1, gs1) = S.addCreature giant S.bob gs0
              stolen = S.giveControl b1 S.alice gs1
              after = kill tid (kill b1 stolen)
          Spec.assertEqWith s "bob's Giant and Tobias" (zombies after) 2

-- CR 608.2i's look-back with a GREATEST over it: the first card whose fold reads
-- a per-member quantity off an event's CR 608.2h snapshot rather than off a live
-- object. GAMEPLAY LEVEL for the Aetherflux group's reason.
--
-- Rootha, Mastering the Moment, {2}{U}{R} Legendary Creature -- Orc Sorcerer 3/4:
-- "At the beginning of combat on your turn, if you've cast an instant or sorcery
-- spell this turn, create an X/X blue and red Elemental creature token with
-- flying and haste, where X is the greatest mana value among instant and sorcery
-- spells you've cast this turn."
--
-- alice casts Fog ({G}, 1) and Trumpet Blast ({2}{R}, 3), both instants or
-- sorceries and both targetless; Panglacial Wurm ({5}{G}{G}, 7), a CREATURE
-- spell; and bob casts Aetherspouts ({3}{U}{U}, 5). Six readings of the fold,
-- six different numbers: greatest 3, least 1, count 2, sum 4, card-type-blind 7,
-- controller-blind 5.
--
-- THREE seats, so "you cast it" and "bob cast it" are different sentences.
roothaSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
roothaSpec s registry =
  let board rootha forest mountain island =
        let addLands printing pid n g = List.foldl' (\g' _ -> snd (S.addCreature printing pid g')) g [1 .. (n :: Int)]
            withLands = addLands island S.bob 8 (addLands mountain S.alice 6 (addLands forest S.alice 10 S.threePlayerGame))
            (_, withRootha) = S.addCreature rootha S.alice withLands
         in withRootha
              { GameState.phase = Phase.PrecombatMain,
                GameState.activePlayer = S.alice,
                GameState.priority = Just S.alice
              }
      -- One spell into `caster`'s hand, cast, and the stack run down.
      castOne printing caster gs =
        let (oid, gs1) = S.addHandCard printing caster gs
            cast = S.runPure S.identityAnswer gs1 (S.cast caster oid)
         in S.runPure S.identityAnswer cast Engine.priorityLoop
      -- Rule 507's beginning of combat step, staged and then RUN: Engine.runStep
      -- is what writes the CR 603.2b StepBegan record the trigger matches, and
      -- the priority loop resolves what it put on the stack.
      intoBeginningOfCombat gs =
        S.runPure
          S.identityAnswer
          gs
            { GameState.phase = Phase.Combat CombatStep.BeginningOfCombat,
              GameState.priority = Just S.alice
            }
          Engine.runStep
      throughBeginningOfCombat gs = S.runPure S.identityAnswer (intoBeginningOfCombat gs) Engine.priorityLoop
      spellsCast rootha forest mountain island fog blast wurm spouts =
        let base = board rootha forest mountain island
            withFog = castOne fog S.alice base
            withBlast = castOne blast S.alice withFog
            withWurm = castOne wurm S.alice withBlast
         in castOne spouts S.bob withWurm
   in Spec.describe s "Rootha, Mastering the Moment" $ do
        Spec.it s "CR 202.3 X is the GREATEST mana value among the instants and sorceries ALICE cast" $ do
          rootha <- S.printingOf s registry "Rootha, Mastering the Moment"
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          island <- S.printingOf s registry "Island"
          fog <- S.printingOf s registry "Fog"
          blast <- S.printingOf s registry "Trumpet Blast"
          wurm <- S.printingOf s registry "Panglacial Wurm"
          spouts <- S.printingOf s registry "Aetherspouts"
          let cast = spellsCast rootha forest mountain island fog blast wurm spouts
              after = throughBeginningOfCombat cast
          -- The four casts are what everything below folds, so a board where one
          -- went unpaid would otherwise report a smaller maximum and look right.
          Spec.assertEqWith s "four spells were cast" (length (filter isSpellCast (S.eventsOf cast))) 4
          Spec.assertEqWith s "no token before combat" (S.tokensOf cast) []
          Spec.assertEqWith s "CR 603.4 the intervening if holds, so the ability triggers" (length (filter isAbilityTriggered (S.eventsOf after))) 1
          Spec.assertEqWith s "exactly one token" (length (S.tokensOf after)) 1
          mapM_ (\oid -> Spec.assertEqWith s "3/3, not 1, 2, 4, 5 or 7" (S.powerToughnessOf oid after) (Just (3, 3))) (S.tokensOf after)
          mapM_ (\oid -> Spec.assertEqWith s "blue and red" (Projection.colorsOf oid after) (Set.fromList [Color.Blue, Color.Red])) (S.tokensOf after)
        -- CR 111.3: the creating ability defines the token's characteristics, and
        -- it defines them as it resolves. GameState.events is cleared at the
        -- handoff, so a token that kept the fold in its power box would have no
        -- power at all on the next turn.
        Spec.it s "CR 111.3 the 3/3 is fixed at creation: it survives the turn handoff that clears the log" $ do
          rootha <- S.printingOf s registry "Rootha, Mastering the Moment"
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          island <- S.printingOf s registry "Island"
          fog <- S.printingOf s registry "Fog"
          blast <- S.printingOf s registry "Trumpet Blast"
          wurm <- S.printingOf s registry "Panglacial Wurm"
          spouts <- S.printingOf s registry "Aetherspouts"
          let after = throughBeginningOfCombat (spellsCast rootha forest mountain island fog blast wurm spouts)
              handed = S.runPure S.identityAnswer after Engine.handoffTurn
          Spec.assertEqWith s "the log is empty on the next turn" (S.eventsOf handed) []
          Spec.assertEqWith s "one token still" (length (S.tokensOf handed)) 1
          mapM_ (\oid -> Spec.assertEqWith s "still 3/3" (S.powerToughnessOf oid (S.settleSba handed)) (Just (3, 3))) (S.tokensOf handed)
        -- CR 603.4's intervening "if", on the same board minus the two spells
        -- that satisfy it. The Wurm alone is a cast this turn, so an ability
        -- reading "if you've cast a spell" would still trigger here.
        --
        -- The assertion is that the ability never TRIGGERS, not merely that no
        -- token appears: an ability that triggered anyway would fold an empty
        -- set, and the token that mints has no power at all and dies to a
        -- state-based action, so "no token" holds for a second reason and cannot
        -- tell the two apart.
        Spec.it s "CR 603.4 a CREATURE spell alone does not satisfy the intervening if" $ do
          rootha <- S.printingOf s registry "Rootha, Mastering the Moment"
          forest <- S.printingOf s registry "Forest"
          mountain <- S.printingOf s registry "Mountain"
          island <- S.printingOf s registry "Island"
          wurm <- S.printingOf s registry "Panglacial Wurm"
          let cast = castOne wurm S.alice (board rootha forest mountain island)
              after = throughBeginningOfCombat cast
          Spec.assertEqWith s "the Wurm was cast" (length (filter isSpellCast (S.eventsOf cast))) 1
          Spec.assertEqWith s "the ability never triggered" (filter isAbilityTriggered (S.eventsOf after)) []
          Spec.assertEqWith s "and no token was created" (S.tokensOf after) []

isAbilityTriggered :: GameEvent.GameEvent -> Bool
isAbilityTriggered event = case event of
  GameEvent.AbilityTriggered {} -> True
  _ -> False

isSpellCast :: GameEvent.GameEvent -> Bool
isSpellCast event = case event of
  GameEvent.SpellCast {} -> True
  _ -> False

-- CR 102.1 read as a NUMBER: the first count whose scope folds players rather
-- than objects. GAMEPLAY LEVEL, for the reason the Aetherflux Reservoir group
-- above is: what it proves is that a printed card reaches the fold and that the
-- fold reaches the seats the engine actually has.
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
tyranidInvasionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
tyranidInvasionSpec s registry =
  let -- alice has four Forests and nothing else; bob and carol have empty
      -- boards, so every token on the battlefield afterwards is the spell's.
      board forest =
        let withLands = List.foldl' (\g _ -> snd (S.addCreature forest S.alice g)) S.threePlayerGame [1 .. (4 :: Int)]
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
        Spec.it s "CR 102.1 at three seats the count is alice's two opponents" $ do
          invasion <- S.printingOf s registry "Tyranid Invasion"
          forest <- S.printingOf s registry "Forest"
          let after = castInvasion invasion (board forest)
              tokens = S.tokensOf after
          Spec.assertEqWith s "one token per opponent" (length tokens) 2
          Spec.assertEqWith s "the spell resolved" (GameState.stack after) []
          Spec.assertEqWith s "each is a Tyranid Warrior" (fmap (\oid -> Set.toList (Projection.subtypesOf oid after)) tokens) [[Subtype.Tyranid, Subtype.Warrior], [Subtype.Tyranid, Subtype.Warrior]]
          Spec.assertEqWith s "each is a 3/3" (fmap (`Projection.powerOf` after) tokens) [Just 3, Just 3]
        -- The discriminating case for CR 800.4a. The seating roster is still
        -- three (CR 800.5) and carol's Status.Departed is the only difference
        -- from the board above, so a count that named seats rather than players
        -- would mint two again.
        Spec.it s "CR 800.4a a player who has conceded is not one of alice's opponents" $ do
          invasion <- S.printingOf s registry "Tyranid Invasion"
          forest <- S.printingOf s registry "Forest"
          let base = Departure.depart Departure.Type.Conceded S.carol (board forest)
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
  Prompt.SearchLibrary _ _ matches cap -> List.genericTake cap matches
  _ -> S.identityAnswer p
