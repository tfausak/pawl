-- Covers Pawl.Engine.Count, Pawl.Types.Count, Pawl.Types.Scope, Pawl.Types.PlayerRef,
-- Pawl.Types.EventShape and Pawl.Types.Aggregation. Unit-level: the fold is driven
-- against a stubbed ViewOf so the evaluator is tested apart from the projection
-- that supplies it (Pawl.PowerToughnessSpec covers the wiring). The one
-- exception is the Aggregation.Greatest case that folds a PROJECTED power --
-- a stub has no power to read, so that case supplies a real projection and says
-- so.
module Pawl.CountSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Departure as Departure
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProjectedCharacteristics as PC
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
    Aggregation.Objects

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Count" $ do
  Spec.it s "Objects counts the matching members of a zone" $ do
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
    Spec.assertEq s (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs swampsYouControl) $ Just 2

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
    Spec.assertEqWith s "two types" (S.countOf viewOf (Filter.MkContext Nothing Nothing) gs count) $ Just 2

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
            Aggregation.Objects
    Spec.assertEqWith s "Alice's one" (S.countOf viewOf (Filter.MkContext (Just S.bob) Nothing) gs count) $ Just 1

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
            Aggregation.Objects
    Spec.assertEqWith s "bob's one plus carol's two, and none of alice's" (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs count) $ Just 3

  -- CR 102.1 / CR 800.4a (#279). Asserted against playersFor directly rather
  -- than through Count.evaluate, because no count can tell the difference: a
  -- departing player's objects leave the game with them (CR 800.4a), so
  -- Game.zoneMembers already answered [] for every zone of theirs and a
  -- departed seat folded to nothing whether or not it was named. The filter
  -- is here so this reading of a PlayerRef and Resolve.playerRefPlayers's --
  -- where it IS observable -- cannot disagree about who a PlayerRef names.
  Spec.it s "CR 800.4a neither EachPlayer nor Opponent names a player who has left the game" $ do
    let gs = Departure.depart Departure.Type.Conceded S.carol S.threePlayerGame
    Spec.assertEqWith
      s
      "EachPlayer names the two still in the game"
      (Count.playersFor (Filter.MkContext Nothing Nothing) gs PlayerRef.EachPlayer)
      (Just [S.alice, S.bob])
    Spec.assertEqWith
      s
      "and from alice, carol is not an opponent either"
      (Count.playersFor (Filter.MkContext (Just S.alice) Nothing) gs (PlayerRef.Relative PlayerRelation.Opponent))
      (Just [S.bob])

  Spec.it s "CR 109.5 Relative with no perspective is undeterminable" $ do
    let gs = Setup.emptyGame S.bothPlayers
        count =
          Count.Type.MkCount
            (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
            (Filter.Type.And [])
            Aggregation.Objects
    Spec.assertEq s (S.countOf (S.stubView []) (Filter.MkContext Nothing Nothing) gs count) Nothing

  Spec.it s "CR 700.4 InHistory counts deaths from the event snapshot" $ do
    -- A battlefield -> graveyard move whose SNAPSHOT is a creature counts,
    -- and a graveyard -> exile move does not, whatever its snapshot says.
    let gs0 = Setup.emptyGame S.bothPlayers
        creatureSnapshot = S.emptyCharacteristics {PC.cardTypes = Set.singleton CardType.Creature}
        died = GameEvent.Moved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Battlefield Zone.Graveyard) creatureSnapshot
        exiled = GameEvent.Moved (ZoneChange.MkZoneChange S.noSource S.noSource Zone.Graveyard Zone.Exile) creatureSnapshot
        gs =
          gs0
            { GameState.events = Seq.fromList [died, exiled],
              GameState.scannedThrough = 0,
              GameState.damageScannedThrough = 0
            }
        count =
          Count.Type.MkCount
            (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
            (Filter.Type.HasCardType CardType.Creature)
            Aggregation.Objects
    Spec.assertEqWith s "one death" (S.countOf (S.stubView []) (Filter.MkContext Nothing Nothing) gs count) $ Just 1

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
            Aggregation.Objects
    Spec.assertEqWith s "three" (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs count) $ Just 3

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
    Spec.assertEqWith s "one" (S.countOf viewOf (Filter.MkContext (Just S.bob) Nothing) gs swampsYouControl) $ Just 1

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
            Aggregation.Objects
    Spec.assertEqWith s "one card" (S.countOf viewOf (Filter.MkContext Nothing (Just srcId)) gs count) $ Just 1

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
    -- same count with Aggregation.Objects is Just 0 -- a count of nothing IS
    -- zero -- which is exactly the answer a maximum must NOT borrow.
    Spec.assertEqWith s "no maximum" (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs count) Nothing
    Spec.assertEqWith s "though counting the same empty set is 0" (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs swampsYouControl) (Just 0)

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
          S.countOf (\oid -> Just (Projection.viewOfObject oid g)) (Filter.MkContext (Just S.alice) Nothing) g count
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
    Spec.assertEqWith s "undeterminable" (S.countOf viewOf (Filter.MkContext (Just S.alice) Nothing) gs count) Nothing
