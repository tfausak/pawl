-- Covers Pawl.Count, Pawl.Type.Count, Pawl.Type.Scope, Pawl.Type.PlayerRef,
-- Pawl.Type.EventShape and Pawl.Type.Aggregation. Unit-level: the fold is driven
-- against a stubbed ViewOf so the evaluator is tested apart from the projection
-- that supplies it (Pawl.PowerToughnessSpec covers the wiring).
module Pawl.CountSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.EventShape as EventShape
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.ProjectedCharacteristics as PC
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

swampsYouControl :: Count.Type.Count
swampsYouControl =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.Type.And [Filter.Type.HasSubtype Subtype.Swamp, Filter.Type.ControlledBy PlayerRelation.You])
    Aggregation.Objects

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Count"
    [ HU.testCase "Objects counts the matching members of a zone" $
        -- Two Swamps Alice controls, one Bob controls; ControlledBy You keeps
        -- Alice's two.
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (a2, gs2) = S.addCreature (Cards.swampPrinting cards) S.alice gs1
            (b1, gs) = S.addCreature (Cards.swampPrinting cards) S.bob gs2
            swamp = Set.singleton Subtype.Swamp
            land = Set.singleton CardType.Land
            viewOf =
              S.stubView
                [ (a1, land, swamp, Just S.alice),
                  (a2, land, swamp, Just S.alice),
                  (b1, land, swamp, Just S.bob)
                ]
         in HU.assertEqual
              "two"
              (Just 2)
              (Count.evaluate viewOf (Filter.MkContext (Just S.alice) Nothing) gs swampsYouControl),
      HU.testCase "CR 208.2a DistinctCardTypes counts the union, not the objects" $
        -- Three graveyard cards, two of them Creatures: the answer is 2 types,
        -- not 3 objects.
        let gs0 = Setup.emptyGame S.bothPlayers
            (g1, gs1) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice gs0
            (g2, gs2) = S.addGraveyardCard (Cards.pikerPrinting cards) S.alice gs1
            (g3, gs) = S.addGraveyardCard (Cards.lightningBoltPrinting cards) S.alice gs2
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
         in HU.assertEqual
              "two types"
              (Just 2)
              (Count.evaluate viewOf (Filter.MkContext Nothing Nothing) gs count),
      HU.testCase "CR 102.2 Relative Opponent excludes the perspective" $
        -- The same board as the first case, read from Bob's perspective: his
        -- opponent Alice controls two Swamps.
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (b1, gs) = S.addCreature (Cards.swampPrinting cards) S.bob gs1
            swamp = Set.singleton Subtype.Swamp
            land = Set.singleton CardType.Land
            viewOf = S.stubView [(a1, land, swamp, Just S.alice), (b1, land, swamp, Just S.bob)]
            count =
              Count.Type.MkCount
                (Scope.InZone Zone.Battlefield (PlayerRef.Relative PlayerRelation.Opponent))
                (Filter.Type.HasSubtype Subtype.Swamp)
                Aggregation.Objects
         in HU.assertEqual
              "Alice's one"
              (Just 1)
              (Count.evaluate viewOf (Filter.MkContext (Just S.bob) Nothing) gs count),
      HU.testCase "CR 109.5 Relative with no perspective is undeterminable" $
        let gs = Setup.emptyGame S.bothPlayers
            count =
              Count.Type.MkCount
                (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
                (Filter.Type.And [])
                Aggregation.Objects
         in HU.assertEqual
              "Nothing"
              Nothing
              (Count.evaluate (S.stubView []) (Filter.MkContext Nothing Nothing) gs count),
      HU.testCase "CR 700.4 InHistory counts deaths from the event snapshot" $
        -- A battlefield -> graveyard move whose SNAPSHOT is a creature counts,
        -- and a graveyard -> exile move does not, whatever its snapshot says.
        let gs0 = Setup.emptyGame S.bothPlayers
            creatureSnapshot = S.emptyCharacteristics {PC.cardTypes = Set.singleton CardType.Creature}
            died = GameEvent.Moved (ZoneChange.MkZoneChange S.noSource Zone.Battlefield Zone.Graveyard) creatureSnapshot
            exiled = GameEvent.Moved (ZoneChange.MkZoneChange S.noSource Zone.Graveyard Zone.Exile) creatureSnapshot
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
         in HU.assertEqual
              "one death"
              (Just 1)
              (Count.evaluate (S.stubView []) (Filter.MkContext Nothing Nothing) gs count),
      HU.testCase "EachPlayer folds every player's copy" $
        -- The first case's board with the ControlledBy conjunct dropped: all
        -- three Swamps across both players count.
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (a2, gs2) = S.addCreature (Cards.swampPrinting cards) S.alice gs1
            (b1, gs) = S.addCreature (Cards.swampPrinting cards) S.bob gs2
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
         in HU.assertEqual
              "three"
              (Just 3)
              (Count.evaluate viewOf (Filter.MkContext (Just S.alice) Nothing) gs count),
      HU.testCase "Relative You resolves against the perspective" $
        -- The first case's board, read from Bob's perspective: Bob's own one
        -- Swamp.
        let gs0 = Setup.emptyGame S.bothPlayers
            (a1, gs1) = S.addCreature (Cards.swampPrinting cards) S.alice gs0
            (a2, gs2) = S.addCreature (Cards.swampPrinting cards) S.alice gs1
            (b1, gs) = S.addCreature (Cards.swampPrinting cards) S.bob gs2
            swamp = Set.singleton Subtype.Swamp
            land = Set.singleton CardType.Land
            viewOf =
              S.stubView
                [ (a1, land, swamp, Just S.alice),
                  (a2, land, swamp, Just S.alice),
                  (b1, land, swamp, Just S.bob)
                ]
         in HU.assertEqual
              "one"
              (Just 1)
              (Count.evaluate viewOf (Filter.MkContext (Just S.bob) Nothing) gs swampsYouControl),
      HU.testCase "InSlot reads the bound player" $
        -- A source object binds Bob under the "target" slot (Sudden Impact's
        -- "that player's hand"); the count reads Bob's hand through the slot.
        let gs0 = Setup.emptyGame S.bothPlayers
            (srcId, gs1) = S.addCreature (Cards.pikerPrinting cards) S.alice gs0
            slot = SlotName.MkSlotName (Text.pack "target")
            bind obj = obj {Object.bindings = Map.insert slot (Binding.toPlayer S.bob) (Object.bindings obj)}
            gs2 = gs1 {GameState.objects = Map.adjust bind srcId (GameState.objects gs1)}
            (h1, gs) = S.addHandCard (Cards.lightningBoltPrinting cards) S.bob gs2
            viewOf = S.stubView [(h1, Set.singleton CardType.Instant, Set.empty, Just S.bob)]
            count =
              Count.Type.MkCount
                (Scope.InZone Zone.Hand (PlayerRef.InSlot slot))
                (Filter.Type.And [])
                Aggregation.Objects
         in HU.assertEqual
              "one card"
              (Just 1)
              (Count.evaluate viewOf (Filter.MkContext Nothing (Just srcId)) gs count)
    ]
