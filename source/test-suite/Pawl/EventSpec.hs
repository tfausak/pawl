module Pawl.EventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Event"
    [ HU.testCase "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Graveyard)
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
         in do
              HU.assertEqual "exiled, not in graveyard" 0 gyCount
              HU.assertEqual "one object in exile" 1 inExile,
      HU.testCase "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Graveyard)
         in case S.zoneChangesOf after of
              zc : _ -> HU.assertEqual "event says exile" Zone.Exile (ZoneChange.to zc)
              [] -> HU.assertFailure "expected an emitted zone change",
      HU.testCase "without Rest in Peace, a creature goes to the graveyard" $
        let (piker, g1) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Graveyard)
         in HU.assertEqual "in graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (bolt, g1) = S.addLibraryCard (Cards.lightningBoltPrinting cards) S.bob g0
            onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
            after = S.runPure S.identityAnswer onStack (Event.changeZone bolt Zone.Graveyard)
         in HU.assertEqual "spell exiled, graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 701.6a Event.counter puts a countered spell into its owner's graveyard" $
        let (spellId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer onStack (Event.counter spellId)
         in do
              HU.assertEqual "off the stack" [] (GameState.stack after)
              HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "not on the battlefield" 0 (S.creaturesInPlay S.bob after),
      HU.testCase "CR 614 a countered spell is exiled under Rest in Peace" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (spellId, onStack) = S.spellOnStack (Cards.pikerPrinting cards) S.bob g0
            after = S.runPure S.identityAnswer onStack (Event.counter spellId)
         in do
              HU.assertEqual "not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "exiled instead" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 603/614 whole card: cast Rest in Peace, ETB exiles graveyards, then deaths are exiled" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 2
            (deadId, withDead) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            g0 = S.runPure S.identityAnswer withDead (Event.changeZone deadId Zone.Graveyard) -- a card already in the graveyard
            (g1, ripId) = S.handOne (Cards.restInPeacePrinting cards) g0
            afterCast = snd (Engine.runGamePure S.identityAnswer g1 (Cast.castSpell S.alice ripId))
            -- run priority: both players pass, RiP resolves and enters, its ETB is
            -- placed (CR 117.5) and resolves, exiling the graveyard.
            settled = snd (Engine.runGamePure S.identityAnswer afterCast Engine.priorityLoop)
         in do
              HU.assertEqual "alice's graveyard exiled by the ETB" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
              HU.assertEqual "Rest in Peace is on the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Rest in Peace") S.alice settled)
              HU.assertEqual "stack empty" [] (GameState.stack settled),
      HU.testCase "CR 111.2 createToken puts a token on the battlefield and emits an enters event" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            before = Game.objectCount base
            after = S.runPure S.identityAnswer base (Event.createToken S.alice goblinCard)
            newIds = Set.toList (GameState.battlefield after)
         in do
              HU.assertEqual "one more object exists" (before + 1) (Game.objectCount after)
              HU.assertEqual "exactly one battlefield object" 1 (length newIds)
              case newIds of
                [tokId] -> do
                  HU.assertEqual "cardOf sees the token" (Just goblinCard) (Game.cardOf tokId after)
                  HU.assertEqual "owned by its creator (CR 111.2)" (Just S.alice) (Projection.controllerOf tokId after)
                  case Game.lookupObject tokId after of
                    Just obj -> HU.assertEqual "summoning sick (CR 302.6)" Sickness.Sick (Object.sickness obj)
                    Nothing -> HU.assertFailure "token vanished"
                  HU.assertEqual
                    "one enters-battlefield event emitted"
                    [Zone.Battlefield]
                    (map ZoneChange.to (S.zoneChangesOf after))
                _ -> HU.assertFailure "expected exactly one token",
      HU.testCase "CR 614 + 704.5d a token dies under Rest in Peace: exiled, then ceases" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            (_, withRip) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (tokId, gs) = S.addToken goblinCard S.alice withRip
            -- Kill the token: route it to the graveyard; RiP redirects to exile.
            dying = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
            settled = S.settleSba dying
         in do
              HU.assertEqual "the token never entered a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice dying))
              HU.assertEqual "it was redirected to exile" 1 (length (Game.zoneMembers Zone.Exile S.alice dying))
              HU.assertEqual "after the SBA it has ceased to exist (gone from exile)" 0 (length (Game.zoneMembers Zone.Exile S.alice settled)),
      HU.testCase "CR 701.19a a regeneration shield is stored per object" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            shielded = S.addRegenShield oid gs0
         in HU.assertEqual "one shield on the object" (Just 1) (Map.lookup oid (GameState.regenerationShields shielded)),
      HU.testCase "CR 701.19a regeneration shields are cleared at cleanup (this turn)" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            cleared = Event.clearRegenerationShields (S.addRegenShield oid gs0)
         in HU.assertEqual "no shields remain" True (Map.null (GameState.regenerationShields cleared)),
      HU.testCase "CR 701.19a Event.destroy consumes a shield and regenerates instead" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            damaged = S.markDamage oid 1 gs0
            shielded = S.addRegenShield oid damaged
            after = S.runPure S.identityAnswer shielded (Event.destroy oid)
         in do
              HU.assertEqual "still on the battlefield (regenerated, not destroyed)" True (Set.member oid (GameState.battlefield after))
              HU.assertEqual "shield consumed" Nothing (Map.lookup oid (GameState.regenerationShields after))
              case Game.lookupObject oid after of
                Just obj -> do
                  HU.assertEqual "tapped (CR 701.19a)" TapState.Tapped (Object.tapped obj)
                  HU.assertEqual "damage removed (CR 701.19a)" 0 (Object.damage obj)
                Nothing -> HU.assertFailure "the creature vanished",
      HU.testCase "CR 701.19a a second destroy with no shield left kills it" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.alice base
            once = S.runPure S.identityAnswer (S.addRegenShield oid gs0) (Event.destroy oid) -- regenerated
            twice = S.runPure S.identityAnswer once (Event.destroy oid) -- no shield -> dies
         in HU.assertEqual "gone from the battlefield" False (Set.member oid (GameState.battlefield twice)),
      HU.testCase "CR 700.4 Event.destroy no-ops on an indestructible permanent" $
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            after = S.runPure S.identityAnswer gs0 (Event.destroy oid)
         in HU.assertEqual "indestructible survives" True (Set.member oid (GameState.battlefield after)),
      HU.testCase "CR 122.2 counters cease to exist when an object changes zones" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, withCreature) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 2 oid withCreature
            -- Bounce to hand: changeZone mints a new incarnation (CR 400.7).
            bounced = S.runPure S.identityAnswer withCounter (Event.changeZone oid Zone.Hand)
            -- Total (no `head`): map over the hand zone; expect exactly one card, empty.
            handCounters = map (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.bob bounced)
         in do
              HU.assertEqual "counter present before the move" (Map.fromList [(CounterKind.PlusOnePlusOne, 2)]) (maybe Map.empty Object.counters (Game.lookupObject oid withCounter))
              HU.assertEqual "the one new incarnation in hand has no counters" [Map.empty] handCounters,
      HU.testCase "CR 704.5q both counter kinds annihilate to zero (symmetric)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = S.settleSba gs2
         in HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject oid after)),
      HU.testCase "CR 704.5q annihilation removes N = min (asymmetric)" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 0
            (oid, gs0) = S.addCreature (Cards.pikerPrinting cards) S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 2 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = S.settleSba gs2
         in do
              HU.assertEqual "one +1/+1 remains" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid after))
              HU.assertEqual "no -1/-1 remains" (Just 0) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid after))
              -- Net P/T unchanged by annihilation: base 2/1 + net +1/+1 = 3/2.
              HU.assertEqual "power still 3" (Just 3) (Projection.powerOf oid after)
              HU.assertEqual "toughness still 2" (Just 2) (Projection.toughnessOf oid after)
    ]
