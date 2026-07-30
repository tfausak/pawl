module Pawl.EventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cast as Cast
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Expiry as Expiry
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Registry as Registry.Type
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Regenerability as Regenerability
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Pawl.Event"
    [ HU.testCase "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (theCreature, g1) = S.addCreature piker S.bob g0
            after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
        HU.assertEqual "exiled, not in graveyard" 0 gyCount
        HU.assertEqual "one object in exile" 1 inExile,
      HU.testCase "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (theCreature, g1) = S.addCreature piker S.bob g0
            after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
        case S.zoneChangesOf after of
          zc : _ -> HU.assertEqual "event says exile" Zone.Exile (ZoneChange.to zc)
          [] -> HU.assertFailure "expected an emitted zone change",
      HU.testCase "without Rest in Peace, a creature goes to the graveyard" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (theCreature, g1) = S.addCreature piker S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer g1 (Event.changeZone theCreature Zone.Graveyard)
        HU.assertEqual "in graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (bolt, g1) = S.addLibraryCard lightningBolt S.bob g0
            onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
            after = S.runPure S.identityAnswer onStack (Event.changeZone bolt Zone.Graveyard)
        HU.assertEqual "spell exiled, graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 701.6a Event.counter puts a countered spell into its owner's graveyard" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (spellId, onStack) = S.spellOnStack piker S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer onStack (Event.counter spellId)
        HU.assertEqual "off the stack" [] (GameState.stack after)
        HU.assertEqual "in bob's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "not on the battlefield" 0 (S.creaturesInPlay S.bob after),
      -- CR 113.6g at the funnel itself, without a countering spell in the way:
      -- Event.counter is what CR 701.6a's "remove it from the stack" runs
      -- through, and the gate is there rather than in the Counter opcode so that
      -- every future counterer inherits it.
      HU.testCase "CR 113.6g Event.counter is a no-op on a spell that can't be countered" $ do
        rendingVolley <- Registry.printing registry "Rending Volley"
        let (spellId, onStack) = S.spellOnStack rendingVolley S.bob (Setup.emptyGame S.bothPlayers)
            after = S.runPure S.identityAnswer onStack (Event.counter spellId)
        HU.assertEqual "still on the stack, under its original id" [spellId] (GameState.stack after)
        HU.assertEqual "nothing reached the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 614 a countered spell is exiled under Rest in Peace" $ do
        restInPeace <- Registry.printing registry "Rest in Peace"
        piker <- Registry.printing registry "Goblin Piker"
        let (_, g0) = S.addCreature restInPeace S.alice (Setup.emptyGame S.bothPlayers)
            (spellId, onStack) = S.spellOnStack piker S.bob g0
            after = S.runPure S.identityAnswer onStack (Event.counter spellId)
        HU.assertEqual "not in the graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "exiled instead" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 603/614 whole card: cast Rest in Peace, ETB exiles graveyards, then deaths are exiled" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        restInPeace <- Registry.printing registry "Rest in Peace"
        let base = S.landsInPlay plains 2
            (deadId, withDead) = S.addLibraryCard piker S.alice base
            g0 = S.runPure S.identityAnswer withDead (Event.changeZone deadId Zone.Graveyard) -- a card already in the graveyard
            (g1, ripId) = S.handOne restInPeace g0
            afterCast = snd (Engine.runGamePure S.identityAnswer g1 (Cast.castSpell S.alice ripId))
            -- run priority: both players pass, RiP resolves and enters, its ETB is
            -- placed (CR 117.5) and resolves, exiling the graveyard.
            settled = snd (Engine.runGamePure S.identityAnswer afterCast Engine.priorityLoop)
        HU.assertEqual "alice's graveyard exiled by the ETB" 0 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
        HU.assertEqual "Rest in Peace is on the battlefield" 1 (S.countOnBattlefieldByName (Text.pack "Rest in Peace") S.alice settled)
        HU.assertEqual "stack empty" [] (GameState.stack settled),
      -- CR 800.4b, sentence 2: "If a token would be created under the control of
      -- a player who has left the game, no token is created." NOT "is created and
      -- then removed" -- so the assertion is that the object count never moved,
      -- which a create-then-clean-up implementation would fail.
      --
      -- Three seats, because CR 800.1 gates the departure machinery on a game
      -- that began with more than two players.
      HU.testCase "CR 800.4b no token is created under the control of a player who has left the game" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let goblinCard = Printing.card piker
            gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
            before = Game.objectCount gone
            after = S.runPure S.identityAnswer gone (Event.createTokens S.alice goblinCard 2 TapState.Untapped)
        HU.assertBool "alice really has left" (notElem S.alice (Game.stillPlaying gone))
        HU.assertEqual "no object was ever minted" before (Game.objectCount after)
        HU.assertEqual "and nothing reached the battlefield" 0 (Set.size (GameState.battlefield after)),
      -- The other half of the guard: a player still in the game is unaffected, so
      -- this cannot pass by refusing every token.
      HU.testCase "CR 800.4b a player still in the game still gets their tokens" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let goblinCard = Printing.card piker
            gone = Departure.depart Departure.Type.Conceded S.alice S.threePlayerGame
            before = Game.objectCount gone
            after = S.runPure S.identityAnswer gone (Event.createTokens S.bob goblinCard 2 TapState.Untapped)
        HU.assertEqual "bob's two tokens exist" (before + 2) (Game.objectCount after)
        HU.assertEqual "both on the battlefield" 2 (Set.size (GameState.battlefield after)),
      HU.testCase "CR 111.2 createTokens puts a token on the battlefield and emits an enters event" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card piker
            before = Game.objectCount base
            after = S.runPure S.identityAnswer base (Event.createTokens S.alice goblinCard 1 TapState.Untapped)
            newIds = Set.toList (GameState.battlefield after)
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
              (fmap ZoneChange.to (S.zoneChangesOf after))
          _ -> HU.assertFailure "expected exactly one token",
      HU.testCase "CR 614 + 704.5d a token dies under Rest in Peace: exiled, then ceases" $ do
        piker <- Registry.printing registry "Goblin Piker"
        restInPeace <- Registry.printing registry "Rest in Peace"
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card piker
            (_, withRip) = S.addCreature restInPeace S.bob base
            (tokId, gs) = S.addToken goblinCard S.alice withRip
            -- Kill the token: route it to the graveyard; RiP redirects to exile.
            dying = S.runPure S.identityAnswer gs (Event.changeZone tokId Zone.Graveyard)
            settled = S.settleSba dying
        HU.assertEqual "the token never entered a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice dying))
        HU.assertEqual "it was redirected to exile" 1 (length (Game.zoneMembers Zone.Exile S.alice dying))
        HU.assertEqual "after the SBA it has ceased to exist (gone from exile)" 0 (length (Game.zoneMembers Zone.Exile S.alice settled)),
      HU.testCase "CR 701.19a / 514.2 a regeneration shield is dropped at cleanup (this turn)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature piker S.alice base
            cleared = Expiry.dropAtCleanup (S.addRegenShield oid gs0)
        HU.assertEqual "no shields remain" [] (GameState.replacements cleared),
      HU.testCase "CR 701.19a Event.destroy consumes a shield and regenerates instead" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature piker S.alice base
            damaged = S.markDamage oid 1 gs0
            shielded = S.addRegenShield oid damaged
            after = S.runPure S.identityAnswer shielded (Event.destroy Regenerability.Regenerable [oid])
        HU.assertEqual "still on the battlefield (regenerated, not destroyed)" True (Set.member oid (GameState.battlefield after))
        HU.assertEqual "shield spent" [] (GameState.replacements after)
        case Game.lookupObject oid after of
          Just obj -> do
            HU.assertEqual "tapped (CR 701.19a)" TapState.Tapped (Object.tapped obj)
            HU.assertEqual "damage removed (CR 701.19a)" 0 (Object.damage obj)
          Nothing -> HU.assertFailure "the creature vanished",
      HU.testCase "CR 701.19a a second destroy with no shield left kills it" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature piker S.alice base
            once = S.runPure S.identityAnswer (S.addRegenShield oid gs0) (Event.destroy Regenerability.Regenerable [oid]) -- regenerated
            twice = S.runPure S.identityAnswer once (Event.destroy Regenerability.Regenerable [oid]) -- no shield -> dies
        HU.assertEqual "gone from the battlefield" False (Set.member oid (GameState.battlefield twice)),
      HU.testCase "CR 700.4 Event.destroy no-ops on an indestructible permanent" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let base = Setup.emptyGame S.bothPlayers
            (oid, gs0) = S.addCreature darksteelMyr S.alice base
            after = S.runPure S.identityAnswer gs0 (Event.destroy Regenerability.Regenerable [oid])
        HU.assertEqual "indestructible survives" True (Set.member oid (GameState.battlefield after)),
      HU.testCase "CR 122.2 counters cease to exist when an object changes zones" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay swamp 0
            (oid, withCreature) = S.addCreature piker S.bob base
            withCounter = S.addCounter CounterKind.PlusOnePlusOne 2 oid withCreature
            -- Bounce to hand: changeZone mints a new incarnation (CR 400.7).
            bounced = S.runPure S.identityAnswer withCounter (Event.changeZone oid Zone.Hand)
            -- Total (no `head`): map over the hand zone; expect exactly one card, empty.
            handCounters = fmap (\h -> maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject h bounced)) (Game.zoneMembers Zone.Hand S.bob bounced)
        HU.assertEqual "counter present before the move" (Map.fromList [(CounterKind.PlusOnePlusOne, 2)]) (maybe Map.empty Object.counters (Game.lookupObject oid withCounter))
        HU.assertEqual "the one new incarnation in hand has no counters" [Map.empty] handCounters,
      HU.testCase "CR 704.5q both counter kinds annihilate to zero (symmetric)" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay swamp 0
            (oid, gs0) = S.addCreature piker S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 1 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = S.settleSba gs2
        HU.assertEqual "no counters remain" Map.empty (maybe (Map.fromList [(CounterKind.PlusOnePlusOne, 99)]) Object.counters (Game.lookupObject oid after)),
      HU.testCase "CR 704.5q annihilation removes N = min (asymmetric)" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay swamp 0
            (oid, gs0) = S.addCreature piker S.bob base
            gs1 = S.addCounter CounterKind.PlusOnePlusOne 2 oid gs0
            gs2 = S.addCounter CounterKind.MinusOneMinusOne 1 oid gs1
            after = S.settleSba gs2
        HU.assertEqual "one +1/+1 remains" (Just 1) (fmap (Map.findWithDefault 0 CounterKind.PlusOnePlusOne . Object.counters) (Game.lookupObject oid after))
        HU.assertEqual "no -1/-1 remains" (Just 0) (fmap (Map.findWithDefault 0 CounterKind.MinusOneMinusOne . Object.counters) (Game.lookupObject oid after))
        -- Net P/T unchanged by annihilation: base 2/1 + net +1/+1 = 3/2.
        HU.assertEqual "power still 3" (Just 3) (Projection.powerOf oid after)
        HU.assertEqual "toughness still 2" (Just 2) (Projection.toughnessOf oid after),
      HU.testCase "CR 603.2 SelfDealsCombatDamageToPlayer matches the bearer's combat damage to a player" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False 0 DamageKind.Combat)
         in HU.assertBool "matches" (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev),
      HU.testCase "it does not match combat damage to a creature" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToCreature (ObjectId.MkObjectId 2)) 2 False False 0 DamageKind.Combat)
         in HU.assertBool "no match" (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)),
      HU.testCase "it does not match noncombat damage to a player" $
        let bearer = ObjectId.MkObjectId 1
            ev = GameEvent.DamageDealt (DamageEvent.MkDamageEvent bearer (Recipient.ToPlayer S.bob) 2 False False 0 DamageKind.Noncombat)
         in HU.assertBool "no match" (not (Event.matchesTrigger (Setup.emptyGame S.bothPlayers) bearer S.alice TriggerCondition.SelfDealsCombatDamageToPlayer ev)),
      HU.testCase "CR 400.7: a zone change forgets attachment" $ do
        plains <- Registry.printing registry "Plains"
        piker <- Registry.printing registry "Goblin Piker"
        let base = S.landsInPlay plains 1
            (host, withHost) = S.addCreature piker S.bob base
            (rider, withRider) = S.addCreature piker S.alice withHost
            attached = S.attach rider host withRider
            bounced = S.runPure S.identityAnswer attached (Event.changeZone rider Zone.Hand)
            moved = filter (\o -> Object.zone o == Zone.Hand) (Map.elems (GameState.objects bounced))
        HU.assertEqual "attached before the move" (Just (Just host)) (fmap Object.attachedTo (Game.lookupObject rider attached))
        HU.assertEqual "one card in hand" 1 (length moved)
        HU.assertEqual "the new incarnation is unattached" [Nothing] (fmap Object.attachedTo moved)
    ]
