module Pawl.EventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TriggerCondition as TriggerCondition
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Event"
    [ HU.testCase "CR 614.1a a graveyard-bound move is redirected to exile" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
         in HU.assertEqual "redirected to exile" Zone.Exile (ZoneChange.to (Event.applyReplacements [rip] proposed)),
      HU.testCase "CR 614.5 the redirect does not re-apply (exile is not graveyard)" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Battlefield, ZoneChange.to = Zone.Graveyard}
         in HU.assertEqual "exile, applied once" Zone.Exile (ZoneChange.to (Event.applyReplacements [rip, rip] proposed)),
      HU.testCase "a non-graveyard move is untouched" $
        let rip = ReplacementEffect.RedirectZoneChange {ReplacementEffect.whenDestination = Zone.Graveyard, ReplacementEffect.toDestination = Zone.Exile}
            proposed = ZoneChange.MkZoneChange {ZoneChange.object = ObjectId.MkObjectId 5, ZoneChange.from = Zone.Stack, ZoneChange.to = Zone.Battlefield}
         in HU.assertEqual "battlefield unchanged" Zone.Battlefield (ZoneChange.to (Event.applyReplacements [rip] proposed)),
      HU.testCase "CR 614: with Rest in Peace out, a creature sent to the graveyard is exiled" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
         in do
              HU.assertEqual "exiled, not in graveyard" 0 gyCount
              HU.assertEqual "one object in exile" 1 inExile,
      HU.testCase "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
         in case GameState.zoneChanges after of
              zc : _ -> HU.assertEqual "event says exile" Zone.Exile (ZoneChange.to zc)
              [] -> HU.assertFailure "expected an emitted zone change",
      HU.testCase "without Rest in Peace, a creature goes to the graveyard" $
        let (piker, g1) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.changeZone piker Zone.Graveyard g1
         in HU.assertEqual "in graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (bolt, g1) = S.addLibraryCard (Cards.lightningBoltPrinting cards) S.bob g0
            onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
            after = Event.changeZone bolt Zone.Graveyard onStack
         in HU.assertEqual "spell exiled, graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 603.6a: Rest in Peace entering yields its ETB trigger" $
        let (ripId, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            -- an event describing RiP having entered the battlefield
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
         in case Event.triggersFrom [entered] gs of
              [(srcId, controller, _)] -> do
                HU.assertEqual "source is RiP" ripId srcId
                HU.assertEqual "controller is alice" S.alice controller
              _ -> HU.assertFailure "expected exactly one pending trigger",
      HU.testCase "a graveyard-bound event yields no enters trigger" $
        let (ripId, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            toGrave = ZoneChange.MkZoneChange ripId Zone.Battlefield Zone.Graveyard
         in HU.assertEqual "no triggers" 0 (length (Event.triggersFrom [toGrave] gs)),
      HU.testCase "SelfEnters matches only a battlefield destination" $ do
        HU.assertBool "enters battlefield matches" (Event.matchesTrigger TriggerCondition.SelfEnters (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) Zone.Stack Zone.Battlefield))
        HU.assertBool "enters graveyard does not" (not (Event.matchesTrigger TriggerCondition.SelfEnters (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) Zone.Battlefield Zone.Graveyard))),
      HU.testCase "CR 603/614 whole card: cast Rest in Peace, ETB exiles graveyards, then deaths are exiled" $
        let base = S.landsInPlay (Cards.plainsPrinting cards) 2
            (deadId, withDead) = S.addLibraryCard (Cards.pikerPrinting cards) S.alice base
            g0 = Event.changeZone deadId Zone.Graveyard withDead -- a card already in the graveyard
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
            after = Event.createToken S.alice goblinCard base
            newIds = Set.toList (GameState.battlefield after)
         in do
              HU.assertEqual "one more object exists" (before + 1) (Game.objectCount after)
              HU.assertEqual "exactly one battlefield object" 1 (length newIds)
              case newIds of
                [tokId] -> do
                  HU.assertEqual "cardOf sees the token" (Just goblinCard) (Game.cardOf tokId after)
                  HU.assertEqual "owned by its creator (CR 111.2)" (Just S.alice) (Game.controllerOf tokId after)
                  case Game.lookupObject tokId after of
                    Just obj -> HU.assertEqual "summoning sick (CR 302.6)" Sickness.Sick (Object.sickness obj)
                    Nothing -> HU.assertFailure "token vanished"
                  HU.assertEqual
                    "one enters-battlefield event emitted"
                    [Zone.Battlefield]
                    (map ZoneChange.to (GameState.zoneChanges after))
                _ -> HU.assertFailure "expected exactly one token",
      HU.testCase "CR 614 + 704.5d a token dies under Rest in Peace: exiled, then ceases" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            (_, withRip) = S.addCreature (Cards.restInPeacePrinting cards) S.bob base
            (tokId, gs) = S.addToken goblinCard S.alice withRip
            -- Kill the token: route it to the graveyard; RiP redirects to exile.
            dying = Event.changeZone tokId Zone.Graveyard gs
            settled = Sba.checkStateBasedActions dying
         in do
              HU.assertEqual "the token never entered a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.alice dying))
              HU.assertEqual "it was redirected to exile" 1 (length (Game.zoneMembers Zone.Exile S.alice dying))
              HU.assertEqual "after the SBA it has ceased to exist (gone from exile)" 0 (length (Game.zoneMembers Zone.Exile S.alice settled))
    ]
