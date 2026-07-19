module Pawl.EventSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Card as Card
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.ReplacementEffect as ReplacementEffect
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
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
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
            inExile = Set.size (GameState.exile after)
            gyCount = length (Game.zoneMembers Zone.Graveyard S.bob after)
         in do
              HU.assertEqual "exiled, not in graveyard" 0 gyCount
              HU.assertEqual "one object in exile" 1 inExile,
      HU.testCase "CR 603.2g: the emitted event records the RESOLVED destination (exile)" $
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker S.bob g0
            after = Event.changeZone piker Zone.Graveyard g1
         in case GameState.zoneChanges after of
              zc : _ -> HU.assertEqual "event says exile" Zone.Exile (ZoneChange.to zc)
              [] -> HU.assertFailure "expected an emitted zone change",
      HU.testCase "without Rest in Peace, a creature goes to the graveyard" $
        let (piker, g1) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
            after = Event.changeZone piker Zone.Graveyard g1
         in HU.assertEqual "in graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.bob after)),
      HU.testCase "CR 608.2n: a resolving spell is exiled from the stack under Rest in Peace" $
        let (_, g0) = S.addCreature Card.restInPeacePrinting S.alice (Setup.emptyGame S.bothPlayers)
            (bolt, g1) = S.addLibraryCard Card.lightningBoltPrinting S.bob g0
            onStack = g1 {GameState.stack = bolt : GameState.stack g1, GameState.objects = Map.adjust (\o -> o {Object.zone = Zone.Stack}) bolt (GameState.objects g1)}
            after = Event.changeZone bolt Zone.Graveyard onStack
         in HU.assertEqual "spell exiled, graveyard empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
    ]
