module Pawl.EventSpec where

import qualified Pawl.Event as Event
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
         in HU.assertEqual "battlefield unchanged" Zone.Battlefield (ZoneChange.to (Event.applyReplacements [rip] proposed))
    ]
