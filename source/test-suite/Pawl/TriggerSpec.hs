-- Covers M4.5 P4 Task 1: the turn-scoped event log (Pawl.Type.GameEvent,
-- GameState's log and watermarks) -- append-only recording, watermark-based
-- consumption per reader (trigger scan, SBA damage check), and the log's
-- turn-scoped clearing at handoff. Later P4 tasks add coverage HERE for the
-- widened CR 603.6a trigger scan, state triggers (CR 603.8), delayed triggers
-- (CR 603.7), intervening "if" (CR 603.4 / 608.2a), and the CR 603.3b ordering
-- prompt -- none of that is implemented yet.
module Pawl.TriggerSpec where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameEvent as GameEvent
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone
import qualified Pawl.Type.ZoneChange as ZoneChange
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 608.2i: the log records; it is never emptied by a reader.
logTests :: Cards.Cards -> Tasty.TestTree
logTests cards =
  Tasty.testGroup
    "EventLog"
    [ -- CR 400.7 / 603.2g: a zone change appends a Moved event carrying the
      -- RESOLVED destination. Pawl.EventSpec's "CR 603.2g: the emitted event
      -- records the RESOLVED destination (exile)" covers this same accessor
      -- (S.zoneChangesOf / ZoneChange.to) more strongly, through a Rest in
      -- Peace redirect -- no separate case needed here.
      -- CR 608.2h: the snapshot is the object as it last existed in the zone it
      -- LEFT. Re-deriving from the printed card would be wrong for an animated
      -- land and impossible for a token (CR 111.3).
      HU.testCase "CR 608.2h a Moved event snapshots the object it moved" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            expected = Projection.project piker gs
            after = Event.changeZone piker Zone.Graveyard gs
         in case Foldable.toList (GameState.events after) of
              GameEvent.Moved _ snapshot : _ -> HU.assertEqual "snapshot from the origin zone" expected snapshot
              _ -> HU.assertFailure "expected exactly one Moved event",
      -- CR 704.5h's window is "since the last SBA check": the check CONSUMES by
      -- bumping a watermark, and the record survives.
      HU.testCase "CR 704 the SBA check advances the damage watermark but keeps the record" $
        let (gs, _, _) = S.combatBoardOf [Cards.typhoidRatsPrinting cards] [Cards.ogreSentryPrinting cards]
            fought = S.fightWith S.aggressiveAnswer gs
            after = Sba.checkStateBasedActions fought
         in do
              HU.assertEqual "nothing left unscanned for damage" [] (Event.unscannedDamage after)
              HU.assertBool "the damage events are still recorded" (not (null (S.damageEventsOf after))),
      HU.testCase "CR 117.5 the trigger scan advances its watermark but keeps the record" $
        let (_, gs) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, gs1) = S.addPiker cards S.bob gs
            moved = Event.changeZone piker Zone.Hand gs1
            scanned = snd (Engine.runGamePure S.identityAnswer moved Engine.placePendingTriggers)
         in do
              HU.assertEqual "nothing left unscanned" [] (Event.unscannedEvents scanned)
              HU.assertBool "the zone change is still recorded" (not (null (S.zoneChangesOf scanned))),
      -- The turn is the log's scope (CR 608.2i). Clearing at cleanup would be
      -- wrong: cleanup is still part of THIS turn.
      HU.testCase "the log and both watermarks are cleared at turn handoff" $
        let (piker, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
            moved = Event.changeZone piker Zone.Graveyard gs
            after = snd (Engine.runGamePure S.identityAnswer moved Engine.handoffTurn)
         in do
              HU.assertEqual "log empty" Seq.empty (GameState.events after)
              HU.assertEqual "scan watermark reset" 0 (GameState.scannedThrough after)
              HU.assertEqual "damage watermark reset" 0 (GameState.damageScannedThrough after),
      -- CR 514.3 (partial): an event emitted by the cleanup step must be scanned
      -- BEFORE handoffTurn clears the log, or its trigger is lost outright.
      HU.testCase "advance settles before handing off, so no unscanned event is discarded" $
        let (ripId, gs0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            entered = ZoneChange.MkZoneChange ripId Zone.Stack Zone.Battlefield
            gs1 = S.withEvent (GameEvent.Moved entered (Projection.project ripId gs0)) gs0
            ending = gs1 {GameState.remaining = Seq.empty}
            after = snd (Engine.runGamePure S.identityAnswer ending Engine.advance)
            isTrigger oid = case Game.lookupObject oid after of
              Just obj -> case Object.source obj of
                Source.OfTrigger _ _ -> True
                _ -> False
              Nothing -> False
         in do
              HU.assertBool "the pending trigger reached the stack" (any isTrigger (GameState.stack after))
              HU.assertEqual "the log was cleared afterwards" Seq.empty (GameState.events after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Pawl.TriggerSpec" [logTests cards]
