-- Covers: Pawl.Event (the placeObject as-enters mark) and Pawl.Engine (the drain),
-- the P2 copy gate (Clone). Gameplay-level: Clone enters via the zone-change funnel
-- and its projected characteristics are asserted.
module Pawl.CopySpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The battlefield objects whose printed card is named "Clone" (their source is
-- unchanged by copying -- only their projected characteristics change).
clonesOnBattlefield :: GameState.GameState -> [ObjectId]
clonesOnBattlefield gs = filter isClone (Set.toList (GameState.battlefield gs))
  where
    isClone oid = maybe False (\c -> Card.Type.name c == Text.pack "Clone") (Game.cardOf oid gs)

cloneOnBattlefield :: GameState.GameState -> Maybe ObjectId
cloneOnBattlefield = Maybe.listToMaybe . clonesOnBattlefield

-- The highest-id (most recently entered) object in a list. Total (no partial
-- `maximum`): sort descending by Down, take the head via listToMaybe.
newest :: [ObjectId] -> Maybe ObjectId
newest = Maybe.listToMaybe . List.sortOn Ord.Down

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Copy"
    [ HU.testCase "a copyOnEnter permanent is marked as-enters-pending when it enters" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, board) = S.addPiker cards S.alice gs0
            (_cloneStackId, staged) = S.spellOnStack (Cards.clonePrinting cards) S.alice board
            -- Resolve the top of the stack purely (a permanent -> changeZone to the
            -- battlefield), WITHOUT the settle drain, so the mark is observable.
            resolved = snd (Engine.runGamePure S.identityAnswer staged Stack.resolveTop)
         in case cloneOnBattlefield resolved of
              Nothing -> HU.assertFailure "Clone did not reach the battlefield"
              Just cloneId ->
                HU.assertBool
                  "Clone is marked as-enters-pending"
                  (maybe False (Binding.pendingCopy . Object.bindings) (Game.lookupObject cloneId resolved))
    ]
