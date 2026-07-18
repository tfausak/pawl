-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
-- The three existence/exit-criterion properties are converted to deterministic
-- tests in their subsystem specs by the next task.
module Pawl.PropertySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        QC.conjoin (map (\m -> Game.objectCount (S.runRandomGame m s) QC.=== 120) S.matchups),
      -- The property that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.conjoin (map (\m -> QC.property (Maybe.isJust (GameState.result (S.runRandomGame m s)))) S.matchups),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.conjoin (map (\m -> QC.property (nextIdOf (S.runRandomGame m s) >= 120)) S.matchups),
      QC.testProperty "no mana floats at the end" $ \s ->
        QC.conjoin (map (\m -> GameState.manaPool (S.runRandomGame m s) QC.=== Map.empty) S.matchups),
      -- Replaces M0's "no life changes". Nothing here GAINS life, so any
      -- increase is a bug. Dies at lifelink (still unscheduled -- see the
      -- design doc's punchlist), the same way this property's ancestor
      -- announced M1b.
      QC.testProperty "life never increases" $ \s ->
        QC.conjoin
          ( map
              (\m -> QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players (S.runRandomGame m s)))))
              S.matchups
          ),
      -- Durable structural property: with a deck that can only ever deck out (60
      -- basic lands, no spells, no attackers), every seed's game ends AND ends by
      -- a player drawing from an empty library (CR 704.5b) -- never by any other
      -- loss condition. Stays true no matter what cards later exist.
      QC.testProperty "a lands-only mirror always ends by deck-out" $ \s ->
        let final = S.runRandomGame S.landsOnly s
         in QC.property
              ( Maybe.isJust (GameState.result final)
                  && not (Set.null (GameState.drewFromEmpty final))
              )
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Properties" [propertyTests]
