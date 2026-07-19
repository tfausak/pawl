-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
-- The three existence/exit-criterion properties are converted to deterministic
-- tests in their subsystem specs by the next task.
module Pawl.PropertySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

-- Did this seed's green-black game put a card of this name into a graveyard? A
-- cast instant always ends there (resolved or fizzled); nothing else moves a
-- Giant Growth or Serpent's Gift out of a library.
castsNamed :: Text.Text -> Int -> Bool
castsNamed name s =
  let gs = S.runRandomGame S.greenBlack s
      named oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.Type.name (Printing.card printing) == name
          Source.OfAbility _ _ -> False
          Source.OfTrigger _ _ -> False
      inGrave pid = any named (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [S.alice, S.bob]

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
              ),
      QC.testProperty "continuous effects happen: some green-black seed casts Giant Growth" $
        QC.once (QC.property (any (castsNamed (Text.pack "Giant Growth")) [1 .. 100 :: Int])),
      QC.testProperty "grants happen: some green-black seed casts Serpent's Gift" $
        QC.once (QC.property (any (castsNamed (Text.pack "Serpent's Gift")) [1 .. 100 :: Int]))
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Properties" [propertyTests]
