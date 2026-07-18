-- Covers cross-cutting universal QuickCheck invariants (true for every seed).
-- The three existence/exit-criterion properties are converted to deterministic
-- tests in their subsystem specs by the next task.
module Pawl.PropertySpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
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
      -- The M1b exit criterion, asserted rather than assumed: across 100 seeds,
      -- at least one red-red game must see damage actually change someone's
      -- life total. Without this, every combat path could silently no-op and
      -- the suite would still be green.
      QC.testProperty "combat happens: some seed changes a life total" $
        QC.once $
          QC.property $
            any someLifeChanged [1 .. 100 :: Int],
      QC.testProperty "green-black: some seed sends a creature to the graveyard" $
        QC.once $
          QC.property $
            any creatureDied [1 .. 100 :: Int],
      -- The M3a exit criterion, asserted the same way combat's was: across 100
      -- seeds some red-red game must actually cast a Bolt, or instant speed
      -- could silently never fire while the suite stays green.
      QC.testProperty "instants happen: some seed casts a Bolt" $
        QC.once $
          QC.property $
            any boltCast_ [1 .. 100 :: Int]
    ]

-- Did this seed's red-red game put a Bolt into a graveyard? A cast Bolt always
-- ends there (resolved or fizzled), and nothing else moves one out of a library.
boltCast_ :: Int -> Bool
boltCast_ s =
  let gs = S.runRandomGame S.redRed s
      isBolt oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Printing.card printing == Printing.card Card.lightningBoltPrinting
      inGrave pid = any isBolt (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [S.alice, S.bob]

-- Did anyone's life total move over the course of the game this seed produces?
someLifeChanged :: Int -> Bool
someLifeChanged s =
  let moved pl = Player.life pl /= Setup.startingLife
   in any moved (Map.elems (GameState.players (S.runRandomGame S.redRed s)))

-- Did some green-black seed put a creature into a graveyard? In green-black the
-- only way a creature dies is combat (trade, deathtouch SBA, or trample), so
-- this fails only if combat never engages across all these seeds.
creatureDied :: Int -> Bool
creatureDied s =
  let gs = S.runRandomGame S.greenBlack s
      isDeadCreature oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
      inGrave pid = any isDeadCreature (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [S.alice, S.bob]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Properties" [propertyTests]
