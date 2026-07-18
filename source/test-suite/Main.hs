{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Card as Card
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.DamageSpec as DamageSpec
import qualified Pawl.Game as Game
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.ReplaySpec as ReplaySpec
import qualified Pawl.ResolveSpec as ResolveSpec
import qualified Pawl.Setup as Setup
import qualified Pawl.SetupSpec as SetupSpec
import Pawl.Support
  ( alice,
    bob,
    greenBlack,
    matchups,
    redRed,
    runRandomGame,
  )
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests,
      CardSpec.tests,
      TurnSpec.tests,
      GameSpec.tests,
      SetupSpec.tests,
      DamageSpec.tests,
      ReplaySpec.tests,
      propertyTests,
      ManaSpec.tests,
      CastSpec.tests,
      CombatSpec.tests,
      ResolveSpec.tests
    ]

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        QC.conjoin (map (\m -> Game.objectCount (runRandomGame m s) QC.=== 120) matchups),
      -- The property that matters most now. Combat is the first thing that can
      -- end a game before the library runs out.
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.conjoin (map (\m -> QC.property (Maybe.isJust (GameState.result (runRandomGame m s)))) matchups),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.conjoin (map (\m -> QC.property (nextIdOf (runRandomGame m s) >= 120)) matchups),
      QC.testProperty "no mana floats at the end" $ \s ->
        QC.conjoin (map (\m -> GameState.manaPool (runRandomGame m s) QC.=== Map.empty) matchups),
      -- Replaces M0's "no life changes". Nothing here GAINS life, so any
      -- increase is a bug. Dies at lifelink (still unscheduled -- see the
      -- design doc's punchlist), the same way this property's ancestor
      -- announced M1b.
      QC.testProperty "life never increases" $ \s ->
        QC.conjoin
          ( map
              (\m -> QC.property (all (\pl -> Player.life pl <= Setup.startingLife) (Map.elems (GameState.players (runRandomGame m s)))))
              matchups
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
  let gs = runRandomGame redRed s
      isBolt oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Printing.card printing == Printing.card Card.lightningBoltPrinting
      inGrave pid = any isBolt (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [alice, bob]

-- Did anyone's life total move over the course of the game this seed produces?
someLifeChanged :: Int -> Bool
someLifeChanged s =
  let moved pl = Player.life pl /= Setup.startingLife
   in any moved (Map.elems (GameState.players (runRandomGame redRed s)))

-- Did some green-black seed put a creature into a graveyard? In green-black the
-- only way a creature dies is combat (trade, deathtouch SBA, or trample), so
-- this fails only if combat never engages across all these seeds.
creatureDied :: Int -> Bool
creatureDied s =
  let gs = runRandomGame greenBlack s
      isDeadCreature oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard printing -> Card.isCreature (Printing.card printing)
      inGrave pid = any isDeadCreature (Game.zoneMembers Zone.Graveyard pid gs)
   in any inGrave [alice, bob]
