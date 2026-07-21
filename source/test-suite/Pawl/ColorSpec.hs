-- Covers: Pawl.Projection (an object's CR 613 layer-5 colour), Pawl.Target
-- (NonblackCreatureTarget) and the P3a colour gates (Doom Blade, Crimson Wisps,
-- Aphotic Wisps, Bad Moon). Gameplay-level: each card is cast or resolved through
-- the stack and the resulting game state is asserted on.
module Pawl.ColorSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Affected as Affected
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.ContinuousEffect as ContinuousEffect
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Source as Source
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Append a stored continuous effect affecting exactly `oid`. Object id 996 is a
-- stand-in source: nothing in these tests reads the source's own characteristics.
withEffect :: ObjectId.ObjectId -> Modification.Modification -> GameState.GameState -> GameState.GameState
withEffect oid m gs =
  let (ts, gs1) = Game.freshTimestamp gs
      eff =
        ContinuousEffect.MkContinuousEffect
          { ContinuousEffect.source = ObjectId.MkObjectId 996,
            ContinuousEffect.timestamp = ts,
            ContinuousEffect.duration = Duration.UntilEndOfTurn,
            ContinuousEffect.modification = m,
            ContinuousEffect.affected = Affected.TheseObjects (Set.singleton oid)
          }
   in gs1 {GameState.continuousEffects = eff : GameState.continuousEffects gs1}

-- The battlefield objects that are tokens (CR 111.1) rather than cards.
tokensOf :: GameState.GameState -> [ObjectId.ObjectId]
tokensOf gs = filter isToken (Set.toList (GameState.battlefield gs))
  where
    isToken oid = case fmap Object.source (Game.lookupObject oid gs) of
      Just (Source.OfToken _) -> True
      _ -> False

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Color"
    [ HU.testCase "CR 202.2 a mono-black card's colour is black" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, gs) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
         in HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 202.2 a generic-plus-red cost is red, and generic contributes nothing" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs) = S.addPiker cards S.alice gs0
         in HU.assertEqual "red" (Set.singleton Color.Red) (Projection.colorsOf pikerId gs),
      HU.testCase "CR 202.2b an object with no coloured mana symbols is colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (myrId, gs) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf myrId gs),
      HU.testCase "CR 202.1b a land has no mana cost, so it is colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (mtnId, gs) = S.addCreature (Cards.mountainPrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf mtnId gs),
      HU.testCase "CR 702.114a devoid makes an object colourless despite a black mana cost" $
        -- THE FALSIFIER for "an object's colours are the coloured symbols in its
        -- mana cost": this card's cost is {1}{B} and it is colourless.
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, gs) = S.addCreature (Cards.devoidDronePrinting cards) S.alice gs0
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf droneId gs),
      HU.testCase "CR 105.3 a new colour REPLACES all previous colours" $
        -- THE FALSIFIER for implementing "becomes red" as an ADD: the Rats are
        -- black, and after the effect they are red and NOT black.
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
            gs = withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) board
         in HU.assertEqual "red only" (Set.singleton Color.Red) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 105.3 an effect may make a coloured object colourless" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice gs0
            gs = withEffect ratsId (Modification.SetColor Set.empty) board
         in HU.assertEqual "colourless" Set.empty (Projection.colorsOf ratsId gs),
      HU.testCase "CR 613.1e a layer-5 colour change beats the CR 702.114a devoid seed" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, board) = S.addCreature (Cards.devoidDronePrinting cards) S.alice gs0
            gs = withEffect droneId (Modification.SetColor (Set.singleton Color.Black)) board
         in HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf droneId gs),
      HU.testCase "Bad Moon pumps a black creature but not a red one" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature (Cards.badMoonPrinting cards) S.alice gs0
            (ratsId, withRats) = S.addCreature (Cards.typhoidRatsPrinting cards) S.alice withMoon
            (pikerId, gs) = S.addPiker cards S.alice withRats
         in do
              HU.assertEqual "the black Rats are 2/2" (Just 2) (Projection.powerOf ratsId gs)
              HU.assertEqual "the red Piker is unchanged at 2" (Just 2) (Projection.powerOf pikerId gs)
              HU.assertEqual "the red Piker's toughness is unchanged at 1" (Just 1) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 702.114a Bad Moon does not pump a devoid creature with a black mana cost" $
        -- FALSIFIER, reader (b) half: a naive "colours are the mana cost's
        -- symbols" implementation pumps this 2/2 to 3/3.
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature (Cards.badMoonPrinting cards) S.alice gs0
            (droneId, gs) = S.addCreature (Cards.devoidDronePrinting cards) S.alice withMoon
         in do
              HU.assertEqual "power unchanged" (Just 2) (Projection.powerOf droneId gs)
              HU.assertEqual "toughness unchanged" (Just 2) (Projection.toughnessOf droneId gs),
      HU.testCase "CR 613 a layer-5 colour change moves a creature INTO Bad Moon's set" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature (Cards.badMoonPrinting cards) S.alice gs0
            (pikerId, board) = S.addPiker cards S.alice withMoon
            gs = withEffect pikerId (Modification.SetColor (Set.singleton Color.Black)) board
         in HU.assertEqual "the now-black Piker is 3/2" (Just 3) (Projection.powerOf pikerId gs),
      HU.testCase "CR 111.3 a token's colour comes from the effect that created it" $
        -- FALSIFIER: a token has no mana cost, so an implementation that derives
        -- colour from the mana cost alone makes Dragon Fodder's Goblins
        -- COLOURLESS -- and Bad Moon is what makes that observable, since
        -- colourless reads as "nonblack" exactly as red does.
        -- S.spellOnStack places the object directly in the Stack zone, bypassing
        -- Cast.castSpell's mode-selection prompt; with an empty bindings map,
        -- Binding.modesOf is empty and Dragon Fodder's Create effect never fires
        -- (proven: even after Step 3's data fix, the empty-binding path still
        -- makes zero tokens). This needs a real cast, mirroring ResolveSpec's
        -- "CR 111 Dragon Fodder creates two 1/1 Goblin tokens".
        let base = S.mountainsInPlay cards 2
            (_, withMoon) = S.addCreature (Cards.badMoonPrinting cards) S.alice base
            (gs, spellId) = S.handOne (Cards.dragonFodderPrinting cards) withMoon
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
         in case tokensOf after of
              [] -> HU.assertFailure "Dragon Fodder made no tokens"
              tokenIds -> do
                HU.assertEqual "two Goblins" 2 (length tokenIds)
                mapM_
                  (\oid -> HU.assertEqual "red" (Set.singleton Color.Red) (Projection.colorsOf oid after))
                  tokenIds
                mapM_
                  (\oid -> HU.assertEqual "Bad Moon does not pump a red token" (Just 1) (Projection.powerOf oid after))
                  tokenIds
    ]
