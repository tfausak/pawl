-- Covers: Pawl.Projection (an object's CR 613 layer-5 colour, including CR
-- 702.114a devoid and CR 111.3 token colour), Pawl.Target (NonblackCreatureTarget)
-- and the P3a colour gates (Doom Blade, Crimson Wisps, Aphotic Wisps, Bad Moon,
-- Dragon Fodder), plus the CR 608.2b colour-change fizzle. Gameplay-level: each
-- card is cast or resolved through the stack and the resulting game state is
-- asserted on.
module Pawl.ColorSpec where

import qualified Data.Set as Set
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.Color as Color
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Keyword as Keyword
import qualified Pawl.Type.Modification as Modification
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Color"
    [ HU.testCase "CR 202.2 a mono-black card's colour is black" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, gs) = S.addCreature typhoidRats S.alice gs0
        HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 202.2 a generic-plus-red cost is red, and generic contributes nothing" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, gs) = S.addCreature piker S.alice gs0
        HU.assertEqual "red" (Set.singleton Color.Red) (Projection.colorsOf pikerId gs),
      HU.testCase "CR 202.2b an object with no coloured mana symbols is colourless" $ do
        darksteelMyr <- Registry.printing registry "Darksteel Myr"
        let gs0 = Setup.emptyGame S.bothPlayers
            (myrId, gs) = S.addCreature darksteelMyr S.alice gs0
        HU.assertEqual "colourless" Set.empty (Projection.colorsOf myrId gs),
      HU.testCase "CR 202.1b a land has no mana cost, so it is colourless" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs0 = Setup.emptyGame S.bothPlayers
            (mtnId, gs) = S.addCreature mountain S.alice gs0
        HU.assertEqual "colourless" Set.empty (Projection.colorsOf mtnId gs),
      HU.testCase "CR 702.114a devoid makes an object colourless despite a black mana cost" $ do
        -- THE FALSIFIER for "an object's colours are the coloured symbols in its
        -- mana cost": this card's cost is {1}{B} and it is colourless.
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, gs) = S.addCreature devoidDrone S.alice gs0
        HU.assertEqual "colourless" Set.empty (Projection.colorsOf droneId gs),
      HU.testCase "CR 105.3 a new colour REPLACES all previous colours" $ do
        -- THE FALSIFIER for implementing "becomes red" as an ADD: the Rats are
        -- black, and after the effect they are red and NOT black.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature typhoidRats S.alice gs0
            gs = S.withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) board
        HU.assertEqual "red only" (Set.singleton Color.Red) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 105.3 an effect may make a coloured object colourless" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature typhoidRats S.alice gs0
            gs = S.withEffect ratsId (Modification.SetColor Set.empty) board
        HU.assertEqual "colourless" Set.empty (Projection.colorsOf ratsId gs),
      HU.testCase "CR 613.1e a layer-5 colour change beats the CR 702.114a devoid seed" $ do
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, board) = S.addCreature devoidDrone S.alice gs0
            gs = S.withEffect droneId (Modification.SetColor (Set.singleton Color.Black)) board
        HU.assertEqual "black" (Set.singleton Color.Black) (Projection.colorsOf droneId gs),
      HU.testCase "Bad Moon pumps a black creature but not a red one" $ do
        badMoon <- Registry.printing registry "Bad Moon"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature badMoon S.alice gs0
            (ratsId, withRats) = S.addCreature typhoidRats S.alice withMoon
            (pikerId, gs) = S.addCreature piker S.alice withRats
        HU.assertEqual "the black Rats are 2/2" (Just 2) (Projection.powerOf ratsId gs)
        HU.assertEqual "the red Piker is unchanged at 2" (Just 2) (Projection.powerOf pikerId gs)
        HU.assertEqual "the red Piker's toughness is unchanged at 1" (Just 1) (Projection.toughnessOf pikerId gs),
      HU.testCase "CR 702.114a Bad Moon does not pump a devoid creature with a black mana cost" $ do
        -- FALSIFIER, reader (b) half: a naive "colours are the mana cost's
        -- symbols" implementation pumps this 2/2 to 3/3.
        badMoon <- Registry.printing registry "Bad Moon"
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature badMoon S.alice gs0
            (droneId, gs) = S.addCreature devoidDrone S.alice withMoon
        HU.assertEqual "power unchanged" (Just 2) (Projection.powerOf droneId gs)
        HU.assertEqual "toughness unchanged" (Just 2) (Projection.toughnessOf droneId gs),
      HU.testCase "CR 613 a layer-5 colour change moves a creature INTO Bad Moon's set" $ do
        badMoon <- Registry.printing registry "Bad Moon"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withMoon) = S.addCreature badMoon S.alice gs0
            (pikerId, board) = S.addCreature piker S.alice withMoon
            gs = S.withEffect pikerId (Modification.SetColor (Set.singleton Color.Black)) board
        HU.assertEqual "the now-black Piker is 3/2" (Just 3) (Projection.powerOf pikerId gs),
      HU.testCase "CR 111.3 a token's colour comes from the effect that created it" $ do
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
        mountain <- Registry.printing registry "Mountain"
        badMoon <- Registry.printing registry "Bad Moon"
        dragonFodder <- Registry.printing registry "Dragon Fodder"
        let base = S.landsInPlay mountain 2
            (_, withMoon) = S.addCreature badMoon S.alice base
            (gs, spellId) = S.handOne dragonFodder withMoon
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        case S.tokensOf after of
          [] -> HU.assertFailure "Dragon Fodder made no tokens"
          tokenIds -> do
            HU.assertEqual "two Goblins" 2 (length tokenIds)
            mapM_
              (\oid -> HU.assertEqual "red" (Set.singleton Color.Red) (Projection.colorsOf oid after))
              tokenIds
            mapM_
              (\oid -> HU.assertEqual "Bad Moon does not pump a red token" (Just 1) (Projection.powerOf oid after))
              tokenIds,
      HU.testCase "CR 115.1a a black creature is not a legal 'target nonblack creature'" $ do
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, withRats) = S.addCreature typhoidRats S.alice gs0
            (pikerId, gs) = S.addCreature piker S.alice withRats
            legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))) gs
        HU.assertBool "the red Piker is legal" (Set.member (Recipient.ToCreature pikerId) legal)
        HU.assertBool "the black Rats are not" (not (Set.member (Recipient.ToCreature ratsId) legal)),
      HU.testCase "CR 702.114a a devoid creature with a black cost IS a legal nonblack target" $ do
        -- FALSIFIER, reader (a) half.
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (droneId, gs) = S.addCreature devoidDrone S.alice gs0
            legal = Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))) gs
        HU.assertBool "colourless is nonblack" (Set.member (Recipient.ToCreature droneId) legal),
      HU.testCase "Doom Blade destroys a devoid creature whose mana cost is black" $ do
        swamp <- Registry.printing registry "Swamp"
        devoidDrone <- Registry.printing registry "Synthetic Devoid Drone"
        doomBlade <- Registry.printing registry "Doom Blade"
        let base = S.landsInPlay swamp 2
            (_, board) = S.addCreature devoidDrone S.bob base
            (gs, dbId) = S.handOne doomBlade board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice dbId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "the Drone is gone" 0 (length (Game.zoneMembers Zone.Battlefield S.bob after)),
      HU.testCase "Crimson Wisps makes a black creature red, and it stops being black" $ do
        -- THE SET-NOT-ADD FALSIFIER, end to end: under Bad Moon the Rats are 2/2
        -- and no legal Doom Blade target; after Crimson Wisps they are a 1/1 red
        -- creature that Doom Blade may target. An AddColor implementation fails
        -- every one of these assertions.
        mountain <- Registry.printing registry "Mountain"
        badMoon <- Registry.printing registry "Bad Moon"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        crimsonWisps <- Registry.printing registry "Crimson Wisps"
        let base = S.landsInPlay mountain 1
            (_, withMoon) = S.addCreature badMoon S.alice base
            (ratsId, board) = S.addCreature typhoidRats S.alice withMoon
            (gs, cwId) = S.handOne crimsonWisps board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cwId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "before: the black Rats are 2/2 under Bad Moon" (Just 2) (Projection.powerOf ratsId board)
        HU.assertEqual "after: red only, not black and red" (Set.singleton Color.Red) (Projection.colorsOf ratsId after)
        HU.assertEqual "after: out of Bad Moon's set, back to 1 power" (Just 1) (Projection.powerOf ratsId after)
        HU.assertBool
          "after: a legal Doom Blade target"
          (Set.member (Recipient.ToCreature ratsId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))) after)),
      HU.testCase "Aphotic Wisps makes a creature black, the mirror of Crimson Wisps" $ do
        swamp <- Registry.printing registry "Swamp"
        badMoon <- Registry.printing registry "Bad Moon"
        piker <- Registry.printing registry "Goblin Piker"
        aphoticWisps <- Registry.printing registry "Aphotic Wisps"
        let base = S.landsInPlay swamp 1
            (_, withMoon) = S.addCreature badMoon S.alice base
            (pikerId, board) = S.addCreature piker S.alice withMoon
            (gs, awId) = S.handOne aphoticWisps board
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice awId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "black only, not red and black" (Set.singleton Color.Black) (Projection.colorsOf pikerId after)
        HU.assertEqual "INTO Bad Moon's set, now 3 power" (Just 3) (Projection.powerOf pikerId after)
        HU.assertBool "gained fear" (Projection.hasKeyword Keyword.Fear pikerId after)
        HU.assertBool
          "no longer a legal Doom Blade target"
          (not (Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients Nothing S.noSource (TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))) after))),
      HU.testCase "CR 608.2b Doom Blade fizzles when its target becomes black in response" $ do
        -- The fizzle is only reachable through a colour change, and Aphotic Wisps
        -- is the one card in the pool that makes something BLACK.
        swamp <- Registry.printing registry "Swamp"
        llanowarElves <- Registry.printing registry "Llanowar Elves"
        doomBlade <- Registry.printing registry "Doom Blade"
        aphoticWisps <- Registry.printing registry "Aphotic Wisps"
        let base = S.landsInPlay swamp 3
            (elvesId, board) = S.addCreature llanowarElves S.bob base
            (gs1, dbId) = S.handOne doomBlade board
            (gs2, awId) = S.handOne aphoticWisps gs1
            castDb = snd (Engine.runGamePure S.identityAnswer gs2 (Cast.castSpell S.alice dbId))
            castAw = snd (Engine.runGamePure S.identityAnswer castDb (Cast.castSpell S.alice awId))
            -- Aphotic Wisps is on top, so it resolves first and turns the green
            -- Elves black; Doom Blade then re-checks its target (CR 608.2b).
            after = snd (Engine.runGamePure S.identityAnswer castAw (Stack.resolveTop >> Stack.resolveTop))
        HU.assertEqual "the Elves are black" (Set.singleton Color.Black) (Projection.colorsOf elvesId after)
        HU.assertBool "the Elves survive" (Set.member elvesId (GameState.battlefield after))
        HU.assertEqual "both spells are in alice's graveyard" 2 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      HU.testCase "CR 105.3 2008-05-01 a colour change overwrites ALL previous colours, even a multicoloured one" $ do
        -- Gatherer ruling on Crimson Wisps / Aphotic Wisps (WotC, 2008-05-01):
        -- "An effect that changes a permanent's colors overwrites all its old
        -- colors unless it specifically says 'in addition to its other
        -- colors.' ... It doesn't matter what colors it used to be (even if,
        -- for example, it used to be blue and black)." Transcribed with two
        -- stacked SetColor effects rather than a card, since no card in the
        -- pool is printed multicoloured: the later effect leaves no residue
        -- of either colour the earlier one set, not just the printed colour.
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (ratsId, board) = S.addCreature typhoidRats S.alice gs0
            multi = S.withEffect ratsId (Modification.SetColor (Set.fromList [Color.Blue, Color.Black])) board
            gs = S.withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) multi
        HU.assertEqual "red only, no residue of blue or black" (Set.singleton Color.Red) (Projection.colorsOf ratsId gs),
      HU.testCase "CR 613.1c/613.1e 2008-05-01 changing a permanent's colour doesn't change its text" $ do
        -- Gatherer ruling on Crimson Wisps / Aphotic Wisps (WotC, 2008-05-01):
        -- "Changing a permanent's color won't change its text. If you turn
        -- Wilt-Leaf Liege blue, it will still affect green creatures and
        -- white creatures." Transcribed with Bad Moon, whose own text
        -- ("Black creatures get +1/+1") is keyed to its OWN printed colour:
        -- turn Bad Moon itself red with a stored SetColor effect (S.withEffect
        -- reaches non-creature permanents, which no card in the pool
        -- targets) and its ability still reads Typhoid Rats as black. This
        -- guards CR 613.1e/613.1c's layer separation -- colour is layer 5,
        -- text-changing is layer 3 -- and must never be implemented as a
        -- colour change rewriting the object's own text.
        badMoon <- Registry.printing registry "Bad Moon"
        typhoidRats <- Registry.printing registry "Typhoid Rats"
        let gs0 = Setup.emptyGame S.bothPlayers
            (moonId, withMoon) = S.addCreature badMoon S.alice gs0
            (ratsId, board) = S.addCreature typhoidRats S.alice withMoon
            gs = S.withEffect moonId (Modification.SetColor (Set.singleton Color.Red)) board
        HU.assertEqual "Bad Moon itself is now red" (Set.singleton Color.Red) (Projection.colorsOf moonId gs)
        HU.assertEqual "the black Rats are still pumped to 2 power" (Just 2) (Projection.powerOf ratsId gs)
        HU.assertEqual "the black Rats are still pumped to 2 toughness" (Just 2) (Projection.toughnessOf ratsId gs)
    ]
