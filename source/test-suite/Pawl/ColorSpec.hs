-- Covers: Pawl.Engine.Projection (an object's CR 613 layer-5 colour, including CR
-- 702.114a devoid and CR 111.3 token colour), Pawl.Engine.Target (NonblackCreatureTarget)
-- and the P3a colour gates (Doom Blade, Crimson Wisps, Aphotic Wisps, Bad Moon,
-- Dragon Fodder), plus the CR 608.2b colour-change fizzle. Gameplay-level: each
-- card is cast or resolved through the stack and the resulting game state is
-- asserted on.
module Pawl.ColorSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Engine.Filter may later be imported and must not collide.

import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- "target nonblack creature", the spec Doom Blade and the CR 115.1a cases share.
nonblackCreature :: TargetSpec.TargetSpec
nonblackCreature = TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Color" $ do
  Spec.it s "CR 202.2 a mono-black card's colour is black" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, gs) = S.addCreature typhoidRats S.alice gs0
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.singleton Color.Black

  Spec.it s "CR 202.2 a generic-plus-red cost is red, and generic contributes nothing" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (pikerId, gs) = S.addCreature piker S.alice gs0
    Spec.assertEq s (Projection.colorsOf pikerId gs) $ Set.singleton Color.Red

  Spec.it s "CR 202.2b an object with no coloured mana symbols is colourless" $ do
    darksteelMyr <- S.printingOf s registry "Darksteel Myr"
    let gs0 = Setup.emptyGame S.bothPlayers
        (myrId, gs) = S.addCreature darksteelMyr S.alice gs0
    Spec.assertEq s (Projection.colorsOf myrId gs) Set.empty

  Spec.it s "CR 202.1b a land has no mana cost, so it is colourless" $ do
    mountain <- S.printingOf s registry "Mountain"
    let gs0 = Setup.emptyGame S.bothPlayers
        (mtnId, gs) = S.addCreature mountain S.alice gs0
    Spec.assertEq s (Projection.colorsOf mtnId gs) Set.empty

  Spec.it s "CR 702.114a devoid makes an object colourless despite a black mana cost" $ do
    -- THE FALSIFIER for "an object's colours are the coloured symbols in its
    -- mana cost": this card's cost is {1}{B} and it is colourless.
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addCreature slaughterDrone S.alice gs0
    Spec.assertEq s (Projection.colorsOf droneId gs) Set.empty

  Spec.it s "CR 105.3 a new colour REPLACES all previous colours" $ do
    -- THE FALSIFIER for implementing "becomes red" as an ADD: the Rats are
    -- black, and after the effect they are red and NOT black.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addCreature typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.singleton Color.Red

  Spec.it s "CR 105.3 an effect may make a coloured object colourless" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addCreature typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.SetColor Set.empty) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) Set.empty

  Spec.it s "CR 613.1e a layer-5 colour change beats the CR 702.114a devoid seed" $ do
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, board) = S.addCreature slaughterDrone S.alice gs0
        gs = S.withEffect droneId (Modification.SetColor (Set.singleton Color.Black)) board
    Spec.assertEq s (Projection.colorsOf droneId gs) $ Set.singleton Color.Black

  Spec.it s "Bad Moon pumps a black creature but not a red one" $ do
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addCreature badMoon S.alice gs0
        (ratsId, withRats) = S.addCreature typhoidRats S.alice withMoon
        (pikerId, gs) = S.addCreature piker S.alice withRats
    Spec.assertEqWith s "the black Rats are 2/2" (Projection.powerOf ratsId gs) $ Just 2
    Spec.assertEqWith s "the red Piker is unchanged at 2" (Projection.powerOf pikerId gs) $ Just 2
    Spec.assertEqWith s "the red Piker's toughness is unchanged at 1" (Projection.toughnessOf pikerId gs) $ Just 1

  Spec.it s "CR 702.114a Bad Moon does not pump a devoid creature with a black mana cost" $ do
    -- FALSIFIER, reader (b) half: a naive "colours are the mana cost's
    -- symbols" implementation pumps this 2/2 to 3/3.
    badMoon <- S.printingOf s registry "Bad Moon"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addCreature badMoon S.alice gs0
        (droneId, gs) = S.addCreature slaughterDrone S.alice withMoon
    Spec.assertEqWith s "power unchanged" (Projection.powerOf droneId gs) $ Just 2
    Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf droneId gs) $ Just 2

  Spec.it s "CR 613 a layer-5 colour change moves a creature INTO Bad Moon's set" $ do
    badMoon <- S.printingOf s registry "Bad Moon"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, withMoon) = S.addCreature badMoon S.alice gs0
        (pikerId, board) = S.addCreature piker S.alice withMoon
        gs = S.withEffect pikerId (Modification.SetColor (Set.singleton Color.Black)) board
    Spec.assertEqWith s "the now-black Piker is 3/2" (Projection.powerOf pikerId gs) $ Just 3

  Spec.it s "CR 111.3 a token's colour comes from the effect that created it" $ do
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
    mountain <- S.printingOf s registry "Mountain"
    badMoon <- S.printingOf s registry "Bad Moon"
    dragonFodder <- S.printingOf s registry "Dragon Fodder"
    let base = S.landsInPlay mountain 2
        (_, withMoon) = S.addCreature badMoon S.alice base
        (gs, spellId) = S.handOne dragonFodder withMoon
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case S.tokensOf after of
      [] -> Spec.assertFailure s "Dragon Fodder made no tokens"
      tokenIds -> do
        Spec.assertEqWith s "two Goblins" (length tokenIds) 2
        mapM_
          (\oid -> Spec.assertEqWith s "red" (Projection.colorsOf oid after) (Set.singleton Color.Red))
          tokenIds
        mapM_
          (\oid -> Spec.assertEqWith s "Bad Moon does not pump a red token" (Projection.powerOf oid after) (Just 1))
          tokenIds

  Spec.it s "CR 115.1a a black creature is not a legal 'target nonblack creature'" $ do
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    piker <- S.printingOf s registry "Goblin Piker"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, withRats) = S.addCreature typhoidRats S.alice gs0
        (pikerId, gs) = S.addCreature piker S.alice withRats
        legal = Target.legalRecipients Nothing S.noSource nonblackCreature gs
    Spec.assertBool s (Set.member (Recipient.ToCreature pikerId) legal) "the red Piker is legal"
    Spec.assertBool s (not (Set.member (Recipient.ToCreature ratsId) legal)) "the black Rats are not"

  Spec.it s "CR 702.114a a devoid creature with a black cost IS a legal nonblack target" $ do
    -- FALSIFIER, reader (a) half.
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addCreature slaughterDrone S.alice gs0
        legal = Target.legalRecipients Nothing S.noSource nonblackCreature gs
    Spec.assertBool s (Set.member (Recipient.ToCreature droneId) legal) "colourless is nonblack"

  Spec.it s "Doom Blade destroys a devoid creature whose mana cost is black" $ do
    swamp <- S.printingOf s registry "Swamp"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    doomBlade <- S.printingOf s registry "Doom Blade"
    let base = S.landsInPlay swamp 2
        (_, board) = S.addCreature slaughterDrone S.bob base
        (gs, dbId) = S.handOne doomBlade board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice dbId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "the Drone is gone" (length (Game.zoneMembers Zone.Battlefield S.bob after)) 0

  Spec.it s "Crimson Wisps makes a black creature red, and it stops being black" $ do
    -- THE SET-NOT-ADD FALSIFIER, end to end: under Bad Moon the Rats are 2/2
    -- and no legal Doom Blade target; after Crimson Wisps they are a 1/1 red
    -- creature that Doom Blade may target. An AddColor implementation fails
    -- every one of these assertions.
    mountain <- S.printingOf s registry "Mountain"
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    crimsonWisps <- S.printingOf s registry "Crimson Wisps"
    let base = S.landsInPlay mountain 1
        (_, withMoon) = S.addCreature badMoon S.alice base
        (ratsId, board) = S.addCreature typhoidRats S.alice withMoon
        (gs, cwId) = S.handOne crimsonWisps board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice cwId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "before: the black Rats are 2/2 under Bad Moon" (Projection.powerOf ratsId board) $ Just 2
    Spec.assertEqWith s "after: red only, not black and red" (Projection.colorsOf ratsId after) $ Set.singleton Color.Red
    Spec.assertEqWith s "after: out of Bad Moon's set, back to 1 power" (Projection.powerOf ratsId after) $ Just 1
    Spec.assertBool
      s
      (Set.member (Recipient.ToCreature ratsId) (Target.legalRecipients Nothing S.noSource nonblackCreature after))
      "after: a legal Doom Blade target"

  Spec.it s "Aphotic Wisps makes a creature black, the mirror of Crimson Wisps" $ do
    swamp <- S.printingOf s registry "Swamp"
    badMoon <- S.printingOf s registry "Bad Moon"
    piker <- S.printingOf s registry "Goblin Piker"
    aphoticWisps <- S.printingOf s registry "Aphotic Wisps"
    let base = S.landsInPlay swamp 1
        (_, withMoon) = S.addCreature badMoon S.alice base
        (pikerId, board) = S.addCreature piker S.alice withMoon
        (gs, awId) = S.handOne aphoticWisps board
        cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice awId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "black only, not red and black" (Projection.colorsOf pikerId after) $ Set.singleton Color.Black
    Spec.assertEqWith s "INTO Bad Moon's set, now 3 power" (Projection.powerOf pikerId after) $ Just 3
    Spec.assertBool s (Projection.hasKeyword Keyword.Fear pikerId after) "gained fear"
    Spec.assertBool
      s
      (not (Set.member (Recipient.ToCreature pikerId) (Target.legalRecipients Nothing S.noSource nonblackCreature after)))
      "no longer a legal Doom Blade target"

  Spec.it s "CR 608.2b Doom Blade fizzles when its target becomes black in response" $ do
    -- The fizzle is only reachable through a colour change, and Aphotic Wisps
    -- is the one card in the pool that makes something BLACK.
    swamp <- S.printingOf s registry "Swamp"
    llanowarElves <- S.printingOf s registry "Llanowar Elves"
    doomBlade <- S.printingOf s registry "Doom Blade"
    aphoticWisps <- S.printingOf s registry "Aphotic Wisps"
    let base = S.landsInPlay swamp 3
        (elvesId, board) = S.addCreature llanowarElves S.bob base
        (gs1, dbId) = S.handOne doomBlade board
        (gs2, awId) = S.handOne aphoticWisps gs1
        castDb = snd (Engine.runGamePure S.identityAnswer gs2 (Cast.castSpell S.alice dbId))
        castAw = snd (Engine.runGamePure S.identityAnswer castDb (Cast.castSpell S.alice awId))
        -- Aphotic Wisps is on top, so it resolves first and turns the green
        -- Elves black; Doom Blade then re-checks its target (CR 608.2b).
        after = snd (Engine.runGamePure S.identityAnswer castAw (Stack.resolveTop >> Stack.resolveTop))
    Spec.assertEqWith s "the Elves are black" (Projection.colorsOf elvesId after) $ Set.singleton Color.Black
    Spec.assertBool s (Set.member elvesId (GameState.battlefield after)) "the Elves survive"
    Spec.assertEqWith s "both spells are in alice's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2

  Spec.it s "CR 105.3 2008-05-01 a colour change overwrites ALL previous colours, even a multicoloured one" $ do
    -- Gatherer ruling on Crimson Wisps / Aphotic Wisps (WotC, 2008-05-01):
    -- "An effect that changes a permanent's colors overwrites all its old
    -- colors unless it specifically says 'in addition to its other
    -- colors.' ... It doesn't matter what colors it used to be (even if,
    -- for example, it used to be blue and black)." Transcribed with two
    -- stacked SetColor effects rather than a card, since no card in the
    -- pool is printed multicoloured: the later effect leaves no residue
    -- of either colour the earlier one set, not just the printed colour.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addCreature typhoidRats S.alice gs0
        multi = S.withEffect ratsId (Modification.SetColor (Set.fromList [Color.Blue, Color.Black])) board
        gs = S.withEffect ratsId (Modification.SetColor (Set.singleton Color.Red)) multi
    Spec.assertEqWith s "red only, no residue of blue or black" (Projection.colorsOf ratsId gs) $ Set.singleton Color.Red

  Spec.it s "CR 613.1c/613.1e 2008-05-01 changing a permanent's colour doesn't change its text" $ do
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
    badMoon <- S.printingOf s registry "Bad Moon"
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (moonId, withMoon) = S.addCreature badMoon S.alice gs0
        (ratsId, board) = S.addCreature typhoidRats S.alice withMoon
        gs = S.withEffect moonId (Modification.SetColor (Set.singleton Color.Red)) board
    Spec.assertEqWith s "Bad Moon itself is now red" (Projection.colorsOf moonId gs) $ Set.singleton Color.Red
    Spec.assertEqWith s "the black Rats are still pumped to 2 power" (Projection.powerOf ratsId gs) $ Just 2
    Spec.assertEqWith s "the black Rats are still pumped to 2 toughness" (Projection.toughnessOf ratsId gs) $ Just 2
