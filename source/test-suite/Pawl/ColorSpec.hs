{-# LANGUAGE GADTs #-}

-- Covers: Pawl.Engine.Projection (an object's CR 613 layer-5 colour, including CR
-- 702.114a devoid, CR 613.3's characteristic-defining-ability-first ordering
-- within layer 5, and CR 111.3 token colour), Pawl.Engine.Target (the "target
-- nonblack creature" filter below, and Red Elemental Blast's two colour-filtered
-- pools read straight off the card), the P3a colour gates (Doom Blade, Crimson
-- Wisps, Aphotic Wisps, Bad Moon, Dragon Fodder) and this phase's own CR 613.3
-- gates (Indigo Faerie, Painter's Servant, Red Elemental Blast), plus the CR
-- 608.2b colour-change fizzle.
-- Mostly gameplay-level: a card is cast or resolved through the stack and the
-- resulting game state is asserted on. The rest assert on a projection over a
-- hand-built board, either because no card in the pool produces the effect
-- (S.withEffect) or because the claim is about a layer BOUNDARY and only
-- projectUpTo can see one ("devoid applies at the START of layer 5").
module Pawl.ColorSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
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
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Layer as Layer
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone

-- "target nonblack creature", the spec Doom Blade and the CR 115.1a cases share.
nonblackCreature :: TargetSpec.TargetSpec
nonblackCreature = TargetSpec.MkTargetSpec Pool.Creatures (Just (Filter.Type.Not (Filter.Type.HasColor Color.Black)))

-- Red Elemental Blast's two modes, in printed order (CR 700.2 /
-- data/cards/red-elemental-blast.json):
--   0. "Counter target blue spell."     -- slot "spell", Pool.Spells
--   1. "Destroy target blue permanent." -- slot "permanent", Pool.Permanents
counterMode :: ModeIndex.ModeIndex
counterMode = ModeIndex.MkModeIndex 0

destroyMode :: ModeIndex.ModeIndex
destroyMode = ModeIndex.MkModeIndex 1

-- The one TargetSpec a mode of a printing's spell declares, read out of the
-- JSON-loaded card BY INDEX -- so a swapped mode order is as visible to a
-- legality assertion here as a wrong pool or a wrong filter is. Nothing when the
-- index is out of range or the mode does not declare exactly one slot; the
-- callers assert on that rather than defaulting to a hand-built spec.
modeSpec :: Printing.Printing -> ModeIndex.ModeIndex -> Maybe TargetSpec.TargetSpec
modeSpec printing idx = case fmap Map.elems (Card.modeTargetSpecs idx (Printing.card printing)) of
  Just [only] -> Just only
  _ -> Nothing

-- The single activated ability of a printing (Indigo Faerie has exactly one).
-- Same shape as ActivateSpec.theAbility -- duplicated per this test suite's
-- existing convention of group-local helpers (ActivateSpec, ReplacementSpec)
-- rather than centralizing a helper this small in Support.
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (Modal.MkModal (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory)) (ModeSelection.ChooseExactly 1)) ActivationTiming.AnyTime

-- Answers CR 614.1c's as-enters colour choice with blue, deferring everything
-- else to S.identityAnswer -- whose own default for that prompt is WHITE, so a
-- test that reached this answer by accident would not see blue.
choosingBlue :: Prompt.Prompt r -> r
choosingBlue p = case p of
  Prompt.ChooseColor {} -> Color.Blue
  _ -> S.identityAnswer p

-- Aims every ChooseTargets slot at one object, deferring the rest to
-- S.identityAnswer -- ProjectionSpec.aimAtObject's shape, duplicated because
-- Indigo Faerie's "target permanent" (Pool.Permanents) answers with ToObject.
aimAtObject :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtObject oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  _ -> S.identityAnswer p

-- Casts Red Elemental Blast: chooses mode `idx` at CR 700.2's mode prompt and
-- aims every target slot at `oid` -- both of the card's pools (Pool.Spells and
-- Pool.Permanents) answer with ToObject. ModalSpec.chooseModeAt's shape,
-- duplicated per this suite's group-local-helper convention rather than
-- centralized. Deliberately does NOT answer ChooseColor: the Painter's Servant
-- cast that precedes it runs under choosingBlue, and the blast's own cast has no
-- colour to choose -- so a ChooseColor reaching here would be a real surprise
-- and falls to S.identityAnswer's white rather than being papered over.
blasting :: ModeIndex.ModeIndex -> ObjectId.ObjectId -> Prompt.Prompt r -> r
blasting idx oid p = case p of
  Prompt.ChooseModes {} -> Set.singleton idx
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  _ -> S.identityAnswer p

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

  Spec.it s "CR 613.3 within layer 5 the CR 702.114a devoid CDA applies first, then the timestamped colour change" $ do
    -- Both live in layer 5 (CR 613.1e). CR 613.3 orders them: devoid, the
    -- characteristic-defining ability, clears the colours at the start of the
    -- layer, and the stored SetColor then applies in timestamp order on top --
    -- so the drone ends up black, not colourless.
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

  Spec.it s "CR 105.3 an 'in addition' colour effect ADDS rather than replaces" $ do
    -- The falsifier for implementing every colour change as a replacement: the
    -- Rats are black, and after an AddColor they are black AND blue.
    typhoidRats <- S.printingOf s registry "Typhoid Rats"
    let gs0 = Setup.emptyGame S.bothPlayers
        (ratsId, board) = S.addCreature typhoidRats S.alice gs0
        gs = S.withEffect ratsId (Modification.AddColor (Set.singleton Color.Blue)) board
    Spec.assertEq s (Projection.colorsOf ratsId gs) $ Set.fromList [Color.Black, Color.Blue]

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

  Spec.it s "Indigo Faerie's 'in addition' blue makes a devoid drone a legal Red Elemental Blast target" $ do
    -- Slaughter Drone is devoid, so it is colourless until something adds a
    -- colour. Red Elemental Blast's destroy mode reads blue, and the drone is
    -- outside its set until Indigo Faerie's ability resolves. Both halves are
    -- driven through real cards -- a real activation and a real cast, not
    -- S.withEffect and not a hand-built TargetSpec -- so the two JSON files, not
    -- just the projection, are under test.
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    indigoFaerie <- S.printingOf s registry "Indigo Faerie"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    redElementalBlast <- S.printingOf s registry "Red Elemental Blast"
    let base = S.landsInPlay island 1
        -- The Island pays Indigo Faerie's {U}; the Mountain pays the blast's {R}.
        (_, withMountain) = S.addCreature mountain S.alice base
        (faerieId, withFaerie) = S.addCreature indigoFaerie S.alice withMountain
        (droneId, board) = S.addCreature slaughterDrone S.alice withFaerie
        (gs, rebId) = S.handOne redElementalBlast board
        activated = snd (Engine.runGamePure (aimAtObject droneId) gs (Activate.activateAbility S.alice faerieId (theAbility indigoFaerie)))
        blued = snd (Engine.runGamePure (aimAtObject droneId) activated Stack.resolveTop)
        answer :: Prompt.Prompt r -> r
        answer = blasting destroyMode droneId
        cast = snd (Engine.runGamePure answer blued (Cast.castSpell S.alice rebId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
    Spec.assertEqWith s "colourless before" (Projection.colorsOf droneId gs) Set.empty
    Spec.assertEqWith s "blue after" (Projection.colorsOf droneId blued) $ Set.singleton Color.Blue
    case modeSpec redElementalBlast destroyMode of
      Nothing -> Spec.assertFailure s "Red Elemental Blast's destroy mode declares exactly one target slot"
      Just destroySpec -> do
        Spec.assertBool
          s
          (not (Set.member (Recipient.ToObject droneId) (Target.legalRecipients Nothing S.noSource destroySpec gs)))
          "the colourless drone is not a legal 'target blue permanent'"
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject droneId) (Target.legalRecipients Nothing S.noSource destroySpec blued))
          "the blue drone is"
    Spec.assertBool s (not (Set.member droneId (GameState.battlefield after))) "and Red Elemental Blast destroys it"
    Spec.assertBool s (Set.member faerieId (GameState.battlefield after)) "the blast hit the drone, not the equally blue Faerie"

  Spec.it s "CR 613.3 devoid applies at the START of layer 5, so layers 2-4 read the mana cost" $ do
    -- CR 613.3 applies characteristic-defining abilities first WITHIN a layer,
    -- and devoid's layer is 5 (CR 613.1e). A layer-2, -3 or -4 effect whose
    -- affected set is colour-keyed therefore sees the mana cost's black, not
    -- the colourless devoid produces. Seeding devoid gets this wrong (#35).
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    let gs0 = Setup.emptyGame S.bothPlayers
        (droneId, gs) = S.addCreature slaughterDrone S.alice gs0
        cands = Projection.gather gs
        below = Projection.projectUpTo Layer.Color cands droneId gs
    Spec.assertEqWith s "black below layer 5" (PC.colors below) $ Set.singleton Color.Black
    Spec.assertEqWith s "colourless once layer 5 has applied" (Projection.colorsOf droneId gs) Set.empty

  Spec.it s "CR 613.3 devoid beats an OLDER layer-5 'in addition' effect" $ do
    -- THE GATE. Painter's Servant is cast and resolves FIRST, naming blue as it
    -- enters (CR 614.1c), so the continuous effect its static ability generates
    -- carries that permanent's timestamp (CR 613.7a). The drone's timestamp is
    -- minted later, when IT enters (CR 613.7d) -- asserted below, so the fixture
    -- cannot quietly stop discriminating. Pure CR 613.7 timestamp order would
    -- therefore add blue FIRST and apply devoid SECOND, leaving the drone
    -- COLOURLESS. CR 613.3 applies the characteristic-defining ability first
    -- within layer 5 (devoid is one, CR 702.114a; colour is layer 5, CR 613.1e),
    -- so devoid clears the colours and the older "in addition" blue lands on
    -- top: the drone is BLUE. Blue rather than black, so nothing here can be
    -- confused with the drone's printed {B}.
    --
    -- CAST rather than S.addCreature, because the colour choice happens only on
    -- the entry path (Replacement.runEntry): a Servant placed straight onto the
    -- battlefield has chosenColor = Nothing and its AddChosenColor adds nothing.
    mountain <- S.printingOf s registry "Mountain"
    paintersServant <- S.printingOf s registry "Painter's Servant"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    redElementalBlast <- S.printingOf s registry "Red Elemental Blast"
    -- Three Mountains: {2} for the Servant and {R} for the blast.
    let base = S.landsInPlay mountain 3
        (inHand, psId) = S.handOne paintersServant base
        cast = snd (Engine.runGamePure choosingBlue inHand (Cast.castSpell S.alice psId))
        withPainter = snd (Engine.runGamePure choosingBlue cast Stack.resolveTop)
        (droneId, withDrone) = S.addCreature slaughterDrone S.alice withPainter
        (rebId, gs) = S.addHandCard redElementalBlast S.alice withDrone
        entered = Set.difference (GameState.battlefield withPainter) (GameState.battlefield base)
        answer :: Prompt.Prompt r -> r
        answer = blasting destroyMode droneId
        blasted = snd (Engine.runGamePure answer gs (Cast.castSpell S.alice rebId))
        after = snd (Engine.runGamePure answer blasted Stack.resolveTop)
    case Set.toList entered of
      [painterId] -> case (Game.lookupObject painterId gs, Game.lookupObject droneId gs) of
        (Just painter, Just drone) -> do
          Spec.assertEqWith s "the Servant chose blue on entry and coloured itself" (Projection.colorsOf painterId withPainter) $ Set.singleton Color.Blue
          Spec.assertBool s (Object.timestamp painter < Object.timestamp drone) "the Servant's effect is OLDER than the drone"
          Spec.assertBool s (Set.member painterId (GameState.battlefield after)) "the blast hit the drone, not the equally blue Servant"
        _ -> Spec.assertFailure s "the fixture lost the Servant or the drone"
      _ -> Spec.assertFailure s "Painter's Servant did not reach the battlefield"
    Spec.assertEqWith s "blue, not colourless" (Projection.colorsOf droneId gs) $ Set.singleton Color.Blue
    -- THE READER, end to end: Red Elemental Blast's "destroy target blue
    -- permanent" mode names the drone only because CR 613.3 left it blue.
    Spec.assertBool s (not (Set.member droneId (GameState.battlefield after))) "Red Elemental Blast destroys the blue-ified drone"

  Spec.it s "CR 604.3 Red Elemental Blast counters a devoid SPELL that Painter's Servant has coloured" $ do
    -- Painter's set is "all cards that aren't on the battlefield, spells, and
    -- permanents", so it is not scoped to the battlefield
    -- (Affected.MatchingAnywhere). CR 604.3 makes a characteristic-defining
    -- ability function in all zones, so devoid still empties the drone SPELL's
    -- colours first and the Servant's blue lands on top -- and Red Elemental
    -- Blast's "counter target blue spell" mode can then name it.
    --
    -- This is also the one test that exercises the card's MODE ORDER: with the
    -- Servant and the Mountains all blue, both of the blast's modes are
    -- fillable, so CR 601.2b's ChooseModes is really asked and mode 0 is really
    -- chosen. Swap the two modes in the JSON and mode 0 becomes "destroy target
    -- blue permanent", which cannot name a spell on the stack -- the cast
    -- no-ops and the drone resolves.
    mountain <- S.printingOf s registry "Mountain"
    paintersServant <- S.printingOf s registry "Painter's Servant"
    slaughterDrone <- S.printingOf s registry "Slaughter Drone"
    redElementalBlast <- S.printingOf s registry "Red Elemental Blast"
    -- Three Mountains: {2} for the Servant and {R} for the blast.
    let base = S.landsInPlay mountain 3
        (inHand, psId) = S.handOne paintersServant base
        cast = snd (Engine.runGamePure choosingBlue inHand (Cast.castSpell S.alice psId))
        withPainter = snd (Engine.runGamePure choosingBlue cast Stack.resolveTop)
        (spellId, withSpell) = S.spellOnStack slaughterDrone S.alice withPainter
        (rebId, gs) = S.addHandCard redElementalBlast S.alice withSpell
        answer :: Prompt.Prompt r -> r
        answer = blasting counterMode spellId
        blasted = snd (Engine.runGamePure answer gs (Cast.castSpell S.alice rebId))
        after = snd (Engine.runGamePure answer blasted Stack.resolveTop)
    Spec.assertEqWith s "the spell is blue" (Projection.colorsOf spellId gs) $ Set.singleton Color.Blue
    case modeSpec redElementalBlast counterMode of
      Nothing -> Spec.assertFailure s "Red Elemental Blast's counter mode declares exactly one target slot"
      Just counterSpec ->
        Spec.assertBool
          s
          (Set.member (Recipient.ToObject spellId) (Target.legalRecipients Nothing S.noSource counterSpec gs))
          "and a legal 'target blue spell'"
    Spec.assertBool s (notElem spellId (GameState.stack after)) "the drone spell is off the stack"
    Spec.assertEqWith s "countered into its owner's graveyard, so it never entered the battlefield" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 2
