-- Covers Pawl.Stack's Aura branch and Pawl.Resolve.targetsAllIllegal: a
-- resolving Aura spell either fizzles (CR 608.2b) or enters the battlefield
-- already attached to its target (CR 303.4).
module Pawl.AuraSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Aura"
    [ HU.testCase "CR 303.4: a resolving Aura spell enters the battlefield attached to its target" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        let base = S.landsInPlay swamp 1
            (creature, withCreature) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne unholyStrength withCreature
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            auras = filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after))
        HU.assertEqual "one attached permanent on the battlefield" 1 (length auras)
        HU.assertEqual "attached to the creature" [Just creature] (fmap Object.attachedTo auras)
        HU.assertEqual "the creature is a 4/2" (Just (4, 2)) (S.powerToughnessOf creature after),
      -- CR 608.2b: an Aura spell is the first PERMANENT spell in this pool that
      -- can be countered on resolution. Before this task, Stack sent every
      -- permanent spell to the battlefield with no target check at all.
      HU.testCase "CR 608.2b: an Aura spell whose target left is countered on resolution" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        let base = S.landsInPlay swamp 1
            (creature, withCreature) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne unholyStrength withCreature
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            -- The target leaves in response, so no legal target remains at resolution.
            bounced = S.runPure S.identityAnswer cast (Event.changeZone creature Zone.Hand)
            after = snd (Engine.runGamePure S.identityAnswer bounced Stack.resolveTop)
        HU.assertEqual "nothing attached on the battlefield" [] (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after)))
        HU.assertEqual "the Aura is in its owner's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      -- CR 704.5m, and CR 704.3's repeat. SBAs are simultaneous, so the pass that
      -- buries the creature judged the Aura against a state in which that creature was
      -- still there; the Aura falls off on the NEXT pass. Asserting both passes is the
      -- point -- an implementation that dropped the Aura in pass one would be reading
      -- post-pass state, which is what CR 704.3's "simultaneously" forbids.
      HU.testCase "CR 704.5m: an Aura whose creature died falls off on the next SBA pass" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        let base = S.landsInPlay swamp 1
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature unholyStrength S.alice withCreature
            attached = S.attach aura creature withAura
            -- Goblin Piker is 2/1; Unholy Strength makes it 4/2, so 2 damage is not
            -- lethal and 3 is (CR 704.5g reads TOTAL marked damage against projected
            -- toughness).
            damaged = S.markDamage creature 3 attached
            pass1 = S.settleSba damaged
            pass2 = S.settleSba pass1
        HU.assertEqual "the creature is gone after pass one" Nothing (Game.lookupObject creature pass1)
        HU.assertBool "the Aura is still on the battlefield after pass one" (Set.member aura (GameState.battlefield pass1))
        HU.assertBool "the Aura is gone from the battlefield after pass two" (not (Set.member aura (GameState.battlefield pass2)))
        HU.assertEqual "and is in its OWNER's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice pass2)),
      -- CR 704.5m's remaining clause: unattached. The third clause -- attached to
      -- an object the enchant spec no longer admits -- is dormant: nothing in
      -- the pool strips creature-ness from a permanent, so it has no test here.
      HU.testCase "CR 704.5m: an unattached Aura on the battlefield goes to the graveyard" $ do
        unholyStrength <- Registry.printing registry "Unholy Strength"
        let base = Setup.emptyGame S.bothPlayers
            (aura, gs) = S.addCreature unholyStrength S.alice base
            after = S.settleSba gs
        HU.assertBool "never attached, so it falls off immediately" (not (Set.member aura (GameState.battlefield after))),
      -- CR 613.1b / 303.4e: Control Magic's static ability moves control of the
      -- enchanted creature to the AURA's controller, and leaves the Aura itself alone.
      HU.testCase "CR 613.1b: Control Magic gives the Aura's controller the creature" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature controlMagic S.alice withCreature
            attached = S.attach aura creature withAura
        HU.assertEqual "unattached, bob still controls it" (Just S.bob) (Projection.controllerOf creature withAura)
        HU.assertEqual "attached, alice controls it" (Just S.alice) (Projection.controllerOf creature attached)
        HU.assertEqual "the Aura's own controller is unchanged" (Just S.alice) (Projection.controllerOf aura attached)
        HU.assertBool "and it is in alice's controls" (elem creature (Projection.controls S.alice attached))
        HU.assertBool "no longer in bob's" (notElem creature (Projection.controls S.bob attached)),
      -- CR 704.5m plus layer 2: destroying the Aura reverts control on the next
      -- projection, because a static ability's effect exists only while its source is
      -- on the battlefield (CR 604.2).
      HU.testCase "CR 604.2: removing Control Magic reverts control" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature controlMagic S.alice withCreature
            attached = S.attach aura creature withAura
            gone = S.runPure S.identityAnswer attached (Event.changeZone aura Zone.Graveyard)
        HU.assertEqual "alice controlled it" (Just S.alice) (Projection.controllerOf creature attached)
        HU.assertEqual "bob controls it again" (Just S.bob) (Projection.controllerOf creature gone),
      -- The whole path: cast, target, enter attached, control moves.
      HU.testCase "CR 303.4: casting Control Magic takes the creature" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = S.landsInPlay island 4
            (creature, withCreature) = S.addCreature piker S.bob base
            (gs, spellId) = S.handOne controlMagic withCreature
            cast = snd (Engine.runGamePure S.identityAnswer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        HU.assertEqual "alice controls bob's creature" (Just S.alice) (Projection.controllerOf creature after),
      -- CR 302.6 across turns (#62): control from an Aura is INDEFINITE, so alice
      -- still holds the creature when her own untap step arrives. Engine.settleAll
      -- iterates Projection.controls, so it settles for the controller, and the
      -- creature can attack. Act of Treason could never test this -- its control ends
      -- at cleanup (CR 514.2), long before the thief's untap step.
      HU.testCase "CR 302.6 (#62): a creature held under indefinite control settles at the thief's untap step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature controlMagic S.alice withCreature
            attached = S.attach aura creature withAura
            sick = S.resick creature attached
            settled = S.runPure S.identityAnswer sick (Engine.settleAll S.alice)
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf creature settled)
        HU.assertBool "and it has settled under her control, so it can attack" (Combat.canAttack S.alice creature settled)
    ]
