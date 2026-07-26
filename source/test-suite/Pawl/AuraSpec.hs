-- Covers Pawl.Stack's Aura branch and Pawl.Resolve.targetsAllIllegal: a
-- resolving Aura spell either fizzles (CR 608.2b) or enters the battlefield
-- already attached to its target (CR 303.4).
module Pawl.AuraSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 301.5 / 702.6: Equipment. Shares the attachment substrate with Auras --
-- Object.attachedTo, Affected.Attached -- so what is genuinely new is the CR
-- 701.3 Attach keyword action that MOVES an already-on-the-battlefield permanent
-- (#187), and CR 704.5n's detach-rather-than-bury state-based action (#193).
equipmentTests :: Registry.Type.Registry -> Tasty.TestTree
equipmentTests registry =
  Tasty.testGroup
    "Equipment"
    [ -- CR 702.6a: "Equip [cost]" means "[Cost]: Attach this permanent to target
      -- creature you control." The Equipment is the ability's SOURCE; the slot is
      -- what it attaches to.
      HU.testCase "CR 702.6a equipping attaches the Equipment to the target creature" $ do
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.alice base
            (equip, gs) = S.addCreature bonesplitter S.alice withCreature
            slot = SlotName.MkSlotName (Text.pack "target")
            run =
              Resolve.applyEffect
                equip
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature creature))
                (Effect.Attach slot)
            after = S.runPure S.identityAnswer gs run
        HU.assertEqual "the Equipment is attached to the creature" (Just (Just creature)) (fmap Object.attachedTo (Game.lookupObject equip after))
        HU.assertBool "and is still on the battlefield" (Set.member equip (GameState.battlefield after)),
      -- CR 301.5a: "The creature an Equipment is attached to is called the
      -- 'equipped creature'." Affected.Attached already means exactly that, so
      -- Bonesplitter's +2/+0 rides the same path Unholy Strength does.
      HU.testCase "CR 301.5a the equipped creature gets the Equipment's bonus" $ do
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.alice base
            (equip, gs) = S.addCreature bonesplitter S.alice withCreature
            attached = S.attach equip creature gs
        HU.assertEqual "unequipped, the Piker is 2/1" (Just 2) (Projection.powerOf creature gs)
        HU.assertEqual "equipped, it is 4/1" (Just 4) (Projection.powerOf creature attached)
        HU.assertEqual "toughness is untouched by +2/+0" (Just 1) (Projection.toughnessOf creature attached),
      -- CR 701.3a: attaching a permanent that is already attached MOVES it. This
      -- is the whole point of the Attach opcode -- Aura attachment happens once,
      -- as the Aura enters, and nothing could relocate it afterwards (#187).
      HU.testCase "CR 701.3a equipping again moves the Equipment off the first creature" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (first, g1) = S.addCreature piker S.alice base
            (second, g2) = S.addCreature warMammoth S.alice g1
            (equip, g3) = S.addCreature bonesplitter S.alice g2
            gs = S.attach equip first g3
            slot = SlotName.MkSlotName (Text.pack "target")
            run =
              Resolve.applyEffect
                equip
                S.alice
                Map.empty
                (Map.singleton slot True)
                (Map.singleton slot (Recipient.ToCreature second))
                (Effect.Attach slot)
            after = S.runPure S.identityAnswer gs run
        HU.assertEqual "it moved to the second creature" (Just (Just second)) (fmap Object.attachedTo (Game.lookupObject equip after))
        HU.assertEqual "the first creature is back to 2 power" (Just 2) (Projection.powerOf first after)
        HU.assertEqual "the second is 3+2" (Just 5) (Projection.powerOf second after),
      -- The gameplay-level proof design.md section 4 asks for: cast Bonesplitter,
      -- activate its printed equip ability through the real activation path, let
      -- it resolve, and see the creature actually hit harder. Everything above
      -- drives Effect.Attach directly; this drives the CARD.
      HU.testCase "CR 702.6 whole card: cast Bonesplitter, equip a Piker, and it swings for 4" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base0 = S.landsInPlay mountain 2 -- {1} to cast, {1} to equip
            (creature, base1) = S.addCreature piker S.alice base0
            (withSpell, spellId) = S.handOne bonesplitter base1
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            equipId = case filter (\oid -> Game.cardOf oid resolved == Just (Printing.card bonesplitter)) (Set.toList (GameState.battlefield resolved)) of
              oid : _ -> Just oid
              [] -> Nothing
        case equipId of
          Nothing -> HU.assertFailure "Bonesplitter should have resolved onto the battlefield"
          Just equip -> do
            let ability = case Card.Type.activatedAbilities (Printing.card bonesplitter) of
                  ab : _ -> Just ab
                  [] -> Nothing
            case ability of
              Nothing -> HU.assertFailure "Bonesplitter should print an equip ability"
              Just equipAbility -> do
                let ready = resolved {GameState.priority = Just S.alice}
                    activated = snd (Engine.runGamePure S.identityAnswer ready (Activate.activateAbility S.alice equip equipAbility))
                    after = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
                HU.assertEqual "unequipped the Piker is 2/1" (Just 2) (Projection.powerOf creature resolved)
                HU.assertEqual "the equip ability attached it" (Just (Just creature)) (fmap Object.attachedTo (Game.lookupObject equip after))
                HU.assertEqual "and the Piker is now 4/1" (Just 4) (Projection.powerOf creature after)
                HU.assertEqual "toughness unchanged" (Just 1) (Projection.toughnessOf creature after),
      -- CR 701.3c: "Attaching an Aura, Equipment, or Fortification on the
      -- battlefield to a different object or player causes [it] to receive a new
      -- timestamp." That feeds CR 613.7's layer ordering, so it is not cosmetic.
      -- CR 701.3b's second sentence is the other half: re-attaching to the object
      -- it is ALREADY attached to "does nothing", so no new timestamp there.
      HU.testCase "CR 701.3c attaching to a different creature restamps; re-attaching to the same one does not" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (first, g1) = S.addCreature piker S.alice base
            (second, g2) = S.addCreature warMammoth S.alice g1
            (equip, g3) = S.addCreature bonesplitter S.alice g2
            gs = S.attach equip first g3
            slot = SlotName.MkSlotName (Text.pack "target")
            attachTo t g =
              S.runPure S.identityAnswer g $
                Resolve.applyEffect
                  equip
                  S.alice
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToCreature t))
                  (Effect.Attach slot)
            stampOf g = fmap Object.timestamp (Game.lookupObject equip g)
            moved = attachTo second gs
            again = attachTo second moved
        HU.assertBool "moving it to a different creature restamps" (stampOf moved /= stampOf gs)
        HU.assertEqual "re-attaching to the same creature does nothing" (stampOf moved) (stampOf again),
      -- CR 704.5n: "If an Equipment or Fortification is attached to an illegal
      -- permanent or to a player, it becomes unattached from that permanent or
      -- player. It REMAINS ON THE BATTLEFIELD." The shape difference from an
      -- Aura, which CR 704.5m buries instead (#193).
      HU.testCase "CR 704.5n an Equipment whose creature dies detaches and stays on the battlefield" $ do
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.alice base
            (equip, g2) = S.addCreature bonesplitter S.alice withCreature
            attached = S.attach equip creature g2
            gone = S.runPure S.identityAnswer attached (Event.changeZone creature Zone.Graveyard)
            after = S.settleSba gone
        HU.assertBool "the Equipment survives" (Set.member equip (GameState.battlefield after))
        HU.assertEqual "and is unattached" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject equip after)),
      -- CR 301.5: "It can't legally be attached to anything that isn't a
      -- creature." An Equipment left on a noncreature permanent detaches too --
      -- the same SBA, a different way of becoming illegal.
      HU.testCase "CR 301.5 an Equipment attached to a noncreature permanent detaches" $ do
        mountain <- Registry.printing registry "Mountain"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        let base = Setup.emptyGame S.bothPlayers
            (land, withLand) = S.addCreature mountain S.alice base
            (equip, g2) = S.addCreature bonesplitter S.alice withLand
            attached = S.attach equip land g2
            after = S.settleSba attached
        HU.assertBool "the Equipment survives" (Set.member equip (GameState.battlefield after))
        HU.assertEqual "and is unattached" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject equip after))
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.Aura" [auraTests registry, equipmentTests registry]

auraTests :: Registry.Type.Registry -> Tasty.TestTree
auraTests registry =
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
      --
      -- The whole span, with nothing forced: the Piker settles under bob, the
      -- steal makes it sick again for alice, and only HER untap step settles it
      -- for her. The middle assertion is what #198 got wrong.
      HU.testCase "CR 302.6 (#62): a creature held under indefinite control settles at the thief's untap step" $ do
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            settledForBob = S.runPure S.identityAnswer withCreature (Engine.settleAll S.bob)
            (aura, withAura) = S.addCreature controlMagic S.alice settledForBob
            stolen = S.attach aura creature withAura
            settled = S.runPure S.identityAnswer stolen (Engine.settleAll S.alice)
        HU.assertEqual "alice controls it" (Just S.alice) (Projection.controllerOf creature stolen)
        HU.assertBool "the turn she steals it, it cannot attack" (not (Combat.canAttack S.alice creature stolen))
        HU.assertBool "and it has settled under her control, so it can attack" (Combat.canAttack S.alice creature settled)
    ]
