-- Pattern matching on Pawl.Types.Prompt, a GADT, in aimAt below.
{-# LANGUAGE GADTs #-}

-- Covers Pawl.Stack's Aura branch and Pawl.Resolve.targetsAllIllegal -- a
-- resolving Aura spell either fizzles (CR 608.2b) or enters the battlefield
-- already attached to its target (CR 303.4) -- together with the rest of the
-- attachment substrate that shares Object.attachedTo: Pawl.Resolve's Attach
-- opcode (CR 701.3) and Pawl.Sba's three attachment state-based actions
-- (CR 704.5m, 704.5n, 704.5p).
module Pawl.AuraSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Activate as Activate
import qualified Pawl.Cast as Cast
import qualified Pawl.Combat as Combat
import qualified Pawl.Departure as Departure
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Registry as Registry.Type
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 301.5 / 702.6: Equipment. Shares the attachment substrate with Auras --
-- Object.attachedTo, Affected.Attached -- so what is genuinely new is the CR
-- 701.3 Attach keyword action that MOVES an already-on-the-battlefield permanent,
-- and CR 704.5n's detach-rather-than-bury state-based action (#193). The
-- Reattach group below is the same keyword action aimed the other way, at a
-- permanent the effect TARGETS rather than at its own source.
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
        HU.assertEqual "the Equipment is attached to the creature" (Just (Just (Recipient.ToCreature creature))) (fmap Object.attachedTo (Game.lookupObject equip after))
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
      -- CR 701.3a: attaching a permanent that is already attached MOVES it --
      -- "take it from where it currently is and put it onto that object".
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
        HU.assertEqual "it moved to the second creature" (Just (Just (Recipient.ToCreature second))) (fmap Object.attachedTo (Game.lookupObject equip after))
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
                HU.assertEqual "the equip ability attached it" (Just (Just (Recipient.ToCreature creature))) (fmap Object.attachedTo (Game.lookupObject equip after))
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

-- An answerer that aims every target slot at one object, deferring everything
-- else to S.identityAnswer (ModalSpec.chooseModeAt's shape). The CR 303.4d case
-- below needs it because both of its target choices are real ones -- alice
-- controls other permanents that each spec admits -- so they cannot be forced by
-- board construction the way the sibling cases above force theirs.
aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject oid)) sets
  _ -> S.identityAnswer p

-- CR 704.5p, the sibling of CR 704.5n above: 704.5n asks whether the HOST is
-- still legal, and this asks whether the attached permanent may be attached to
-- anything at all. Both detach and leave the permanent on the battlefield.
unattachableTests :: Registry.Type.Registry -> Tasty.TestTree
unattachableTests registry =
  Tasty.testGroup
    "Unattachable"
    [ -- CR 704.5p, first sentence: "If a battle or creature is attached to an
      -- object or player, it becomes unattached and remains on the battlefield."
      -- CR 301.5c says the same from the card type's side -- "An Equipment that's
      -- also a creature can't equip a creature unless that Equipment has
      -- reconfigure" -- and so does Skilled Animator's own ruling: "If an
      -- Equipment becomes an artifact creature, it usually can't be attached to
      -- another creature. If it was attached to a creature, it becomes
      -- unattached." (Bonesplitter has no reconfigure; nothing in the pool does.)
      --
      -- Note which SBA does NOT fire here: CR 704.5n needs an ILLEGAL host, and
      -- the Piker is a perfectly legal one. It is the Equipment itself that has
      -- stopped being attachable.
      --
      -- Whole card, through the real pipeline: cast Skilled Animator, let its
      -- CR 603.6a enters-the-battlefield trigger target the equipped
      -- Bonesplitter, and watch the equipped creature lose the bonus.
      HU.testCase "CR 704.5p whole card: animating an equipped Bonesplitter detaches it and the Piker loses the bonus" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        bonesplitter <- Registry.printing registry "Bonesplitter"
        animator <- Registry.printing registry "Skilled Animator"
        let base = S.landsInPlay island 3 -- {2}{U}
            (creature, g1) = S.addCreature piker S.alice base
            (equip, g2) = S.addCreature bonesplitter S.alice g1
            attached = S.attach equip creature g2
            (withSpell, spellId) = S.handOne animator attached
            cast = snd (Engine.runGamePure S.identityAnswer withSpell (Cast.castSpell S.alice spellId))
            resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- The ETB trigger goes on the stack here, with its target chosen:
            -- Bonesplitter is the only artifact alice controls, so CR 603.3d's
            -- choice is forced.
            triggered = snd (Engine.runGamePure S.identityAnswer resolved Engine.settleForPriority)
            animated = snd (Engine.runGamePure S.identityAnswer triggered Stack.resolveTop)
            after = snd (Engine.runGamePure S.identityAnswer animated Engine.settleForPriority)
        HU.assertEqual "equipped, the Piker was 4/1" (Just 4) (Projection.powerOf creature attached)
        HU.assertBool "the enters-the-battlefield trigger really was on the stack" (not (null (GameState.stack triggered)))
        HU.assertEqual "the Equipment is now a 5/5 creature" (Just 5) (Projection.powerOf equip after)
        HU.assertBool "it stays on the battlefield" (Set.member equip (GameState.battlefield after))
        HU.assertEqual "and is unattached" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject equip after))
        HU.assertEqual "so the Piker is back to 2 power" (Just 2) (Projection.powerOf creature after),
      -- CR 704.5p, second sentence: "if any nonbattle, noncreature permanent
      -- that's neither an Aura, an Equipment, nor a Fortification is attached to
      -- an object or player, it becomes unattached and remains on the
      -- battlefield." No card can produce this state -- nothing in the pool
      -- strips the Equipment subtype, and nothing attaches a land -- so the
      -- attachment is hand-built, the way the CR 301.5 case in the Equipment
      -- group above hand-builds an Equipment on a land. What it pins is that the
      -- branch reads the permanent's own types and not its host's: the Piker here
      -- is a legal host for anything that may be attached at all.
      HU.testCase "CR 704.5p a land attached to a creature detaches and stays on the battlefield" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.alice base
            (land, g2) = S.addCreature mountain S.alice withCreature
            attached = S.attach land creature g2
            after = S.settleSba attached
        HU.assertBool "the land survives" (Set.member land (GameState.battlefield after))
        HU.assertEqual "and is unattached" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject land after)),
      -- CR 303.4d's second clause: "An Aura that's also a creature can't enchant
      -- anything. If this occurs somehow, the Aura becomes unattached, then is
      -- put into its owner's graveyard. (These are state-based actions.)"
      --
      -- Two state-based actions, in that order, and this drives them one pass at
      -- a time to prove the order rather than only the end state: CR 704.5p
      -- unattaches the Aura (it is a creature that is attached), and only then
      -- does CR 704.5m see an Aura attached to nothing and bury it. Neither rule
      -- names CR 303.4d; between them they are what enforces it.
      --
      -- Reaching the clause at all takes two cards, because every printed
      -- enchantment animator carefully excludes Auras -- Opalescence and
      -- Starfield of Nyx both say "non-Aura enchantment", and so does Zur,
      -- Eternal Schemer. So the Aura is made
      -- an ARTIFACT first (Liquimetal Coating), which nothing excludes it from,
      -- and Skilled Animator then animates it as one.
      HU.testCase "CR 303.4d whole cards: an Aura made a creature unattaches, then is buried" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        coating <- Registry.printing registry "Liquimetal Coating"
        animator <- Registry.printing registry "Skilled Animator"
        let base = S.landsInPlay island 3 -- {2}{U} for the Animator
            (creature, g1) = S.addCreature piker S.alice base
            (aura, g2) = S.addCreature unholyStrength S.alice g1
            attached = S.attach aura creature g2
            (coatingId, g3) = S.addCreature coating S.alice attached
            ability = case Card.Type.activatedAbilities (Printing.card coating) of
              ab : _ -> Just ab
              [] -> Nothing
        case ability of
          Nothing -> HU.assertFailure "Liquimetal Coating should print one activated ability"
          Just coat -> do
            let ready = g3 {GameState.priority = Just S.alice}
                activated = snd (Engine.runGamePure (aimAt aura) ready (Activate.activateAbility S.alice coatingId coat))
                coated = snd (Engine.runGamePure (aimAt aura) activated Stack.resolveTop)
                (withSpell, spellId) = S.handOne animator coated
                cast = snd (Engine.runGamePure (aimAt aura) withSpell (Cast.castSpell S.alice spellId))
                entered = snd (Engine.runGamePure (aimAt aura) cast Stack.resolveTop)
                triggered = snd (Engine.runGamePure (aimAt aura) entered Engine.settleForPriority)
                animated = snd (Engine.runGamePure (aimAt aura) triggered Stack.resolveTop)
                -- S.settleSba is ONE pass (the CR 704.3 repeat lives in
                -- Engine.settleForPriority), which is what makes the two steps
                -- separately observable.
                unattachedNow = S.settleSba animated
                buried = S.settleSba unattachedNow
            HU.assertBool "the Aura is an artifact now" (Set.member CardType.Artifact (Projection.cardTypesOf aura coated))
            HU.assertEqual "and once animated it is a 5/5 creature" (Just 5) (Projection.powerOf aura animated)
            HU.assertEqual "still enchanting the Piker at that moment" (Just (Just (Recipient.ToCreature creature))) (fmap Object.attachedTo (Game.lookupObject aura animated))
            HU.assertEqual "which is still 2/1 + 2/+1" (Just 4) (Projection.powerOf creature animated)
            -- Step one: unattached, and still on the battlefield.
            HU.assertEqual "one SBA pass unattaches it" (Just Nothing) (fmap Object.attachedTo (Game.lookupObject aura unattachedNow))
            HU.assertBool "and it has not been buried yet" (Set.member aura (GameState.battlefield unattachedNow))
            -- Step two: CR 704.5m buries the now-unattached Aura.
            HU.assertBool "the next pass buries it" (not (Set.member aura (GameState.battlefield buried)))
            -- CR 400.7: it is a new object there, so this counts the zone rather
            -- than looking the battlefield id up again.
            HU.assertEqual "in its OWNER's graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice buried))
            HU.assertEqual "so the Piker loses the +2/+1" (Just 2) (Projection.powerOf creature buried)
    ]

-- CR 702.5d: "Auras that can enchant a player can target and be attached to
-- players. Such Auras can't target permanents and can't be attached to
-- permanents." Curse of Death's Hold is the proving card -- "Enchant player.
-- Creatures enchanted player controls get -1/-1" -- and it is the one that needs
-- BOTH halves of an enchant-player Aura: the Pool.Players enchant spec, which
-- Card.enchant could already express, and a static ability whose affected set is
-- reached THROUGH the enchanted player (Affected.AttachedPlayerControls).
enchantPlayerTests :: Registry.Type.Registry -> Tasty.TestTree
enchantPlayerTests registry =
  Tasty.testGroup
    "EnchantPlayer"
    [ -- The gameplay proof design.md section 4 asks for: cast the real card at a
      -- real player, let it resolve, and see the creatures on the other side of
      -- the table get smaller. CR 303.4: an Aura "enters the battlefield attached
      -- to an object or player", so the attachment is asserted on the incarnation
      -- that entered, not written by a later step.
      HU.testCase "CR 702.5d whole card: Curse of Death's Hold enters attached to the player it targeted and shrinks that player's creatures" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        curse <- Registry.printing registry "Curse of Death's Hold"
        let base = S.landsInPlay swamp 5
            (his, withHis) = S.addCreature piker S.bob base
            (hers, withBoth) = S.addCreature piker S.alice withHis
            (gs, spellId) = S.handOne curse withBoth
            answer = aimRecipient (Recipient.ToPlayer S.bob)
            cast = snd (Engine.runGamePure answer gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure answer cast Stack.resolveTop)
            settled = S.settleSba after
        HU.assertEqual "one attached permanent, and it is attached to bob himself" [Just (Recipient.ToPlayer S.bob)] (attachments after)
        HU.assertEqual "bob's Goblin Piker is a 1/0" (Just (1, 0)) (S.powerToughnessOf his after)
        HU.assertEqual "alice's is untouched -- she is not the enchanted player" (Just (2, 1)) (S.powerToughnessOf hers after)
        -- CR 704.5f: the shrunk creature has toughness 0, so the pass that judges
        -- the Curse legal buries the Piker.
        HU.assertEqual "so his Piker dies on the next SBA pass" Nothing (Game.lookupObject his settled)
        HU.assertEqual "and the Curse is still attached to him -- he is still in the game" [Just (Recipient.ToPlayer S.bob)] (attachments settled),
      -- The affected set is DYNAMIC in its controller half, which is what makes it
      -- a set rather than a list of ids: CR 613.1b applies control changes in
      -- layer 2, before the layer 7c this ability lands in, so a creature the
      -- enchanted player no longer controls is out of the set on the very next
      -- projection. Control Magic is the only control-changer in the pool.
      HU.testCase "CR 613.1b: a creature stolen from the enchanted player leaves the Curse's affected set" $ do
        piker <- Registry.printing registry "Goblin Piker"
        curse <- Registry.printing registry "Curse of Death's Hold"
        controlMagic <- Registry.printing registry "Control Magic"
        let base = Setup.emptyGame S.bothPlayers
            (creature, withCreature) = S.addCreature piker S.bob base
            (aura, withAura) = S.addCreature curse S.alice withCreature
            cursed = S.attachTo aura (Recipient.ToPlayer S.bob) withAura
            (steal, withSteal) = S.addCreature controlMagic S.alice cursed
            stolen = S.attach steal creature withSteal
        HU.assertEqual "bob controls it and it is shrunk" (Just (1, 0)) (S.powerToughnessOf creature cursed)
        HU.assertEqual "alice controls it now, so the Curse does not reach it" (Just (2, 1)) (S.powerToughnessOf creature stolen),
      -- CR 704.5m's remaining clause, and the one only an enchant-player Aura can
      -- reach: CR 303.4c spells it out as "the player it was attached to has left
      -- the game". Three seats, because CR 104.2a ends a two-player game the
      -- moment anyone leaves and no state-based action would ever be checked
      -- again.
      HU.testCase "CR 704.5m / 303.4c: a Curse attached to a player who has left the game is put into its owner's graveyard" $ do
        curse <- Registry.printing registry "Curse of Death's Hold"
        let (aura, withAura) = S.addCreature curse S.alice S.threePlayerGame
            attached = S.attachTo aura (Recipient.ToPlayer S.carol) withAura
            before = S.settleSba attached
            departed = Departure.depart Departure.Type.Conceded S.carol before
            after = S.settleSba departed
        HU.assertBool "while carol is in the game the Curse is legally attached" (Set.member aura (GameState.battlefield before))
        HU.assertBool "she leaves, and it is off the battlefield after one pass" (not (Set.member aura (GameState.battlefield after)))
        HU.assertEqual "in its OWNER's graveyard -- a put-into-graveyard, not a destruction" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after)),
      -- CR 702.5d's second sentence -- such Auras "can't target permanents and
      -- can't be attached to permanents" -- at the reattach door, and it needs no
      -- clause of its own: Crown of the Ages moves "target Aura attached to a
      -- creature", and CR 701.3a's IsAttachedToCreature reads the attachment for
      -- the OBJECT it names, which a player attachment does not name at all. So
      -- the Curse is not a legal target and there is nothing to refuse later.
      HU.testCase "CR 702.5d: a Curse attached to a player is not an Aura attached to a creature" $ do
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        curse <- Registry.printing registry "Curse of Death's Hold"
        crown <- Registry.printing registry "Crown of the Ages"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.alice base
            (onCreature, g2) = S.addCreature unholyStrength S.alice g1
            (onPlayer, g3) = S.addCreature curse S.alice g2
            (crownId, g4) = S.addCreature crown S.alice g3
            gs = S.attachTo onPlayer (Recipient.ToPlayer S.bob) (S.attach onCreature creature g4)
        case crownTargetSpec crown of
          Nothing -> HU.assertFailure "the fixture wanted Crown of the Ages' one printed target slot"
          Just spec ->
            HU.assertEqual
              "only the Aura on a creature is offered"
              (Set.singleton (Recipient.ToObject onCreature))
              (Target.legalRecipients (Just S.alice) crownId spec gs)
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.Aura" [auraTests registry, equipmentTests registry, unattachableTests registry, reattachTests registry, auraGraftTests registry, enchantPlayerTests registry]

-- Answers every target slot with one fixed recipient, deferring everything else
-- to S.identityAnswer. aimAt above does the same for a Pool.Permanents slot
-- only, because it hard-codes Recipient.ToObject; these cases mix a
-- Pool.Creatures slot (Unholy Strength's enchant) with a Pool.Permanents one
-- (Crown of the Ages' "target Aura"), and a recipient tagged for the wrong pool
-- is not in the legal set at all.
aimRecipient :: Recipient.Recipient -> Prompt.Prompt r -> r
aimRecipient recipient p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const recipient) sets
  _ -> S.identityAnswer p

-- Answers Prompt.ChooseAttachment with one fixed object -- the destination an
-- attach-moving effect picks on resolution (CR 701.3a). Discriminating where
-- S.identityAnswer's head-of-candidates would not be: these boards deliberately
-- offer several.
destination :: ObjectId.ObjectId -> Prompt.Prompt r -> r
destination oid p = case p of
  Prompt.ChooseAttachment {} -> oid
  _ -> S.identityAnswer p

-- Both of Crown of the Ages' prompts at once: its "target Aura" slot with a fixed
-- Aura, and its CR 701.3a destination choice with a fixed creature. aimRecipient
-- and destination each answer one of the two, and driving the printed activated
-- ability needs both.
moveAura :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
moveAura aura dest p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject aura)) sets
  Prompt.ChooseAttachment {} -> dest
  _ -> S.identityAnswer p

-- What every attached battlefield permanent is attached to. The whole-board read
-- an Aura test wants when the id it would look up is not the one it holds: CR
-- 400.7 mints a fresh id for the battlefield incarnation of a resolved Aura
-- spell.
attachments :: GameState.GameState -> [Maybe Recipient.Recipient]
attachments gs =
  fmap
    Object.attachedTo
    (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects gs)))

-- The battlefield permanents attached to `host`. How a test finds the Aura a
-- resolved Aura SPELL entered as: CR 400.7 mints a fresh id for the battlefield
-- incarnation, so the spell's own id names nothing afterwards.
attachedTo :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
attachedTo host gs =
  filter
    (\oid -> fmap Object.attachedTo (Game.lookupObject oid gs) == Just (Just (Recipient.ToCreature host)))
    (Set.toList (GameState.battlefield gs))

-- Crown of the Ages' one target slot, read off its printed activated ability --
-- the committed spec, not a restatement of it, so a test asserting what it admits
-- is asserting what the card really says.
crownTargetSpec :: Printing.Printing -> Maybe TargetSpec.TargetSpec
crownTargetSpec printing = case Card.Type.activatedAbilities (Printing.card printing) of
  ability : _ -> Map.lookup (SlotName.MkSlotName (Text.pack "target")) (Modal.allTargetSpecs (ActivatedAbility.modal ability))
  [] -> Nothing

-- CR 701.3 Attach, aimed at the effect's TARGET rather than at its source: an
-- opcode that moves an Aura already on the battlefield, which the Auras unit left
-- unbuilt. Crown of the Ages is the proving card -- "{4}, {T}: Attach target Aura
-- attached to a creature to another creature".
reattachTests :: Registry.Type.Registry -> Tasty.TestTree
reattachTests registry =
  Tasty.testGroup
    "Reattach"
    [ -- The gameplay-level proof design.md section 4 asks for: cast the Aura, cast
      -- the Crown, activate its printed ability through the real activation path,
      -- let it resolve, and see the bonus MOVE -- which is what "the Aura moved"
      -- means observably, not a field changing.
      HU.testCase "CR 701.3 whole card: Crown of the Ages moves Unholy Strength from the Piker to the Mammoth" $ do
        swamp <- Registry.printing registry "Swamp"
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        crown <- Registry.printing registry "Crown of the Ages"
        -- {B} for the Aura, {2} to cast the Crown, {4} to activate it.
        let base0 = S.landsInPlay swamp 7
            (first, base1) = S.addCreature piker S.alice base0
            -- A THIRD creature, and deliberately the one with the lower object id
            -- of the two destinations: "another creature" offers both, so the
            -- Prompt.ChooseAttachment answer below is a real choice rather than a
            -- forced single candidate, and answering with the Mammoth is not the
            -- head of the candidate list.
            (decoy, base1b) = S.addCreature piker S.alice base1
            (second, base2) = S.addCreature warMammoth S.alice base1b
            (withAura, auraSpell) = S.handOne unholyStrength base2
            castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature first)) withAura (Cast.castSpell S.alice auraSpell))
            enchanted = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature first)) castAura Stack.resolveTop)
        case attachedTo first enchanted of
          [] -> HU.assertFailure "Unholy Strength should have entered attached to the Piker"
          aura : _ -> do
            let (withCrown, crownSpell) = S.handOne crown enchanted
                castCrown = snd (Engine.runGamePure S.identityAnswer withCrown (Cast.castSpell S.alice crownSpell))
                onBattlefield = snd (Engine.runGamePure S.identityAnswer castCrown Stack.resolveTop)
                crownId = case filter (\oid -> Game.cardOf oid onBattlefield == Just (Printing.card crown)) (Set.toList (GameState.battlefield onBattlefield)) of
                  oid : _ -> Just oid
                  [] -> Nothing
            case crownId of
              Nothing -> HU.assertFailure "Crown of the Ages should have resolved onto the battlefield"
              Just crownObj -> do
                let ability = case Card.Type.activatedAbilities (Printing.card crown) of
                      ab : _ -> Just ab
                      [] -> Nothing
                case ability of
                  Nothing -> HU.assertFailure "Crown of the Ages should print one activated ability"
                  Just move -> do
                    let ready = onBattlefield {GameState.priority = Just S.alice}
                        activated = snd (Engine.runGamePure (moveAura aura second) ready (Activate.activateAbility S.alice crownObj move))
                        after = snd (Engine.runGamePure (moveAura aura second) activated Stack.resolveTop)
                        settled = S.settleSba (S.settleSba after)
                    HU.assertEqual "before, the Piker is 2/1 + 2/+1" (Just (4, 2)) (S.powerToughnessOf first onBattlefield)
                    HU.assertEqual "and the Mammoth is a plain 3/3" (Just (3, 3)) (S.powerToughnessOf second onBattlefield)
                    HU.assertEqual "the Aura is attached to the Mammoth now" (Just (Just (Recipient.ToCreature second))) (fmap Object.attachedTo (Game.lookupObject aura after))
                    HU.assertEqual "so the Piker is back to 2/1" (Just (2, 1)) (S.powerToughnessOf first after)
                    HU.assertEqual "and the Mammoth is 5/4" (Just (5, 4)) (S.powerToughnessOf second after)
                    HU.assertEqual "the creature nobody chose is untouched" (Just (2, 1)) (S.powerToughnessOf decoy after)
                    -- CR 704.5m: the Aura landed on a legal host, so nothing buries it.
                    HU.assertBool "the Aura survives the state-based actions" (Set.member aura (GameState.battlefield settled))
                    HU.assertEqual "still on the Mammoth" (Just (Just (Recipient.ToCreature second))) (fmap Object.attachedTo (Game.lookupObject aura settled)),
      -- CR 303.4b through the target slot: "target Aura ATTACHED TO A CREATURE"
      -- is Pool.Permanents narrowed by HasSubtype Aura and IsAttachedToCreature,
      -- so the narrowing has to do real work. The same Aura is offered when it
      -- sits on the Piker and withheld when it sits on a Mountain -- nothing
      -- about the Aura itself differs between the two boards, which is what makes
      -- the pair discriminating.
      --
      -- An Aura on a noncreature permanent is hand-built here, as the CR 704.5p
      -- land case above hand-builds its attachment: CR 704.5m would bury such an
      -- Aura on the next state-based-action pass, so no sequence of card plays
      -- leaves one standing for a player to look at.
      HU.testCase "CR 303.4b Crown of the Ages offers an Aura on a creature and not one on a land" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        crown <- Registry.printing registry "Crown of the Ages"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.alice base
            (land, g2) = S.addCreature mountain S.alice g1
            (aura, g3) = S.addCreature unholyStrength S.alice g2
            (crownObj, g4) = S.addCreature crown S.alice g3
            onCreature = S.attach aura creature g4
            onLand = S.attach aura land g4
            offered gs = fmap (\spec -> Target.legalRecipients (Just S.alice) crownObj spec gs) (crownTargetSpec crown)
            admits oid gs = fmap (Set.member (Recipient.ToObject oid)) (offered gs)
        HU.assertEqual "on the Piker the Aura is a legal target" (Just True) (admits aura onCreature)
        HU.assertEqual "on the Mountain it is not" (Just False) (admits aura onLand)
        -- Not vacuous for a second reason: the slot rejects the Crown itself on
        -- the very board where it accepts the Aura.
        HU.assertEqual "and the Crown is never a candidate" (Just False) (admits crownObj onCreature),
      -- CR 701.3c: "Attaching an Aura, Equipment, or Fortification on the
      -- battlefield to a different object or player causes [it] to receive a new
      -- timestamp." Feeds CR 613.7's layer ordering, so it is not cosmetic.
      HU.testCase "CR 701.3c moving an Aura to a different creature restamps it" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        crown <- Registry.printing registry "Crown of the Ages"
        let base = Setup.emptyGame S.bothPlayers
            (first, g1) = S.addCreature piker S.alice base
            (second, g2) = S.addCreature warMammoth S.alice g1
            (aura, g3) = S.addCreature unholyStrength S.alice g2
            (crownObj, g4) = S.addCreature crown S.alice g3
            gs = S.attach aura first g4
            slot = SlotName.MkSlotName (Text.pack "target")
            after =
              S.runPure (destination second) gs $
                Resolve.applyEffect
                  crownObj
                  S.alice
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToObject aura))
                  (Effect.AttachTarget slot (Filter.Type.HasCardType CardType.Creature))
            stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
        HU.assertEqual "it moved" (Just (Just (Recipient.ToCreature second))) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertBool "and was restamped" (stampOf after /= stampOf gs),
      -- CR 701.3b, second sentence: "If an effect tries to attach an Aura,
      -- Equipment, or Fortification to the object or player it's already attached
      -- to, the effect does nothing." Crown of the Ages spells the same exclusion
      -- as "ANOTHER creature", so with no other creature on the battlefield there
      -- is nothing to choose and nothing happens -- in particular no restamp.
      HU.testCase "CR 701.3b with only its own host available the Aura does not move and is not restamped" $ do
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        crown <- Registry.printing registry "Crown of the Ages"
        let base = Setup.emptyGame S.bothPlayers
            (first, g1) = S.addCreature piker S.alice base
            (aura, g2) = S.addCreature unholyStrength S.alice g1
            (crownObj, g3) = S.addCreature crown S.alice g2
            gs = S.attach aura first g3
            slot = SlotName.MkSlotName (Text.pack "target")
            after =
              S.runPure S.identityAnswer gs $
                Resolve.applyEffect
                  crownObj
                  S.alice
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToObject aura))
                  (Effect.AttachTarget slot (Filter.Type.HasCardType CardType.Creature))
            stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
        HU.assertEqual "still on the Piker" (Just (Just (Recipient.ToCreature first))) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "and not restamped" (stampOf gs) (stampOf after),
      -- CR 303.4j: "If an effect attempts to attach an Aura on the battlefield to
      -- an object or player it can't legally enchant, the Aura doesn't move." A
      -- FAILURE MODE, not a fizzle: the ability resolved, and the only thing that
      -- did not happen is the move.
      --
      -- The case where the destination is not a CREATURE at all, which no card in
      -- the pool reaches. Crown of the Ages' destination filter is HasCardType
      -- Creature, so every destination it can offer is at least a creature; Aura
      -- Graft's is Filter.CanHostSubject, so every destination IT can offer is one
      -- the Aura may legally enchant, and this rule's refusal never fires for it.
      -- The opcode is driven directly with a wider, hand-made filter instead -- the
      -- same way the Effect.Attach cases in the Equipment group above drive that
      -- opcode -- and that synthetic filter is the labeled crutch (#431). The
      -- rule's other case, a destination the Aura's own enchant restriction
      -- rejects, is the whole-cards test right below.
      HU.testCase "CR 303.4j attaching an Aura to something it cannot enchant leaves it where it was" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        crown <- Registry.printing registry "Crown of the Ages"
        let base = Setup.emptyGame S.bothPlayers
            (first, g1) = S.addCreature piker S.alice base
            (land, g2) = S.addCreature mountain S.alice g1
            (aura, g3) = S.addCreature unholyStrength S.alice g2
            (crownObj, g4) = S.addCreature crown S.alice g3
            gs = S.attach aura first g4
            slot = SlotName.MkSlotName (Text.pack "target")
            after =
              S.runPure (destination land) gs $
                Resolve.applyEffect
                  crownObj
                  S.alice
                  Map.empty
                  (Map.singleton slot True)
                  (Map.singleton slot (Recipient.ToObject aura))
                  -- `And []` matches everything, so the land is offered as a
                  -- destination and CR 303.4j is what rejects it.
                  (Effect.AttachTarget slot (Filter.Type.And []))
            stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
            settled = S.settleSba (S.settleSba after)
        HU.assertEqual "the Aura did not move onto the land" (Just (Just (Recipient.ToCreature first))) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "and was not restamped" (stampOf gs) (stampOf after)
        HU.assertEqual "the Piker still has the bonus" (Just (4, 2)) (S.powerToughnessOf first after)
        -- CR 704.5m: a failed move must not leave the Aura in a state the
        -- state-based actions then punish -- it is still on a legal host.
        HU.assertBool "and the Aura is not buried afterwards" (Set.member aura (GameState.battlefield settled)),
      -- CR 303.4j through two printed cards. Setessan Training's "Enchant creature
      -- you control" is the first Card.enchant in the pool that narrows past
      -- "creature" (CR 702.5a: the enchant ability "restricts what an Aura spell can
      -- target and what an Aura can enchant"), and Crown of the Ages' destination
      -- filter is the bare "another creature" -- so the Crown really does offer a
      -- destination the Aura may not legally enchant, which is the situation CR
      -- 303.4j is about and which no pair of cards could produce before.
      --
      -- CR 109.5 fixes whose "you" that is: the AURA's controller, not the moving
      -- effect's. Pawl.Resolve.attachmentFor asks Target.legalRecipients with
      -- Projection.controllerOf on the Aura for exactly that reason. Alice controls
      -- both cards here, so this board cannot tell the two readings apart -- nothing
      -- in the pool takes control of a noncreature artifact -- but attachmentFor is
      -- never handed the moving effect's source at all, so there is no second
      -- controller for it to read by mistake.
      --
      -- BOTH branches off one board and one activation, so the refusal cannot be
      -- the machinery declining to move anything: aimed at alice's own Mammoth the
      -- very same ability moves the Aura.
      HU.testCase "CR 303.4j whole cards: Crown of the Ages cannot move Setessan Training onto an opponent's creature" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        setessanTraining <- Registry.printing registry "Setessan Training"
        crown <- Registry.printing registry "Crown of the Ages"
        -- {1}{G} for the Aura, {2} to cast the Crown, {4} to activate it.
        let base0 = S.landsInPlay forest 8
            (host, base1) = S.addCreature piker S.alice base0
            (mine, base2) = S.addCreature warMammoth S.alice base1
            (theirs, base3) = S.addCreature piker S.bob base2
            (withAura, auraSpell) = S.handOne setessanTraining base3
            castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature host)) withAura (Cast.castSpell S.alice auraSpell))
            enchanted = snd (Engine.runGamePure S.identityAnswer castAura Stack.resolveTop)
            (withCrown, crownSpell) = S.handOne crown enchanted
            castCrown = snd (Engine.runGamePure S.identityAnswer withCrown (Cast.castSpell S.alice crownSpell))
            settledIn = snd (Engine.runGamePure S.identityAnswer castCrown Stack.resolveTop)
            crowns = filter (\oid -> Game.cardOf oid settledIn == Just (Printing.card crown)) (Set.toList (GameState.battlefield settledIn))
        case (attachedTo host enchanted, crowns, Card.Type.activatedAbilities (Printing.card crown)) of
          ([aura], [crownObj], [move]) -> do
            let ready = settledIn {GameState.priority = Just S.alice}
                run dest =
                  let activated = snd (Engine.runGamePure (moveAura aura dest) ready (Activate.activateAbility S.alice crownObj move))
                   in snd (Engine.runGamePure (moveAura aura dest) activated Stack.resolveTop)
                refused = run theirs
                moved = run mine
                stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
            HU.assertEqual "before either activation the Piker is 2/1 + 1/+0" (Just (3, 1)) (S.powerToughnessOf host ready)
            -- A FAILURE MODE, not a fizzle: the ability resolved, and the only
            -- thing that did not happen is the move.
            HU.assertEqual "the Aura did not move onto bob's creature" (Just (Just (Recipient.ToCreature host))) (fmap Object.attachedTo (Game.lookupObject aura refused))
            HU.assertEqual "and was not restamped (CR 701.3c)" (stampOf ready) (stampOf refused)
            HU.assertEqual "alice's Piker keeps the +1/+0" (Just (3, 1)) (S.powerToughnessOf host refused)
            HU.assertEqual "bob's Piker gains nothing" (Just (2, 1)) (S.powerToughnessOf theirs refused)
            HU.assertBool "and no trample" (not (Projection.hasKeyword Keyword.Trample theirs refused))
            -- CR 704.5m: a refused move must not leave the Aura somewhere the
            -- state-based actions then punish.
            HU.assertBool "the Aura survives the state-based actions" (Set.member aura (GameState.battlefield (S.settleSba (S.settleSba refused))))
            -- The control case, which is what stops the assertions above from
            -- passing for the wrong reason.
            HU.assertEqual "onto a creature alice DOES control it moves" (Just (Just (Recipient.ToCreature mine))) (fmap Object.attachedTo (Game.lookupObject aura moved))
            HU.assertBool "and was restamped" (stampOf ready /= stampOf moved)
            HU.assertEqual "so the Mammoth is 3/3 + 1/+0" (Just (4, 3)) (S.powerToughnessOf mine moved)
            HU.assertBool "with trample (CR 702.19)" (Projection.hasKeyword Keyword.Trample mine moved)
            HU.assertEqual "and the Piker is a plain 2/1" (Just (2, 1)) (S.powerToughnessOf host moved)
          _ -> HU.assertFailure "the fixture wanted one Aura on the Piker, one Crown on the battlefield, and one printed ability"
    ]

-- CR 303.4e's half of the CR 701.3 Attach work: an effect that changes the AURA's
-- own controller and moves it in one resolution. Aura Graft is the proving card --
-- "Gain control of target Aura that's attached to a permanent. Attach it to
-- another permanent it can enchant" -- and each of its three clauses is a
-- different thing from Crown of the Ages' above.
--
-- "target Aura THAT'S ATTACHED TO A PERMANENT" is wider than the Crown's "attached
-- to a creature" and narrower than "attached to anything": CR 303.4 attaches an
-- Aura to "an object or player", so an enchant-player Aura is out.
--
-- "another permanent IT CAN ENCHANT" is a destination restriction the card states
-- in its own text, and it asks about the SUBJECT's enchant ability rather than
-- about the candidate -- Filter.CanHostSubject. Not the same thing as CR 303.4j,
-- which is the backstop for a card that does NOT say it: the Crown offers a
-- creature its Aura may not enchant and the move then fails, where Aura Graft may
-- not offer one at all. The pair of cards is what keeps the two apart.
auraGraftTests :: Registry.Type.Registry -> Tasty.TestTree
auraGraftTests registry =
  Tasty.testGroup
    "AuraGraft"
    [ -- The gameplay-level proof design.md section 4 asks for, and the one the
      -- control clause exists for: CR 303.4e says an Aura's controller is separate
      -- from the enchanted object's, so gaining the Aura and moving it are two
      -- changes -- and Control Magic's own static ability then hands the NEW host
      -- to the Aura's new controller. Three permanents change hands off one spell:
      -- alice takes the Aura, takes the Mammoth it lands on, and gets her own
      -- Piker back because the Aura left it.
      HU.testCase "CR 303.4e whole cards: Aura Graft takes bob's Control Magic and the creature it moves onto" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        controlMagic <- Registry.printing registry "Control Magic"
        auraGraft <- Registry.printing registry "Aura Graft"
        -- {1}{U} for the Graft.
        let base0 = S.landsInPlay island 2
            (host, base1) = S.addCreature piker S.alice base0
            (prize, base2) = S.addCreature warMammoth S.bob base1
            -- A second candidate, so the destination is a real Prompt.ChooseAttachment
            -- choice rather than the single-candidate elision.
            (decoy, base3) = S.addCreature piker S.bob base2
            (aura, base4) = S.addCreature controlMagic S.bob base3
            stolen = S.attach aura host base4
            (gs, graft) = S.handOne auraGraft stolen
            cast = snd (Engine.runGamePure (moveAura aura prize) gs (Cast.castSpell S.alice graft))
            after = snd (Engine.runGamePure (moveAura aura prize) cast Stack.resolveTop)
            settled = S.settleSba (S.settleSba after)
        HU.assertEqual "before: bob's Control Magic holds alice's Piker" (Just S.bob) (Projection.controllerOf host stolen)
        HU.assertEqual "before: bob controls the Mammoth too" (Just S.bob) (Projection.controllerOf prize stolen)
        HU.assertEqual "before: and the Aura itself" (Just S.bob) (Projection.controllerOf aura stolen)
        -- CR 303.4e: the Aura's controller changed, and it is the thing the spell
        -- gained -- not the permanent it was attached to.
        HU.assertEqual "alice controls the Aura now" (Just S.alice) (Projection.controllerOf aura after)
        HU.assertEqual "and it sits on the Mammoth (CR 701.3a)" (Just (Just (Recipient.ToCreature prize))) (fmap Object.attachedTo (Game.lookupObject aura after))
        -- CR 613.1b: Control Magic's SetControllerToSource reads the AURA's
        -- controller, so the creature follows the Aura to alice.
        HU.assertEqual "so alice controls the Mammoth" (Just S.alice) (Projection.controllerOf prize after)
        HU.assertEqual "and her own Piker is hers again" (Just S.alice) (Projection.controllerOf host after)
        HU.assertEqual "the creature nobody chose stays bob's" (Just S.bob) (Projection.controllerOf decoy after)
        -- CR 302.6: alice has not controlled the Mammoth continuously since her
        -- turn began. Nothing re-Sicks it -- the control came from a static
        -- ability, not from the GainControl opcode -- but Object.sickness names
        -- the player it settled under, so the answer is right anyway.
        HU.assertBool "the Mammoth cannot attack for her yet" (not (Combat.canAttack S.alice prize after))
        -- CR 302.6 speaks only of creatures, so the re-Sick the GainControl arm
        -- applies to the Aura is inert -- pinned here because an enchantment is the
        -- first thing that opcode has been aimed at.
        HU.assertEqual "the Aura is re-Sicked all the same" (Just Sickness.Sick) (fmap Object.sickness (Game.lookupObject aura after))
        -- CR 704.5m: it landed on a creature, which its enchant ability admits.
        HU.assertBool "and it survives the state-based actions" (Set.member aura (GameState.battlefield settled)),
      -- "target Aura THAT'S ATTACHED TO A PERMANENT" (CR 601.2c), spelled
      -- `And [HasSubtype Aura, IsAttachedToPermanent]`. Three boundaries at once,
      -- all four Auras on one board: an Aura on a noncreature permanent is in --
      -- which is what makes the atom wider than the Crown's IsAttachedToCreature --
      -- an Aura on a PLAYER is out (CR 303.4: an Aura is attached to "an object or
      -- player", and only one of those is a permanent), and an unattached one is
      -- out.
      --
      -- The Aura on a land is hand-built, as the Crown's own filter test above
      -- hand-builds one: CR 704.5m would bury it on the next state-based-action
      -- pass, so no sequence of card plays leaves one standing to be targeted.
      HU.testCase "CR 601.2c Aura Graft targets an Aura on any permanent, where Crown of the Ages needs one on a creature" $ do
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        curse <- Registry.printing registry "Curse of Death's Hold"
        crown <- Registry.printing registry "Crown of the Ages"
        auraGraft <- Registry.printing registry "Aura Graft"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.alice base
            (land, g2) = S.addCreature mountain S.alice g1
            (onCreature, g3) = S.addCreature unholyStrength S.alice g2
            (onLand, g4) = S.addCreature unholyStrength S.alice g3
            (loose, g5) = S.addCreature unholyStrength S.alice g4
            (onPlayer, g6) = S.addCreature curse S.alice g5
            (crownId, g7) = S.addCreature crown S.alice g6
            (gs, graft) = S.handOne auraGraft g7
            attached =
              S.attachTo onPlayer (Recipient.ToPlayer S.bob) $
                S.attach onLand land (S.attach onCreature creature gs)
            graftOffers oid = fmap (Set.member (Recipient.ToObject oid) . (\spec -> Target.legalRecipients (Just S.alice) graft spec attached)) (S.spellTargetSpec auraGraft)
            crownOffers oid = fmap (Set.member (Recipient.ToObject oid) . (\spec -> Target.legalRecipients (Just S.alice) crownId spec attached)) (crownTargetSpec crown)
        HU.assertEqual "an Aura on a creature is a legal target" (Just True) (graftOffers onCreature)
        HU.assertEqual "and so is one on a land" (Just True) (graftOffers onLand)
        -- The pair that makes IsAttachedToPermanent do work the Crown's atom
        -- cannot: one board, one Aura, two cards, two answers.
        HU.assertEqual "which Crown of the Ages will not have" (Just False) (crownOffers onLand)
        HU.assertEqual "an Aura on a PLAYER is not attached to a permanent" (Just False) (graftOffers onPlayer)
        HU.assertEqual "nor is an unattached one" (Just False) (graftOffers loose)
        -- Not vacuous: the Graft's own slot rejects a permanent that is not an Aura
        -- at all on the very board where it accepts three that are.
        HU.assertEqual "and a plain creature is not an Aura" (Just False) (graftOffers creature),
      -- The window CR 704.3 leaves open between a host leaving and the pass that
      -- buries the Aura (CR 704.5m): the attachment is still recorded, and it
      -- still names an object, but that object is no longer on the battlefield --
      -- and CR 110.1 makes only a battlefield object a permanent. So "attached to
      -- a permanent" is False, and the atom has to read the battlefield rather
      -- than stopping at the stored recipient.
      --
      -- Not reachable while a player holds priority, because CR 704.3 runs the
      -- pass first -- which is exactly why the state has to be built by hand here.
      HU.testCase "CR 110.1 an Aura whose host has left the battlefield is not attached to a permanent" $ do
        piker <- Registry.printing registry "Goblin Piker"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        auraGraft <- Registry.printing registry "Aura Graft"
        let base = Setup.emptyGame S.bothPlayers
            (creature, g1) = S.addCreature piker S.alice base
            (aura, g2) = S.addCreature unholyStrength S.alice g1
            (gs, graft) = S.handOne auraGraft (S.attach aura creature g2)
            bounced = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Hand)
            offers g = fmap (Set.member (Recipient.ToObject aura) . (\spec -> Target.legalRecipients (Just S.alice) graft spec g)) (S.spellTargetSpec auraGraft)
        HU.assertEqual "while the Piker is there the Aura is a legal target" (Just True) (offers gs)
        -- CR 400.7 minted a new object in the hand, so the recipient the Aura
        -- still holds names an id nothing on the battlefield answers to.
        HU.assertBool "the Aura is still attached to the old id" (Maybe.isJust (Game.lookupObject aura bounced >>= Object.attachedTo))
        HU.assertEqual "but it is no longer attached to a PERMANENT" (Just False) (offers bounced),
      -- "another permanent IT CAN ENCHANT" is a destination filter the card states,
      -- not a restatement of CR 303.4j: the Mountain here is a permanent, and CR
      -- 701.3b excludes only the Aura's current host, so a card reading just
      -- "another permanent" would offer it. Aura Graft does not, which is what this
      -- pins -- answering Prompt.ChooseAttachment with the Mountain cannot make the
      -- Aura go there, because the Mountain was never in the offer.
      --
      -- The two readings differ here and only here. Under "the clause is
      -- reminder-like" the Mountain is offered, the answer takes it, and CR 303.4j
      -- leaves the Aura on the Piker; under "the clause is the filter" the Mammoth
      -- is the sole candidate and the Aura moves. Gatherer's 2004-10-04 ruling is
      -- the tiebreak -- "if there is no legal place to move the enchantment, then
      -- it doesn't move" -- which is a sentence about LEGAL PLACES being what the
      -- card looks for.
      HU.testCase "the destination filter offers only a permanent the Aura can enchant, so a Mountain is never a destination" $ do
        island <- Registry.printing registry "Island"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        unholyStrength <- Registry.printing registry "Unholy Strength"
        auraGraft <- Registry.printing registry "Aura Graft"
        -- Gatherer, 2007-07-15: "You can target an Aura you already control just to
        -- move that Aura to a new permanent."
        let base0 = S.landsInPlay island 2
            (host, base1) = S.addCreature piker S.alice base0
            (land, base2) = S.addCreature mountain S.alice base1
            (elsewhere, base3) = S.addCreature warMammoth S.alice base2
            (aura, base4) = S.addCreature unholyStrength S.alice base3
            onPiker = S.attach aura host base4
            (gs, graft) = S.handOne auraGraft onPiker
            cast = snd (Engine.runGamePure (moveAura aura land) gs (Cast.castSpell S.alice graft))
            after = snd (Engine.runGamePure (moveAura aura land) cast Stack.resolveTop)
        HU.assertEqual "before, the Piker carries the +2/+1" (Just (4, 2)) (S.powerToughnessOf host onPiker)
        HU.assertEqual "the Aura went to the Mammoth, not the Mountain" (Just (Just (Recipient.ToCreature elsewhere))) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "so the Mammoth is 3/3 + 2/+1" (Just (5, 4)) (S.powerToughnessOf elsewhere after)
        HU.assertEqual "and the Piker is a plain 2/1" (Just (2, 1)) (S.powerToughnessOf host after),
      -- CR 608.2c: "the spell or ability's controller follows its instructions in
      -- the order written". The control change is the FIRST instruction, so by the
      -- time the destination is chosen the Aura is alice's -- and CR 109.5 makes
      -- the "you" in Setessan Training's "Enchant creature you control" the Aura's
      -- controller, which is now her.
      --
      -- Discriminating with no prompt at all: exactly one destination is legal
      -- under each reading of the order, and they are different creatures. Run the
      -- attach first and the Aura lands on bob's Mammoth.
      HU.testCase "CR 608.2c the control change lands before the destination is chosen, so 'creature you control' means alice's" $ do
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        setessanTraining <- Registry.printing registry "Setessan Training"
        auraGraft <- Registry.printing registry "Aura Graft"
        let base0 = S.landsInPlay island 2
            (host, base1) = S.addCreature piker S.bob base0
            (his, base2) = S.addCreature warMammoth S.bob base1
            (hers, base3) = S.addCreature piker S.alice base2
            (aura, base4) = S.addCreature setessanTraining S.bob base3
            onHis = S.attach aura host base4
            (gs, graft) = S.handOne auraGraft onHis
            cast = snd (Engine.runGamePure (moveAura aura his) gs (Cast.castSpell S.alice graft))
            after = snd (Engine.runGamePure (moveAura aura his) cast Stack.resolveTop)
        HU.assertEqual "alice controls the Aura" (Just S.alice) (Projection.controllerOf aura after)
        HU.assertEqual "so it moved to HER creature" (Just (Just (Recipient.ToCreature hers))) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "which now has +1/+0" (Just (3, 1)) (S.powerToughnessOf hers after)
        HU.assertBool "and trample" (Projection.hasKeyword Keyword.Trample hers after)
        HU.assertEqual "bob's Mammoth was never a legal destination" (Just (3, 3)) (S.powerToughnessOf his after),
      -- Gatherer, 2004-10-04: "If there is no legal place to move the enchantment,
      -- then it doesn't move but you still control it." CR 609.3 -- "if an effect
      -- attempts to do something impossible, it does only as much as possible" --
      -- for the move half, with the control half unaffected, so the two clauses
      -- really are independent, which is the point of CR 303.4e's "an Aura's
      -- controller is separate".
      HU.testCase "with no permanent it can enchant the Aura does not move, and alice still gains it" $ do
        island <- Registry.printing registry "Island"
        mountain <- Registry.printing registry "Mountain"
        piker <- Registry.printing registry "Goblin Piker"
        controlMagic <- Registry.printing registry "Control Magic"
        auraGraft <- Registry.printing registry "Aura Graft"
        let base0 = S.landsInPlay island 2
            (host, base1) = S.addCreature piker S.alice base0
            (_, base2) = S.addCreature mountain S.alice base1
            (aura, base3) = S.addCreature controlMagic S.bob base2
            stolen = S.attach aura host base3
            (gs, graft) = S.handOne auraGraft stolen
            cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject aura)) gs (Cast.castSpell S.alice graft))
            after = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject aura)) cast Stack.resolveTop)
            stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
        HU.assertEqual "it is still on the only creature there is" (Just (Just (Recipient.ToCreature host))) (fmap Object.attachedTo (Game.lookupObject aura after))
        -- CR 701.3c restamps only a permanent that actually moved.
        HU.assertEqual "and was not restamped" (stampOf stolen) (stampOf after)
        HU.assertEqual "but alice controls it" (Just S.alice) (Projection.controllerOf aura after)
        HU.assertEqual "so the creature it holds is hers" (Just S.alice) (Projection.controllerOf host after)
    ]

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
        HU.assertEqual "attached to the creature" [Just (Recipient.ToCreature creature)] (fmap Object.attachedTo auras)
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
      -- CR 704.5m's remaining clause: unattached. Its third clause -- attached to an
      -- object the enchant spec no longer admits (CR 303.4c) -- is the Control Magic
      -- and Setessan Training case below; nothing in the pool strips creature-ness
      -- from a permanent, so a CONTROL change is how that clause is reached.
      HU.testCase "CR 704.5m: an unattached Aura on the battlefield goes to the graveyard" $ do
        unholyStrength <- Registry.printing registry "Unholy Strength"
        let base = Setup.emptyGame S.bothPlayers
            (aura, gs) = S.addCreature unholyStrength S.alice base
            after = S.settleSba gs
        HU.assertBool "never attached, so it falls off immediately" (not (Set.member aura (GameState.battlefield after))),
      -- Setessan Training's own three lines, at gameplay level (design.md section 4):
      -- "Enchant creature you control" (CR 702.5a) narrowing the enchant slot, "When
      -- this Aura enters, draw a card" firing, and "+1/+0 and has trample" (CR
      -- 702.19) landing on the enchanted creature.
      --
      -- The first pair of assertions is the one the filter exists for: CR 109.5's
      -- "you" on an enchant ability is the Aura's would-be controller while the
      -- spell is being cast, so alice's creature is offered and bob's is withheld.
      -- The spec is read out of the committed card, never hand-built.
      HU.testCase "CR 702.5a whole card: Setessan Training enchants only its caster's creature, draws, and grants +1/+0 and trample" $ do
        forest <- Registry.printing registry "Forest"
        piker <- Registry.printing registry "Goblin Piker"
        setessanTraining <- Registry.printing registry "Setessan Training"
        let base0 = S.landsInPlay forest 2
            (mine, base1) = S.addCreature piker S.alice base0
            (theirs, base2) = S.addCreature piker S.bob base1
            -- Something to draw, so the trigger is observable rather than an
            -- attempted draw from an empty library (CR 704.5b).
            (_, base3) = S.addLibraryCard forest S.alice base2
            (gs, spellId) = S.handOne setessanTraining base3
            offered = fmap (\spec -> Target.legalRecipients (Just S.alice) spellId spec gs) (Card.Type.enchant (Printing.card setessanTraining))
            cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature mine)) gs (Cast.castSpell S.alice spellId))
            after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
            -- CR 704.3: the enters trigger waits until a player would get priority,
            -- which resolveTop alone never reaches.
            placed = snd (Engine.runGamePure S.identityAnswer after Engine.placePendingTriggers)
            drawn = snd (Engine.runGamePure S.identityAnswer placed Stack.resolveTop)
        HU.assertEqual "alice's own creature is a legal host" (Just True) (fmap (Set.member (Recipient.ToCreature mine)) offered)
        HU.assertEqual "bob's is not" (Just False) (fmap (Set.member (Recipient.ToCreature theirs)) offered)
        HU.assertEqual "exactly one permanent entered attached to alice's creature" 1 (length (attachedTo mine after))
        HU.assertEqual "and nothing is attached to bob's" 0 (length (attachedTo theirs after))
        HU.assertEqual "the enchanted creature is a 2/1 plus 1/+0" (Just (3, 1)) (S.powerToughnessOf mine after)
        HU.assertBool "and has trample" (Projection.hasKeyword Keyword.Trample mine after)
        HU.assertBool "bob's Piker has neither" (not (Projection.hasKeyword Keyword.Trample theirs after))
        HU.assertEqual "bob's Piker is still 2/1" (Just (2, 1)) (S.powerToughnessOf theirs after)
        HU.assertEqual "the cast emptied alice's hand" 0 (S.handSize S.alice after)
        HU.assertEqual "and the enters trigger refills it" 1 (S.handSize S.alice drawn),
      -- CR 704.5m's third clause -- "attached to an illegal object ... as defined by
      -- its enchant ability and other applicable effects" (CR 303.4c) -- reached
      -- without touching the host's card types: Setessan Training says "enchant
      -- creature you control", so an opponent STEALING the creature is enough. CR
      -- 109.5 makes that "you" the Aura's controller (enchant is a static ability,
      -- CR 702.5a), which is why the answer changes when control does.
      --
      -- This is the case Pawl.Sba.stillLegalEnchant's Filter fallthrough exists for.
      -- Its Pool.Creatures-with-no-Filter reduction -- still a creature, on the
      -- battlefield, owned by a player still in the game -- would answer "legal"
      -- here, because none of those three facts changed.
      --
      -- Discriminating on one board and one pass: Control Magic's own enchant spec
      -- is a bare "enchant creature", so it stays attached to the very creature
      -- Setessan Training just fell off.
      HU.testCase "CR 704.5m whole cards: Control Magic steals the enchanted creature, so Setessan Training is buried and Control Magic is not" $ do
        forest <- Registry.printing registry "Forest"
        island <- Registry.printing registry "Island"
        piker <- Registry.printing registry "Goblin Piker"
        setessanTraining <- Registry.printing registry "Setessan Training"
        controlMagic <- Registry.printing registry "Control Magic"
        -- {1}{G} for alice's Aura, {2}{U}{U} for bob's. S.landsInPlay seats
        -- alice's lands and nothing else, so bob's go in one at a time through
        -- S.addCreature -- which puts a printing onto the battlefield whatever its
        -- card types are, despite the name.
        let base0 = S.landsInPlay forest 2
            (creature, base1) = S.addCreature piker S.alice base0
            (_, base2) = S.addCreature island S.bob base1
            (_, base3) = S.addCreature island S.bob base2
            (_, base4) = S.addCreature island S.bob base3
            (_, base5) = S.addCreature island S.bob base4
            (gs, auraSpell) = S.handOne setessanTraining base5
            castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) gs (Cast.castSpell S.alice auraSpell))
            enchanted = snd (Engine.runGamePure S.identityAnswer castAura Stack.resolveTop)
        case attachedTo creature enchanted of
          [training] -> do
            let (stealId, withSteal) = S.addHandCard controlMagic S.bob enchanted
                ready = withSteal {GameState.priority = Just S.bob, GameState.activePlayer = S.bob}
                castSteal = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) ready (Cast.castSpell S.bob stealId))
                stolen = snd (Engine.runGamePure S.identityAnswer castSteal Stack.resolveTop)
                settled = S.settleSba stolen
                survivors = attachedTo creature settled
            -- The control case: with control unchanged the Aura is legal, so an SBA
            -- pass leaves it alone.
            HU.assertBool "while alice still controls the creature the Aura survives a pass" (S.onBattlefield training (S.settleSba enchanted))
            HU.assertEqual "the steal really moved control (CR 613.1b)" (Just S.bob) (Projection.controllerOf creature stolen)
            HU.assertBool "Setessan Training is off the battlefield" (not (S.onBattlefield training settled))
            HU.assertEqual "and in its OWNER's graveyard, not the thief's" 1 (length (Game.zoneMembers Zone.Graveyard S.alice settled))
            HU.assertEqual "bob's graveyard is empty" 0 (length (Game.zoneMembers Zone.Graveyard S.bob settled))
            HU.assertEqual "exactly one Aura is left on the creature" 1 (length survivors)
            HU.assertEqual "and it is Control Magic, whose enchant spec narrows nothing" [Just (Printing.card controlMagic)] (fmap (\oid -> Game.cardOf oid settled) survivors)
            HU.assertEqual "so the creature is a plain 2/1 again" (Just (2, 1)) (S.powerToughnessOf creature settled)
            HU.assertBool "and has lost trample" (not (Projection.hasKeyword Keyword.Trample creature settled))
          _ -> HU.assertFailure "Setessan Training should have entered attached to alice's Piker",
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
