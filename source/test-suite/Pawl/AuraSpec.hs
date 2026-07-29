-- Pattern matching on Pawl.Type.Prompt, a GADT, in aimAt below.
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
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Modal as Modal
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Resolve as Resolve
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
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
            HU.assertEqual "still enchanting the Piker at that moment" (Just (Just creature)) (fmap Object.attachedTo (Game.lookupObject aura animated))
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

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Pawl.Aura" [auraTests registry, equipmentTests registry, unattachableTests registry, reattachTests registry]

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
            auraId = case filter (\oid -> fmap Object.attachedTo (Game.lookupObject oid enchanted) == Just (Just first)) (Set.toList (GameState.battlefield enchanted)) of
              oid : _ -> Just oid
              [] -> Nothing
        case auraId of
          Nothing -> HU.assertFailure "Unholy Strength should have entered attached to the Piker"
          Just aura -> do
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
                    let answer :: Prompt.Prompt r -> r
                        answer p = case p of
                          Prompt.ChooseTargets _ _ _ sets -> fmap (const (Recipient.ToObject aura)) sets
                          Prompt.ChooseAttachment {} -> second
                          _ -> S.identityAnswer p
                        ready = onBattlefield {GameState.priority = Just S.alice}
                        activated = snd (Engine.runGamePure answer ready (Activate.activateAbility S.alice crownObj move))
                        after = snd (Engine.runGamePure answer activated Stack.resolveTop)
                        settled = S.settleSba (S.settleSba after)
                    HU.assertEqual "before, the Piker is 2/1 + 2/+1" (Just (4, 2)) (S.powerToughnessOf first onBattlefield)
                    HU.assertEqual "and the Mammoth is a plain 3/3" (Just (3, 3)) (S.powerToughnessOf second onBattlefield)
                    HU.assertEqual "the Aura is attached to the Mammoth now" (Just (Just second)) (fmap Object.attachedTo (Game.lookupObject aura after))
                    HU.assertEqual "so the Piker is back to 2/1" (Just (2, 1)) (S.powerToughnessOf first after)
                    HU.assertEqual "and the Mammoth is 5/4" (Just (5, 4)) (S.powerToughnessOf second after)
                    HU.assertEqual "the creature nobody chose is untouched" (Just (2, 1)) (S.powerToughnessOf decoy after)
                    -- CR 704.5m: the Aura landed on a legal host, so nothing buries it.
                    HU.assertBool "the Aura survives the state-based actions" (Set.member aura (GameState.battlefield settled))
                    HU.assertEqual "still on the Mammoth" (Just (Just second)) (fmap Object.attachedTo (Game.lookupObject aura settled)),
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
        HU.assertEqual "it moved" (Just (Just second)) (fmap Object.attachedTo (Game.lookupObject aura after))
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
        HU.assertEqual "still on the Piker" (Just (Just first)) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "and not restamped" (stampOf gs) (stampOf after),
      -- CR 303.4j: "If an effect attempts to attach an Aura on the battlefield to
      -- an object or player it can't legally enchant, the Aura doesn't move." A
      -- FAILURE MODE, not a fizzle: the ability resolved, and the only thing that
      -- did not happen is the move.
      --
      -- No printed card in the pool reaches this through its own text: every
      -- Card.enchant in the pool is an unfiltered Pool.Creatures, and Crown of the
      -- Ages only ever offers creatures, so every destination it can offer is one
      -- the Aura may legally enchant. The opcode is driven directly with a wider
      -- destination filter instead -- the same way the Effect.Attach cases in the
      -- Equipment group above drive that opcode -- because the clause is the rule
      -- whether or not a card exercises it (#355).
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
        HU.assertEqual "the Aura did not move onto the land" (Just (Just first)) (fmap Object.attachedTo (Game.lookupObject aura after))
        HU.assertEqual "and was not restamped" (stampOf gs) (stampOf after)
        HU.assertEqual "the Piker still has the bonus" (Just (4, 2)) (S.powerToughnessOf first after)
        -- CR 704.5m: a failed move must not leave the Aura in a state the
        -- state-based actions then punish -- it is still on a legal host.
        HU.assertBool "and the Aura is not buried afterwards" (Set.member aura (GameState.battlefield settled))
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
