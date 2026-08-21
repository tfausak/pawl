-- Pattern matching on Pawl.Types.Prompt, a GADT, in aimAt below.
{-# LANGUAGE GADTs #-}
-- replenishSpec's `run` takes an ANSWERER, which is polymorphic in the prompt's
-- answer type -- Pawl.CostSpec's reason for the same pragma.
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Stack's Aura branch and Pawl.Engine.Resolve.targetsAllIllegal -- a
-- resolving Aura spell either fizzles (CR 608.2b) or enters the battlefield
-- already attached to its target (CR 303.4) -- together with the rest of the
-- attachment substrate that shares Object.attachedTo: Pawl.Engine.Resolve's Attach
-- opcode over Pawl.Engine.Attach (CR 701.3) and Pawl.Engine.Sba's three attachment
-- state-based actions (CR 704.5m, 704.5n, 704.5p). Resolution is not the only door
-- onto the battlefield: CR 303.4f's Aura entering from anywhere else has its host
-- chosen inside Pawl.Engine.Event.changeZoneAttaching, which is replenishSpec's,
-- and a search whose destination NAMES the host seeds that same funnel instead,
-- which is couldEnchantSpec's.
-- Rule 701.3's OTHER caller, CR 303.4k's attachment as an Aura is turned face up,
-- is Pawl.FaceDownSpec's: CR 708.11 puts it inside the turning-over rather than in
-- a resolution.
--
-- Also Pawl.Engine.Replacement's CR 614.1c as-enters basic-land-type choice,
-- since the pool's one producer of it is an Aura (Convincing Mirage) and
-- proving it needs a real cast through this file's machinery.
module Pawl.AuraSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.AttachTarget as AttachTarget
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Departure as Departure.Type
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Zone as Zone

-- CR 301.5 / 702.6: Equipment. Shares the attachment substrate with Auras --
-- Object.attachedTo, Affected.Attached -- so what is genuinely new is the CR
-- 701.3 Attach keyword action that MOVES an already-on-the-battlefield permanent,
-- and CR 704.5n's detach-rather-than-bury state-based action (#193). The
-- Reattach group below is the same keyword action aimed the other way, at a
-- permanent the effect TARGETS rather than at its own source.
equipmentSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
equipmentSpec s registry = Spec.describe s "Equipment" $ do
  -- CR 702.6a: "Equip [cost]" means "[Cost]: Attach this permanent to target
  -- creature you control." The Equipment is the ability's SOURCE; the slot is
  -- what it attaches to.
  Spec.it s "CR 702.6a equipping attaches the Equipment to the target creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.alice base
        (equip, gs) = S.addCreature bonesplitter S.alice withCreature
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            equip
            equip
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature creature)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature creature)))
            (Effect.Attach slot)
        after = S.runPure S.identityAnswer gs run
    Spec.assertEqWith s "the Equipment is attached to the creature" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just (Just (Recipient.ToCreature creature)))
    Spec.assertBool s (Set.member equip (GameState.battlefield after)) "and is still on the battlefield"
  -- CR 301.5a: "The creature an Equipment is attached to is called the
  -- 'equipped creature'." Affected.Attached already means exactly that, so
  -- Bonesplitter's +2/+0 rides the same path Unholy Strength does.
  Spec.it s "CR 301.5a the equipped creature gets the Equipment's bonus" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.alice base
        (equip, gs) = S.addCreature bonesplitter S.alice withCreature
        attached = S.attach equip creature gs
    Spec.assertEqWith s "unequipped, the Piker is 2/1" (Projection.powerOf creature gs) (Just 2)
    Spec.assertEqWith s "equipped, it is 4/1" (Projection.powerOf creature attached) (Just 4)
    Spec.assertEqWith s "toughness is untouched by +2/+0" (Projection.toughnessOf creature attached) (Just 1)
  -- CR 701.3a: attaching a permanent that is already attached MOVES it --
  -- "take it from where it currently is and put it onto that object".
  Spec.it s "CR 701.3a equipping again moves the Equipment off the first creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (first, g1) = S.addCreature piker S.alice base
        (second, g2) = S.addCreature warMammoth S.alice g1
        (equip, g3) = S.addCreature bonesplitter S.alice g2
        gs = S.attach equip first g3
        slot = SlotName.MkSlotName (Text.pack "target")
        run =
          Resolve.applyEffect
            equip
            equip
            S.alice
            (Map.singleton slot (Set.singleton (Recipient.ToCreature second)))
            (Map.singleton slot (Set.singleton (Recipient.ToCreature second)))
            (Effect.Attach slot)
        after = S.runPure S.identityAnswer gs run
    Spec.assertEqWith s "it moved to the second creature" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just (Just (Recipient.ToCreature second)))
    Spec.assertEqWith s "the first creature is back to 2 power" (Projection.powerOf first after) (Just 2)
    Spec.assertEqWith s "the second is 3+2" (Projection.powerOf second after) (Just 5)
  -- The gameplay-level proof design.md section 4 asks for: cast Bonesplitter,
  -- activate its printed equip ability through the real activation path, let
  -- it resolve, and see the creature actually hit harder. Everything above
  -- drives Effect.Attach directly; this drives the CARD.
  Spec.it s "CR 702.6 whole card: cast Bonesplitter, equip a Piker, and it swings for 4" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base0 = S.landsInPlay mountain 2 -- {1} to cast, {1} to equip
        (creature, base1) = S.addCreature piker S.alice base0
        (withSpell, spellId) = S.handOne bonesplitter base1
        cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        equipId = case filter (\oid -> Game.cardOf oid resolved == Just (Printing.card bonesplitter)) (Set.toList (GameState.battlefield resolved)) of
          oid : _ -> Just oid
          [] -> Nothing
    case equipId of
      Nothing -> Spec.assertFailure s "Bonesplitter should have resolved onto the battlefield"
      Just equip -> do
        let ability = case Face.activatedAbilities (S.combinedFace bonesplitter) of
              ab : _ -> Just ab
              [] -> Nothing
        case ability of
          Nothing -> Spec.assertFailure s "Bonesplitter should print an equip ability"
          Just equipAbility -> do
            let ready = resolved {GameState.priority = Just S.alice}
                activated = snd (Engine.runGamePure S.identityAnswer ready (Activate.activateAbility S.alice equip equipAbility))
                after = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            Spec.assertEqWith s "unequipped the Piker is 2/1" (Projection.powerOf creature resolved) (Just 2)
            Spec.assertEqWith s "the equip ability attached it" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just (Just (Recipient.ToCreature creature)))
            Spec.assertEqWith s "and the Piker is now 4/1" (Projection.powerOf creature after) (Just 4)
            Spec.assertEqWith s "toughness unchanged" (Projection.toughnessOf creature after) (Just 1)
  -- CR 701.3c: "Attaching an Aura, Equipment, or Fortification on the
  -- battlefield to a different object or player causes [it] to receive a new
  -- timestamp." That feeds CR 613.7's layer ordering, so it is not cosmetic.
  -- CR 701.3b's second sentence is the other half: re-attaching to the object
  -- it is ALREADY attached to "does nothing", so no new timestamp there.
  Spec.it s "CR 701.3c attaching to a different creature restamps; re-attaching to the same one does not" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
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
              equip
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToCreature t)))
              (Map.singleton slot (Set.singleton (Recipient.ToCreature t)))
              (Effect.Attach slot)
        stampOf g = fmap Object.timestamp (Game.lookupObject equip g)
        moved = attachTo second gs
        again = attachTo second moved
    Spec.assertBool s (stampOf moved /= stampOf gs) "moving it to a different creature restamps"
    Spec.assertEqWith s "re-attaching to the same creature does nothing" (stampOf again) (stampOf moved)
  -- CR 704.5n: "If an Equipment or Fortification is attached to an illegal
  -- permanent or to a player, it becomes unattached from that permanent or
  -- player. It REMAINS ON THE BATTLEFIELD." The shape difference from an
  -- Aura, which CR 704.5m buries instead (#193).
  Spec.it s "CR 704.5n an Equipment whose creature dies detaches and stays on the battlefield" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.alice base
        (equip, g2) = S.addCreature bonesplitter S.alice withCreature
        attached = S.attach equip creature g2
        gone = S.runPure S.identityAnswer attached (Event.changeZone creature Zone.Graveyard)
        after = S.settleSba gone
    Spec.assertBool s (Set.member equip (GameState.battlefield after)) "the Equipment survives"
    Spec.assertEqWith s "and is unattached" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just Nothing)
  -- CR 301.5: "It can't legally be attached to anything that isn't a
  -- creature." An Equipment left on a noncreature permanent detaches too --
  -- the same SBA, a different way of becoming illegal.
  Spec.it s "CR 301.5 an Equipment attached to a noncreature permanent detaches" $ do
    mountain <- S.printingOf s registry "Mountain"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base = Setup.emptyGame S.bothPlayers
        (land, withLand) = S.addCreature mountain S.alice base
        (equip, g2) = S.addCreature bonesplitter S.alice withLand
        attached = S.attach equip land g2
        after = S.settleSba attached
    Spec.assertBool s (Set.member equip (GameState.battlefield after)) "the Equipment survives"
    Spec.assertEqWith s "and is unattached" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just Nothing)

-- An answerer that aims every target slot at one object, deferring everything
-- else to S.identityAnswer (ModalSpec.chooseModeAt's shape). The CR 303.4d case
-- below needs it because both of its target choices are real ones -- alice
-- controls other permanents that each target slot admits -- so they cannot be forced by
-- board construction the way the sibling cases above force theirs.
aimAt :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAt oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject oid))) sets
  _ -> S.identityAnswer p

-- CR 704.5p, the sibling of CR 704.5n above: 704.5n asks whether the HOST is
-- still legal, and this asks whether the attached permanent may be attached to
-- anything at all. Both detach and leave the permanent on the battlefield.
unattachableSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
unattachableSpec s registry = Spec.describe s "Unattachable" $ do
  -- CR 704.5p, first sentence: "If a battle or creature is attached to an
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
  Spec.it s "CR 704.5p whole card: animating an equipped Bonesplitter detaches it and the Piker loses the bonus" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    animator <- S.printingOf s registry "Skilled Animator"
    let base = S.landsInPlay island 3 -- {2}{U}
        (creature, g1) = S.addCreature piker S.alice base
        (equip, g2) = S.addCreature bonesplitter S.alice g1
        attached = S.attach equip creature g2
        (withSpell, spellId) = S.handOne animator attached
        cast = snd (Engine.runGamePure S.identityAnswer withSpell (S.cast S.alice spellId))
        resolved = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- The ETB trigger goes on the stack here, with its target chosen:
        -- Bonesplitter is the only artifact alice controls, so CR 603.3d's
        -- choice is forced.
        triggered = snd (Engine.runGamePure S.identityAnswer resolved Engine.settleForPriority)
        animated = snd (Engine.runGamePure S.identityAnswer triggered Stack.resolveTop)
        after = snd (Engine.runGamePure S.identityAnswer animated Engine.settleForPriority)
    Spec.assertEqWith s "equipped, the Piker was 4/1" (Projection.powerOf creature attached) (Just 4)
    Spec.assertBool s (not (null (GameState.stack triggered))) "the enters-the-battlefield trigger really was on the stack"
    Spec.assertEqWith s "the Equipment is now a 5/5 creature" (Projection.powerOf equip after) (Just 5)
    Spec.assertBool s (Set.member equip (GameState.battlefield after)) "it stays on the battlefield"
    Spec.assertEqWith s "and is unattached" (fmap Object.attachedTo (Game.lookupObject equip after)) (Just Nothing)
    Spec.assertEqWith s "so the Piker is back to 2 power" (Projection.powerOf creature after) (Just 2)
  -- CR 704.5p, second sentence: "if any nonbattle, noncreature permanent
  -- that's neither an Aura, an Equipment, nor a Fortification is attached to
  -- an object or player, it becomes unattached and remains on the
  -- battlefield." No card can produce this state -- nothing in the pool
  -- strips the Equipment subtype, and nothing attaches a land -- so the
  -- attachment is hand-built, the way the CR 301.5 case in the Equipment
  -- group above hand-builds an Equipment on a land. What it pins is that the
  -- branch reads the permanent's own types and not its host's: the Piker here
  -- is a legal host for anything that may be attached at all.
  Spec.it s "CR 704.5p a land attached to a creature detaches and stays on the battlefield" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.alice base
        (land, g2) = S.addCreature mountain S.alice withCreature
        attached = S.attach land creature g2
        after = S.settleSba attached
    Spec.assertBool s (Set.member land (GameState.battlefield after)) "the land survives"
    Spec.assertEqWith s "and is unattached" (fmap Object.attachedTo (Game.lookupObject land after)) (Just Nothing)
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
  Spec.it s "CR 303.4d whole cards: an Aura made a creature unattaches, then is buried" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    coating <- S.printingOf s registry "Liquimetal Coating"
    animator <- S.printingOf s registry "Skilled Animator"
    let base = S.landsInPlay island 3 -- {2}{U} for the Animator
        (creature, g1) = S.addCreature piker S.alice base
        (aura, g2) = S.addCreature unholyStrength S.alice g1
        attached = S.attach aura creature g2
        (coatingId, g3) = S.addCreature coating S.alice attached
        ability = case Face.activatedAbilities (S.combinedFace coating) of
          ab : _ -> Just ab
          [] -> Nothing
    case ability of
      Nothing -> Spec.assertFailure s "Liquimetal Coating should print one activated ability"
      Just coat -> do
        let ready = g3 {GameState.priority = Just S.alice}
            activated = snd (Engine.runGamePure (aimAt aura) ready (Activate.activateAbility S.alice coatingId coat))
            coated = snd (Engine.runGamePure (aimAt aura) activated Stack.resolveTop)
            (withSpell, spellId) = S.handOne animator coated
            cast = snd (Engine.runGamePure (aimAt aura) withSpell (S.cast S.alice spellId))
            entered = snd (Engine.runGamePure (aimAt aura) cast Stack.resolveTop)
            triggered = snd (Engine.runGamePure (aimAt aura) entered Engine.settleForPriority)
            animated = snd (Engine.runGamePure (aimAt aura) triggered Stack.resolveTop)
            -- S.settleSba is ONE pass (the CR 704.3 repeat lives in
            -- Engine.settleForPriority), which is what makes the two steps
            -- separately observable.
            unattachedNow = S.settleSba animated
            buried = S.settleSba unattachedNow
        Spec.assertBool s (Set.member CardType.Artifact (Projection.cardTypesOf aura coated)) "the Aura is an artifact now"
        Spec.assertEqWith s "and once animated it is a 5/5 creature" (Projection.powerOf aura animated) (Just 5)
        Spec.assertEqWith s "still enchanting the Piker at that moment" (fmap Object.attachedTo (Game.lookupObject aura animated)) (Just (Just (Recipient.ToCreature creature)))
        Spec.assertEqWith s "which is still 2/1 + 2/+1" (Projection.powerOf creature animated) (Just 4)
        -- Step one: unattached, and still on the battlefield.
        Spec.assertEqWith s "one SBA pass unattaches it" (fmap Object.attachedTo (Game.lookupObject aura unattachedNow)) (Just Nothing)
        Spec.assertBool s (Set.member aura (GameState.battlefield unattachedNow)) "and it has not been buried yet"
        -- Step two: CR 704.5m buries the now-unattached Aura.
        Spec.assertBool s (not (Set.member aura (GameState.battlefield buried))) "the next pass buries it"
        -- CR 400.7: it is a new object there, so this counts the zone rather
        -- than looking the battlefield id up again.
        Spec.assertEqWith s "in its OWNER's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice buried)) 1
        Spec.assertEqWith s "so the Piker loses the +2/+1" (Projection.powerOf creature buried) (Just 2)

-- CR 702.5d: "Auras that can enchant a player can target and be attached to
-- players. Such Auras can't target permanents and can't be attached to
-- permanents." Curse of Death's Hold is the proving card -- "Enchant player.
-- Creatures enchanted player controls get -1/-1" -- and it is the one that needs
-- BOTH halves of an enchant-player Aura: the Pool.Players enchant slot, which
-- Face.enchant could already express, and a static ability whose affected set is
-- reached THROUGH the enchanted player (Affected.AttachedPlayerControls).
enchantPlayerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
enchantPlayerSpec s registry = Spec.describe s "EnchantPlayer" $ do
  -- The gameplay proof design.md section 4 asks for: cast the real card at a
  -- real player, let it resolve, and see the creatures on the other side of
  -- the table get smaller. CR 303.4: an Aura "enters the battlefield attached
  -- to an object or player", so the attachment is asserted on the incarnation
  -- that entered, not written by a later step.
  Spec.it s "CR 702.5d whole card: Curse of Death's Hold enters attached to the player it targeted and shrinks that player's creatures" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    let base = S.landsInPlay swamp 5
        (his, withHis) = S.addCreature piker S.bob base
        (hers, withBoth) = S.addCreature piker S.alice withHis
        (gs, spellId) = S.handOne curse withBoth
        answer = aimRecipient (Recipient.ToPlayer S.bob)
        cast = snd (Engine.runGamePure answer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure answer cast Stack.resolveTop)
        settled = S.settleSba after
    Spec.assertEqWith s "one attached permanent, and it is attached to bob himself" (attachments after) [Just (Recipient.ToPlayer S.bob)]
    Spec.assertEqWith s "bob's Goblin Piker is a 1/0" (S.powerToughnessOf his after) (Just (1, 0))
    Spec.assertEqWith s "alice's is untouched -- she is not the enchanted player" (S.powerToughnessOf hers after) (Just (2, 1))
    -- CR 704.5f: the shrunk creature has toughness 0, so the pass that judges
    -- the Curse legal buries the Piker.
    Spec.assertEqWith s "so his Piker dies on the next SBA pass" (Game.lookupObject his settled) Nothing
    Spec.assertEqWith s "and the Curse is still attached to him -- he is still in the game" (attachments settled) [Just (Recipient.ToPlayer S.bob)]
  -- The affected set is DYNAMIC in its controller half, which is what makes it
  -- a set rather than a list of ids: CR 613.1b applies control changes in
  -- layer 2, before the layer 7c this ability lands in, so a creature the
  -- enchanted player no longer controls is out of the set on the very next
  -- projection. Control Magic moves it: `data/cards/`'s control-changing Auras
  -- are it and Confiscate, and Control Magic's creature-only enchant slot is the
  -- narrower fit. Synthetic Goblin Dominion moves control too, but by a
  -- predicate rather than an attachment, so it cannot be pointed at one creature.
  Spec.it s "CR 613.1b: a creature stolen from the enchanted player leaves the Curse's affected set" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.bob base
        (aura, withAura) = S.addCreature curse S.alice withCreature
        cursed = S.attachTo aura (Recipient.ToPlayer S.bob) withAura
        (steal, withSteal) = S.addCreature controlMagic S.alice cursed
        stolen = S.attach steal creature withSteal
    Spec.assertEqWith s "bob controls it and it is shrunk" (S.powerToughnessOf creature cursed) (Just (1, 0))
    Spec.assertEqWith s "alice controls it now, so the Curse does not reach it" (S.powerToughnessOf creature stolen) (Just (2, 1))
  -- CR 704.5m's remaining clause, and the one only an enchant-player Aura can
  -- reach: CR 303.4c spells it out as "the player it was attached to has left
  -- the game". Three seats, because CR 104.2a ends a two-player game the
  -- moment anyone leaves and no state-based action would ever be checked
  -- again.
  Spec.it s "CR 704.5m / 303.4c: a Curse attached to a player who has left the game is put into its owner's graveyard" $ do
    curse <- S.printingOf s registry "Curse of Death's Hold"
    let (aura, withAura) = S.addCreature curse S.alice S.threePlayerGame
        attached = S.attachTo aura (Recipient.ToPlayer S.carol) withAura
        before = S.settleSba attached
        departed = S.departs Departure.Type.Conceded S.carol before
        after = S.settleSba departed
    Spec.assertBool s (Set.member aura (GameState.battlefield before)) "while carol is in the game the Curse is legally attached"
    Spec.assertBool s (not (Set.member aura (GameState.battlefield after))) "she leaves, and it is off the battlefield after one pass"
    Spec.assertEqWith s "in its OWNER's graveyard -- a put-into-graveyard, not a destruction" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 702.5d's second sentence -- such Auras "can't target permanents and
  -- can't be attached to permanents" -- at the reattach door, and it needs no
  -- clause of its own: Crown of the Ages moves "target Aura attached to a
  -- creature", and CR 701.3a's AttachedTo reads the attachment for the OBJECT
  -- it names, which a player attachment does not name at all. So
  -- the Curse is not a legal target and there is nothing to refuse later.
  Spec.it s "CR 702.5d: a Curse attached to a player is not an Aura attached to a creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    crown <- S.printingOf s registry "Crown of the Ages"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.alice base
        (onCreature, g2) = S.addCreature unholyStrength S.alice g1
        (onPlayer, g3) = S.addCreature curse S.alice g2
        (crownId, g4) = S.addCreature crown S.alice g3
        gs = S.attachTo onPlayer (Recipient.ToPlayer S.bob) (S.attach onCreature creature g4)
    case activatedTargetSlot crown of
      Nothing -> Spec.assertFailure s "the fixture wanted Crown of the Ages' one printed target slot"
      Just theSlot ->
        Spec.assertEqWith
          s
          "only the Aura on a creature is offered"
          (Target.legalRecipients (Just S.alice) crownId theSlot gs)
          (Set.singleton (Recipient.ToObject onCreature))

-- CR 702.5c: "If an Aura has multiple instances of enchant, all of them apply.
-- The Aura's target must follow the restrictions from all the instances of
-- enchant. The Aura can enchant only objects or players that match all of its
-- enchant abilities." Pawl.Engine.Card.enchantTargetSlot is the conjunction, and
-- this group is what proves it applies at all three doors CR 702.5a opens: the
-- cast's target legality (CR 601.2c), the state-based re-check (CR 704.5m /
-- 303.4c) and attachment admission (CR 701.3a, through the same slot).
--
-- SYNTHETIC, and the last rank in design.md section 6's order: no printing has
-- ever carried two instances of enchant, so nothing better exists. Both halves
-- of the invented card ARE printed text, on different cards -- "enchant creature
-- you control" (Setessan Training, in this pool) and "enchant tapped creature"
-- (Entangling Vines, Glimmerdust Nap) -- so only their combination is new, and CR
-- 702.5c is a rule that says what such a card DOES rather than one forbidding it.
--
-- The two restrictions are INDEPENDENT on purpose, and that is what makes the
-- group discriminating rather than decorative. An Aura whose second restriction
-- were implied by its first would behave identically on an engine that honoured
-- only one, so each test below turns on a creature satisfying exactly ONE:
-- alice's untapped creature and bob's tapped one at the cast, and then each
-- restriction broken in turn while the other still holds -- untapping the host
-- (the second fails) and Control Magic stealing it (the first fails). Dropping
-- either instance from the fold fails one of these two.
twoEnchantSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
twoEnchantSpec s registry = Spec.describe s "TwoEnchantAbilities" $ do
  -- CR 601.2c through CR 702.5a's first job: what the Aura SPELL may target.
  -- Read out of the committed card through Card.enchantTargetSlot, never
  -- hand-built.
  Spec.it s "CR 702.5c whole card: only a creature matching BOTH instances of enchant is a legal host" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    twofold <- S.printingOf s registry "Synthetic Twofold Enchant"
    let base0 = S.landsInPlay plains 2
        (mineTapped, base1) = S.addCreature piker S.alice base0
        (mineUntapped, base2) = S.addCreature piker S.alice base1
        (theirsTapped, base3) = S.addCreature piker S.bob base2
        base4 = S.tapObject theirsTapped (S.tapObject mineTapped base3)
        (gs, spellId) = S.handOne twofold base4
        offered = fmap (\theSlot -> Target.legalRecipients (Just S.alice) spellId theSlot gs) (Card.enchantTargetSlot (S.combinedFace twofold))
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature mineTapped)) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        settled = S.settleSba after
    Spec.assertEqWith s "alice's TAPPED creature matches both" (fmap (Set.member (Recipient.ToCreature mineTapped)) offered) (Just True)
    Spec.assertEqWith s "her untapped one matches only 'creature you control'" (fmap (Set.member (Recipient.ToCreature mineUntapped)) offered) (Just False)
    Spec.assertEqWith s "bob's tapped one matches only 'tapped creature'" (fmap (Set.member (Recipient.ToCreature theirsTapped)) offered) (Just False)
    Spec.assertEqWith s "the Aura entered attached to the one creature that matched both" (length (attachedTo mineTapped after)) 1
    Spec.assertEqWith s "the enchanted creature is a 2/1 plus +2/+2" (S.powerToughnessOf mineTapped settled) (Just (4, 3))
    Spec.assertEqWith s "and a state-based pass leaves it alone, since both instances still hold" (length (attachedTo mineTapped settled)) 1
  -- CR 704.5m / 303.4c with the SECOND instance broken and the first untouched:
  -- alice still controls the creature, but CR 502.3's untap step untaps it, so
  -- "enchant tapped creature" no longer admits it. An engine that read only the
  -- first instance would keep the Aura here.
  Spec.it s "CR 704.5m: the host untapping breaks the second instance, so the Aura is buried" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    twofold <- S.printingOf s registry "Synthetic Twofold Enchant"
    let base0 = S.landsInPlay plains 2
        (creature, base1) = S.addCreature piker S.alice base0
        base2 = S.tapObject creature base1
        (gs, spellId) = S.handOne twofold base2
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) gs (S.cast S.alice spellId))
        enchanted = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        untapped = S.runPure S.identityAnswer enchanted (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
        after = S.settleSba untapped
    case attachedTo creature enchanted of
      [aura] -> do
        Spec.assertBool s (S.onBattlefield aura (S.settleSba enchanted)) "while the creature is tapped the Aura survives a pass"
        Spec.assertEqWith s "the untap step really untapped it (CR 502.3)" (S.tappedCount S.alice untapped) 0
        Spec.assertBool s (not (S.onBattlefield aura after)) "untapped, it is off the battlefield"
        Spec.assertEqWith s "and in its owner's graveyard, not destroyed" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        Spec.assertEqWith s "so the creature is a plain 2/1 again" (S.powerToughnessOf creature after) (Just (2, 1))
      _ -> Spec.assertFailure s "the Aura should have entered attached to alice's tapped Piker"
  -- The mirror image: the FIRST instance broken and the second untouched. Control
  -- Magic moves control without untapping anything, so "enchant tapped creature"
  -- still admits the host and "enchant creature you control" -- CR 109.5's "you"
  -- being the Aura's controller -- no longer does. An engine that read only the
  -- second instance would keep the Aura here.
  Spec.it s "CR 704.5m whole cards: Control Magic breaks the first instance while the host stays tapped" $ do
    plains <- S.printingOf s registry "Plains"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    twofold <- S.printingOf s registry "Synthetic Twofold Enchant"
    controlMagic <- S.printingOf s registry "Control Magic"
    -- {1}{W} for alice's Aura, {2}{U}{U} for bob's: S.landsInPlay seats alice's
    -- lands only, so bob's Islands go in one at a time.
    let base0 = S.landsInPlay plains 2
        (creature, base1) = S.addCreature piker S.alice base0
        base2 = S.tapObject creature base1
        (_, base3) = S.addCreature island S.bob base2
        (_, base4) = S.addCreature island S.bob base3
        (_, base5) = S.addCreature island S.bob base4
        (_, base6) = S.addCreature island S.bob base5
        (gs, spellId) = S.handOne twofold base6
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) gs (S.cast S.alice spellId))
        enchanted = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    case attachedTo creature enchanted of
      [aura] -> do
        let (stealId, withSteal) = S.addHandCard controlMagic S.bob enchanted
            ready = withSteal {GameState.priority = Just S.bob, GameState.activePlayer = S.bob}
            castSteal = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) ready (S.cast S.bob stealId))
            stolen = snd (Engine.runGamePure S.identityAnswer castSteal Stack.resolveTop)
            after = S.settleSba stolen
        Spec.assertEqWith s "the steal really moved control (CR 613.1b)" (Projection.controllerOf creature stolen) (Just S.bob)
        Spec.assertEqWith s "and left the creature tapped, so the second instance still holds" (fmap Object.tapped (Game.lookupObject creature after)) (Just TapState.Tapped)
        Spec.assertBool s (not (S.onBattlefield aura after)) "the two-enchant Aura is off the battlefield"
        Spec.assertEqWith s "in ALICE's graveyard, not the thief's" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
        Spec.assertEqWith s "exactly one Aura is left on the creature" (length (attachedTo creature after)) 1
        Spec.assertEqWith s "and it is Control Magic, whose enchant slot narrows nothing" (fmap (\oid -> Game.cardOf oid after) (attachedTo creature after)) [Just (Printing.card controlMagic)]
      _ -> Spec.assertFailure s "the Aura should have entered attached to alice's tapped Piker"

-- Replenish {3}{W} Sorcery -- "Return all enchantment cards from your graveyard to
-- the battlefield. (Auras with nothing to enchant remain in your graveyard.)" (name,
-- cost, type line and Oracle text checked against api.scryfall.com). The
-- parenthetical is reminder text (CR 207.2) restating CR 303.4g, so the card
-- transcribes only the first sentence and the rules do the rest.
--
-- The pool's first producer of an Aura entering the battlefield by any means other
-- than resolving as an Aura spell, so the first card to reach CR 303.4f's host
-- choice in Pawl.Engine.Event.changeZoneAttaching. Its effect is Rise of the Dark
-- Realms' shape one card type over.
replenishSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
replenishSpec s registry =
  let -- Eight Plains for a {3}{W} sorcery -- twice its cost, so a payment that taps
      -- one source at a time cannot fail for reasons of its own, on both boards
      -- below (the cast-gate vacuity trap).
      board plains replenish creatures buried =
        let withLands = S.landsFor plains S.alice 8 (Setup.emptyGame S.bothPlayers)
            step add (acc, g) printing = let (oid, g') = add printing S.alice g in (acc <> [oid], g')
            (creatureIds, withCreatures) = List.foldl' (step S.addCreature) ([], withLands) creatures
            (buriedIds, withBuried) = List.foldl' (step S.addGraveyardCard) ([], withCreatures) buried
            (ready, spell) = S.handOne replenish withBuried
         in (spell, creatureIds, buriedIds, ready)
      -- Cast, resolve, and take one CR 704.3 pass, so an Aura that entered
      -- unattached has actually been buried by CR 704.5m before anything looks. The
      -- responses come back beside the board, so one call answers both "what
      -- happened" and "who was asked".
      run :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Response.Response])
      run answer spell gs =
        let ((_, resolved), responses) = Replay.record answer gs (S.cast S.alice spell >> Stack.resolveTop)
         in (S.settleSba resolved, responses)
      hostsChosen responses = length [() | Response.ChoseAttachment _ <- responses]
      -- Which card is on this host, read through attachedTo: CR 400.7 minted a
      -- fresh id at the destination, so the Aura cannot be named by the id it was
      -- buried under.
      auraOn host gs = fmap (\oid -> Game.cardOf oid gs) (attachedTo host gs)
      copiesOn printing gs =
        length (filter (\oid -> Game.cardOf oid gs == Just (Printing.card printing)) (Set.toList (GameState.battlefield gs)))
      nth n candidates = case drop (n - 1) (NonEmpty.toList candidates) of
        x : _ -> x
        [] -> NonEmpty.head candidates
   in Spec.describe s "Replenish" $ do
        -- CR 303.4f. THREE creatures for TWO Auras, so the prompt cannot pass by
        -- short-circuiting (Attach.chooseHost elides at one candidate), and the two
        -- Auras are pinned to DIFFERENT candidates so neither answer can stand in
        -- for the other's.
        Spec.it s "CR 303.4f each returned Aura's controller chooses what it enchants" $ do
          plains <- S.printingOf s registry "Plains"
          replenish <- S.printingOf s registry "Replenish"
          unholy <- S.printingOf s registry "Unholy Strength"
          pacifism <- S.printingOf s registry "Pacifism"
          piker <- S.printingOf s registry "Goblin Piker"
          mammoth <- S.printingOf s registry "War Mammoth"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (spell, creatureIds, buriedIds, gs) = board plains replenish [piker, mammoth, maiden] [unholy, pacifism]
          case (creatureIds, buriedIds) of
            ([pikerId, mammothId, maidenId], [unholyId, pacifismId]) -> do
              -- Pinned BY THE SUBJECT the prompt names, which is the Aura's
              -- GRAVEYARD incarnation: CR 303.4f's choice is made as it enters, so
              -- before the CR 400.7 move. Pinned by INDEX, never by searching for a
              -- legal option, so no mutation can let the answerer repair the
              -- assertion. Attach.hostsFor sorts ascending, and the creatures went
              -- in in this order, so candidate 2 is the Mammoth and 3 the Maiden.
              let choosing :: Prompt.Prompt r -> r
                  choosing p = case p of
                    Prompt.ChooseAttachment _ _ subject offered
                      | subject == unholyId -> nth 2 offered
                      | subject == pacifismId -> nth 3 offered
                    _ -> S.castAnswer p
                  (after, responses) = run choosing spell gs
              Spec.assertEqWith s "both Auras' controllers were asked" (hostsChosen responses) 2
              Spec.assertEqWith s "Unholy Strength enchants the creature its own answer named" (auraOn mammothId after) [Just (Printing.card unholy)]
              Spec.assertEqWith s "and Pacifism the creature its answer named" (auraOn maidenId after) [Just (Printing.card pacifism)]
              Spec.assertEqWith s "nothing landed on the first candidate" (auraOn pikerId after) []
              -- The choice is made AS the Aura enters, so a state-based pass leaves
              -- both alone and the bonus is already applying -- which is what proves
              -- the attachment is real rather than a field nothing reads.
              Spec.assertEqWith s "the Piker is a plain 2/1" (S.powerToughnessOf pikerId after) (Just (2, 1))
              Spec.assertEqWith s "the Mammoth is 3/3 plus Unholy Strength's +2/+1" (S.powerToughnessOf mammothId after) (Just (5, 4))
              Spec.assertEqWith s "and Pacifism, which changes no characteristic, leaves the Maiden 1/2" (S.powerToughnessOf maidenId after) (Just (1, 2))
            _ -> Spec.assertFailure s "the board should hold three creatures and two graveyard cards"
        -- The paired control, and the whole reason there are three creatures: the
        -- same cast on the same board with an answerer that takes every FIRST
        -- candidate. If the engine were picking the host, the two legs could not
        -- disagree.
        Spec.it s "CR 303.4f the engine does not pick: another answer puts both Auras elsewhere" $ do
          plains <- S.printingOf s registry "Plains"
          replenish <- S.printingOf s registry "Replenish"
          unholy <- S.printingOf s registry "Unholy Strength"
          pacifism <- S.printingOf s registry "Pacifism"
          piker <- S.printingOf s registry "Goblin Piker"
          mammoth <- S.printingOf s registry "War Mammoth"
          maiden <- S.printingOf s registry "Bird Maiden"
          let (spell, creatureIds, _, gs) = board plains replenish [piker, mammoth, maiden] [unholy, pacifism]
          case creatureIds of
            [pikerId, mammothId, maidenId] -> do
              let takingFirst :: Prompt.Prompt r -> r
                  takingFirst p = case p of
                    Prompt.ChooseAttachment _ _ _ offered -> NonEmpty.head offered
                    _ -> S.castAnswer p
                  (after, responses) = run takingFirst spell gs
              Spec.assertEqWith s "both controllers were asked here too" (hostsChosen responses) 2
              Spec.assertEqWith s "both Auras went onto the first candidate" (List.sort (auraOn pikerId after)) (List.sort [Just (Printing.card unholy), Just (Printing.card pacifism)])
              Spec.assertEqWith s "the Mammoth has none" (auraOn mammothId after) []
              Spec.assertEqWith s "the Maiden has none" (auraOn maidenId after) []
              Spec.assertEqWith s "and the Piker carries the +2/+1 instead" (S.powerToughnessOf pikerId after) (Just (4, 2))
            _ -> Spec.assertFailure s "the board should hold three creatures"
        -- CR 303.4g's "remains in its current zone". ONE board, TWO enchantment
        -- cards: the non-Aura comes back and the Aura does not, so the same
        -- resolution proves the effect ran and that the Aura was left alone.
        --
        -- The DISCRIMINATOR is the ObjectId, not the zone. "Remains in the
        -- graveyard" and "was put into its owner's graveyard by CR 704.5m" are the
        -- same zone on this board, so no zone assertion can tell them apart. A
        -- completed move deletes the old id and mints a new one (CR 400.7), so an
        -- Aura that entered and was buried has NO object under its original id while
        -- one that never moved still does. CR 303.4g's other branch -- an Aura whose
        -- current zone is the STACK -- has no producer and is not asked here
        -- (gap #1734).
        Spec.it s "CR 303.4g an Aura with nothing to enchant never leaves the graveyard" $ do
          plains <- S.printingOf s registry "Plains"
          replenish <- S.printingOf s registry "Replenish"
          unholy <- S.printingOf s registry "Unholy Strength"
          scales <- S.printingOf s registry "Hardened Scales"
          -- NO creatures: Unholy Strength's enchant pool is Creatures, so its legal
          -- host set is empty and alice's Plains are the only permanents.
          let (spell, _, buriedIds, gs) = board plains replenish [] [unholy, scales]
          case buriedIds of
            [auraId, _] -> do
              let (after, responses) = run S.castAnswer spell gs
              Spec.assertEqWith s "nobody was asked -- there was nothing to choose" (hostsChosen responses) 0
              Spec.assertEqWith s "the Aura is the same object it always was, still in the graveyard" (fmap Object.zone (Game.lookupObject auraId after)) (Just Zone.Graveyard)
              Spec.assertEqWith s "and no Aura reached the battlefield" (copiesOn unholy after) 0
              Spec.assertEqWith s "while the non-Aura enchantment did come back, so the effect really ran" (copiesOn scales after) 1
            _ -> Spec.assertFailure s "the board should hold two graveyard cards"
        -- THE PROVING TEST for #1738, and the CR 303.4f half of it. CR 608.2f makes
        -- Replenish's return ONE event, so each Aura's host choice is made against
        -- the board the batch began on -- where a would-be host returned in the
        -- same batch is not on the battlefield at all. Master of the Feast is that
        -- would-be host: an enchantment card, so Replenish returns it, and a
        -- creature, so Unholy Strength's "enchant creature" would admit it.
        --
        -- NO creature on the battlefield, which is what makes the board
        -- discriminate: one already there would be a legal host under both
        -- readings, and every assertion below would pass either way.
        --
        -- TWO LEGS in the two sweep orders, because visibility of a sibling
        -- inherently depends on which member moved first: Resolve.graveyardCardsOf
        -- sorts ascending ObjectId and S.addGraveyardCard mints in call order, so
        -- the `buried` list pins the order exactly. The per-object reading answers
        -- differently in the two orders and CR 608.2f says it may not, so the legs
        -- must AGREE.
        --
        -- The prompt count is deliberately not the evidence: Attach.chooseHost
        -- elides at one candidate, so nobody is asked under either reading. The
        -- gameplay-level quantity is the host's power and toughness, and the zone
        -- is read through the Aura's ORIGINAL ObjectId -- "remains in the
        -- graveyard" (CR 303.4g) and "entered and was buried" (CR 704.5m) are the
        -- same zone, and only the surviving id separates them (CR 400.7), exactly
        -- as the case above argues.
        Spec.it s "CR 608.2f a returned Aura cannot enchant a permanent returned beside it (#1738)" $ do
          plains <- S.printingOf s registry "Plains"
          replenish <- S.printingOf s registry "Replenish"
          unholy <- S.printingOf s registry "Unholy Strength"
          master <- S.printingOf s registry "Master of the Feast"
          let outcome buried =
                let (spell, _, buriedIds, gs) = board plains replenish [] buried
                    (after, responses) = run S.castAnswer spell gs
                    -- The Aura's GRAVEYARD id, whichever slot of the batch it took.
                    auraId = Maybe.listToMaybe [oid | (oid, printing) <- zip buriedIds buried, Printing.card printing == Printing.card unholy]
                    -- By CARD: CR 400.7 minted a fresh id at the battlefield.
                    masterNew = List.find (\oid -> Game.cardOf oid after == Just (Printing.card master)) (Set.toList (GameState.battlefield after))
                 in ( fmap (`S.powerToughnessOf` after) masterNew,
                      fmap (\oid -> fmap Object.zone (Game.lookupObject oid after)) auraId,
                      copiesOn unholy after,
                      hostsChosen responses
                    )
              hostFirst = outcome [master, unholy]
              auraFirst = outcome [unholy, master]
          Spec.assertEqWith
            s
            "the host came back a plain 5/5, the Aura is the same object still in the graveyard, and nobody was asked"
            hostFirst
            (Just (Just (5, 5)), Just (Just Zone.Graveyard), 0, 0)
          Spec.assertEqWith
            s
            "and the batch's processing order changes nothing (CR 608.2f)"
            auraFirst
            hostFirst
        -- CR 708.2a against CR 303.4f, on Soul Summons rather than Replenish: an
        -- Aura card MANIFESTED off a library (CR 701.40a) enters as a 2/2 with no
        -- subtypes and no enchant ability, so it is not an Aura the rule speaks
        -- about and nobody is asked -- even though the card is an Aura in the zone
        -- it is leaving, which is where the gate's projection reads. It enters
        -- unattached, and CR 704.5m leaves it alone because Sba.fallsOff reads the
        -- same substituted face.
        --
        -- The Piker is the whole point: it is a legal host for a face-UP Unholy
        -- Strength, so a gate that read the projection alone would find it and
        -- attach.
        Spec.it s "CR 708.2a a manifested Aura card is not an Aura, so nobody chooses a host" $ do
          plains <- S.printingOf s registry "Plains"
          summons <- S.printingOf s registry "Soul Summons"
          unholy <- S.printingOf s registry "Unholy Strength"
          piker <- S.printingOf s registry "Goblin Piker"
          mammoth <- S.printingOf s registry "War Mammoth"
          let (host, base0) = S.addCreature piker S.alice (S.landsInPlay plains 2)
              -- TWO creatures, so Attach.chooseHost's one-candidate elision cannot
              -- make "nobody was asked" pass for the wrong reason.
              (host2, base1) = S.addCreature mammoth S.alice base0
              -- A second library card keeps CR 104.3c off the board; Unholy Strength
              -- goes on top of it, so the manifest reaches the Aura.
              (_, base2) = S.addLibraryCard piker S.alice base1
              (_, base3) = S.addLibraryCard unholy S.alice base2
              (gs, spell) = S.handOne summons base3
              (after, responses) = run S.castAnswer spell gs
              -- By CARD, since CR 400.7 minted a fresh id at the destination.
              manifested = filter (\oid -> Game.cardOf oid after == Just (Printing.card unholy)) (Set.toList (GameState.battlefield after))
          Spec.assertEqWith s "nobody was asked -- a face-down permanent has no enchant ability" (hostsChosen responses) 0
          Spec.assertEqWith s "and it did not land on the Piker, which would have hosted it face up" (auraOn host after) []
          Spec.assertEqWith s "nor on the Mammoth" (auraOn host2 after) []
          case manifested of
            [oid] -> do
              Spec.assertEqWith s "the manifested card is on the battlefield, unattached" (fmap Object.attachedTo (Game.lookupObject oid after)) (Just Nothing)
              Spec.assertEqWith s "CR 708.2a as a 2/2, not Unholy Strength" (S.powerToughnessOf oid after) (Just (2, 2))
            _ -> Spec.assertFailure s "the manifest should have put Unholy Strength onto the battlefield"

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Aura" $ do
  auraSpec s registry
  equipmentSpec s registry
  unattachableSpec s registry
  reattachSpec s registry
  arbitrationSpec s registry
  auraGraftSpec s registry
  miracleWorkerSpec s registry
  enchantPlayerSpec s registry
  chosenLandTypeSpec s registry
  twoEnchantSpec s registry
  replenishSpec s registry
  attachRestrictionSpec s registry
  couldEnchantSpec s registry

-- Both of Convincing Mirage's prompts at once: its CR 303.4a enchant slot
-- (Pool.Permanents narrowed to lands, so the recipient is tagged ToObject) and
-- its CR 614.1c as-enters basic land type. aimRecipient below answers only the
-- first, and the entry choice is the whole point of this card.
mirageOn :: ObjectId.ObjectId -> Subtype.Subtype -> Prompt.Prompt r -> r
mirageOn landId subtype p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject landId))) sets
  Prompt.ChooseBasicLandType {} -> subtype
  _ -> S.identityAnswer p

-- CR 614.1c's as-enters choice, whose value is a SUBTYPE rather than a colour,
-- and CR 305.7's set reading it back off the Aura. Convincing Mirage is the
-- pool's only producer of either. It enchants a non-creature OBJECT, which
-- Song of the Dryads and Consecrate Land also do -- where the two Curses
-- (enchantPlayerSpec below) enchant players, CR 702.5d's other shape, reaching
-- the battlefield through Affected.AttachedPlayerControls rather than
-- Affected.Attached.
chosenLandTypeSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
chosenLandTypeSpec s registry = Spec.describe s "ChosenLandType" $ do
  -- The gameplay-level proof design.md section 4 asks for: cast the Aura, answer
  -- its as-enters prompt for real, let it resolve, and see the enchanted land
  -- tap for the CHOSEN colour.
  --
  -- Run TWICE with different answers on one board. Once would only prove the
  -- choice was made; a modification that ignored Object.chosenSubtype and
  -- conjured a fixed type would still pass a single half. Two halves that
  -- disagree can only be told apart by reading the choice.
  Spec.it s "CR 614.1c whole card: Convincing Mirage makes a Mountain the chosen basic land type" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    convincingMirage <- S.printingOf s registry "Convincing Mirage"
    -- The Islands pay {1}{U}; the Mountain is the host, and is added last so it
    -- is never the head of a mana-source candidate list.
    let base0 = S.landsInPlay island 4
        (landId, base1) = S.addCreature mountain S.alice base0
        (withAura, auraSpell) = S.handOne convincingMirage base1
        run pick =
          let cast = snd (Engine.runGamePure (mirageOn landId pick) withAura (S.cast S.alice auraSpell))
           in snd (Engine.runGamePure (mirageOn landId pick) cast Stack.resolveTop)
        asIsland = run Subtype.Island
        asSwamp = run Subtype.Swamp
    Spec.assertEqWith s "before: a plain Mountain" (Projection.subtypesOf landId withAura) (Set.singleton Subtype.Mountain)
    Spec.assertBool s (ManaType.Colored Color.Red `elem` Mana.manaTypesOf landId withAura) "and it taps for red"
    -- CR 303.4: the Aura entered attached to what its enchant slot named, so it
    -- really did resolve -- without this a failed cast would look like a failed
    -- type change.
    Spec.assertBool s (not (null (attachedTo landId asIsland))) "the Aura entered attached to the land"
    -- CR 305.7: "the land no longer has its old land type". CR 305.6: a land
    -- with a basic land type has the intrinsic "{T}: Add [mana symbol]".
    Spec.assertEqWith s "choosing Island: only an Island" (Projection.subtypesOf landId asIsland) (Set.singleton Subtype.Island)
    Spec.assertBool s (ManaType.Colored Color.Blue `elem` Mana.manaTypesOf landId asIsland) "so it taps for blue"
    Spec.assertBool s (ManaType.Colored Color.Red `notElem` Mana.manaTypesOf landId asIsland) "and no longer for red"
    -- The same board, the other answer.
    Spec.assertEqWith s "choosing Swamp: only a Swamp" (Projection.subtypesOf landId asSwamp) (Set.singleton Subtype.Swamp)
    Spec.assertBool s (ManaType.Colored Color.Black `elem` Mana.manaTypesOf landId asSwamp) "so it taps for black"

-- Answers every target slot with one fixed recipient, deferring everything else
-- to S.identityAnswer. aimAt above does the same for a Pool.Permanents slot
-- only, because it hard-codes Recipient.ToObject; these cases mix a
-- Pool.Creatures slot (Unholy Strength's enchant) with a Pool.Permanents one
-- (Crown of the Ages' "target Aura"), and a recipient tagged for the wrong pool
-- is not in the legal set at all.
aimRecipient :: Recipient.Recipient -> Prompt.Prompt r -> r
aimRecipient recipient p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton recipient)) sets
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
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject aura))) sets
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
--
-- Compared through Recipient.objectOf rather than against a fixed tag, because
-- the tag is the enchant slot's, not the host's: a Pool.Creatures slot stores
-- ToCreature and Convincing Mirage's Pool.Permanents slot stores ToObject. This
-- read only wants to know WHICH object is named, which is exactly the question
-- Affected.Attached asks (Pawl.Engine.Projection.affects).
attachedTo :: ObjectId.ObjectId -> GameState.GameState -> [ObjectId.ObjectId]
attachedTo host gs =
  filter
    (\oid -> (Game.lookupObject oid gs >>= Object.attachedTo >>= Recipient.objectOf) == Just host)
    (Set.toList (GameState.battlefield gs))

-- The slot named "target" on a card's FIRST printed activated ability -- the
-- committed declaration, not a restatement of it, so a test asserting what it admits
-- is asserting what the card really says. Crown of the Ages and Miracle Worker are
-- the two callers, each printing exactly one such ability.
activatedTargetSlot :: Printing.Printing -> Maybe TargetSlot.TargetSlot
activatedTargetSlot printing = case Face.activatedAbilities (S.combinedFace printing) of
  ability : _ -> Map.lookup (SlotName.MkSlotName (Text.pack "target")) (Modal.allTargetSlots (ActivatedAbility.modal ability))
  [] -> Nothing

-- Answers Prompt.ChooseTargets by FILTERING the offered set down to the recipients
-- that name one object, rather than constructing a recipient of its own: a
-- hand-built Recipient.ToObject the pool never offered is dropped by CR 608.2b's
-- re-read at resolution with no error, which is how a targeting test goes green
-- and empty.
aimAtOffered :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtOffered oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter ((==) (Just oid) . Recipient.objectOf) . snd) sets
  _ -> S.identityAnswer p

-- CR 701.3 Attach, aimed at the effect's TARGET rather than at its source: an
-- opcode that moves an Aura already on the battlefield, which the Auras unit left
-- unbuilt. Crown of the Ages is the proving card -- "{4}, {T}: Attach target Aura
-- attached to a creature to another creature".
reattachSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
reattachSpec s registry = Spec.describe s "Reattach" $ do
  -- The gameplay-level proof design.md section 4 asks for: cast the Aura, cast
  -- the Crown, activate its printed ability through the real activation path,
  -- let it resolve, and see the bonus MOVE -- which is what "the Aura moved"
  -- means observably, not a field changing.
  Spec.it s "CR 701.3 whole card: Crown of the Ages moves Unholy Strength from the Piker to the Mammoth" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
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
        castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature first)) withAura (S.cast S.alice auraSpell))
        enchanted = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature first)) castAura Stack.resolveTop)
    case attachedTo first enchanted of
      [] -> Spec.assertFailure s "Unholy Strength should have entered attached to the Piker"
      aura : _ -> do
        let (withCrown, crownSpell) = S.handOne crown enchanted
            castCrown = snd (Engine.runGamePure S.identityAnswer withCrown (S.cast S.alice crownSpell))
            onBattlefield = snd (Engine.runGamePure S.identityAnswer castCrown Stack.resolveTop)
            crownId = case filter (\oid -> Game.cardOf oid onBattlefield == Just (Printing.card crown)) (Set.toList (GameState.battlefield onBattlefield)) of
              oid : _ -> Just oid
              [] -> Nothing
        case crownId of
          Nothing -> Spec.assertFailure s "Crown of the Ages should have resolved onto the battlefield"
          Just crownObj -> do
            let ability = case Face.activatedAbilities (S.combinedFace crown) of
                  ab : _ -> Just ab
                  [] -> Nothing
            case ability of
              Nothing -> Spec.assertFailure s "Crown of the Ages should print one activated ability"
              Just move -> do
                let ready = onBattlefield {GameState.priority = Just S.alice}
                    activated = snd (Engine.runGamePure (moveAura aura second) ready (Activate.activateAbility S.alice crownObj move))
                    after = snd (Engine.runGamePure (moveAura aura second) activated Stack.resolveTop)
                    settled = S.settleSba (S.settleSba after)
                Spec.assertEqWith s "before, the Piker is 2/1 + 2/+1" (S.powerToughnessOf first onBattlefield) (Just (4, 2))
                Spec.assertEqWith s "and the Mammoth is a plain 3/3" (S.powerToughnessOf second onBattlefield) (Just (3, 3))
                Spec.assertEqWith s "the Aura is attached to the Mammoth now" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature second)))
                Spec.assertEqWith s "so the Piker is back to 2/1" (S.powerToughnessOf first after) (Just (2, 1))
                Spec.assertEqWith s "and the Mammoth is 5/4" (S.powerToughnessOf second after) (Just (5, 4))
                Spec.assertEqWith s "the creature nobody chose is untouched" (S.powerToughnessOf decoy after) (Just (2, 1))
                -- CR 704.5m: the Aura landed on a legal host, so nothing buries it.
                Spec.assertBool s (Set.member aura (GameState.battlefield settled)) "the Aura survives the state-based actions"
                Spec.assertEqWith s "still on the Mammoth" (fmap Object.attachedTo (Game.lookupObject aura settled)) (Just (Just (Recipient.ToCreature second)))
  -- CR 303.4b through the target slot: "target Aura ATTACHED TO A CREATURE"
  -- is Pool.Permanents narrowed by `And [HasSubtype Aura, AttachedTo (HasCardType
  -- Creature)]`, so the narrowing has to do real work. The same Aura is offered when it
  -- sits on the Piker and withheld when it sits on a Mountain -- nothing
  -- about the Aura itself differs between the two boards, which is what makes
  -- the pair discriminating.
  --
  -- An Aura on a noncreature permanent is hand-built here, as the CR 704.5p
  -- land case above hand-builds its attachment: CR 704.5m would bury such an
  -- Aura on the next state-based-action pass, so no sequence of card plays
  -- leaves one standing for a player to look at.
  Spec.it s "CR 303.4b Crown of the Ages offers an Aura on a creature and not one on a land" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.alice base
        (land, g2) = S.addCreature mountain S.alice g1
        (aura, g3) = S.addCreature unholyStrength S.alice g2
        (crownObj, g4) = S.addCreature crown S.alice g3
        onCreature = S.attach aura creature g4
        onLand = S.attach aura land g4
        offered gs = fmap (\theSlot -> Target.legalRecipients (Just S.alice) crownObj theSlot gs) (activatedTargetSlot crown)
        admits oid gs = fmap (Set.member (Recipient.ToObject oid)) (offered gs)
    Spec.assertEqWith s "on the Piker the Aura is a legal target" (admits aura onCreature) (Just True)
    Spec.assertEqWith s "on the Mountain it is not" (admits aura onLand) (Just False)
    -- Not vacuous for a second reason: the slot rejects the Crown itself on
    -- the very board where it accepts the Aura.
    Spec.assertEqWith s "and the Crown is never a candidate" (admits crownObj onCreature) (Just False)
  -- CR 701.3c: "Attaching an Aura, Equipment, or Fortification on the
  -- battlefield to a different object or player causes [it] to receive a new
  -- timestamp." Feeds CR 613.7's layer ordering, so it is not cosmetic.
  Spec.it s "CR 701.3c moving an Aura to a different creature restamps it" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
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
              crownObj
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              (Effect.AttachTarget (AttachTarget.MkAttachTarget slot (Filter.Type.HasCardType CardType.Creature)))
        stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
    Spec.assertEqWith s "it moved" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature second)))
    Spec.assertBool s (stampOf after /= stampOf gs) "and was restamped"
  -- CR 701.3b, second sentence: "If an effect tries to attach an Aura,
  -- Equipment, or Fortification to the object or player it's already attached
  -- to, the effect does nothing." Crown of the Ages spells the same exclusion
  -- as "ANOTHER creature", so with no other creature on the battlefield there
  -- is nothing to choose and nothing happens -- in particular no restamp.
  Spec.it s "CR 701.3b with only its own host available the Aura does not move and is not restamped" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
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
              crownObj
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              (Effect.AttachTarget (AttachTarget.MkAttachTarget slot (Filter.Type.HasCardType CardType.Creature)))
        stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
    Spec.assertEqWith s "still on the Piker" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature first)))
    Spec.assertEqWith s "and not restamped" (stampOf after) (stampOf gs)
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
  Spec.it s "CR 303.4j attaching an Aura to something it cannot enchant leaves it where it was" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
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
              crownObj
              S.alice
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              (Map.singleton slot (Set.singleton (Recipient.ToObject aura)))
              -- `And []` matches everything, so the land is offered as a
              -- destination and CR 303.4j is what rejects it.
              (Effect.AttachTarget (AttachTarget.MkAttachTarget slot (Filter.Type.And [])))
        stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
        settled = S.settleSba (S.settleSba after)
    Spec.assertEqWith s "the Aura did not move onto the land" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature first)))
    Spec.assertEqWith s "and was not restamped" (stampOf after) (stampOf gs)
    Spec.assertEqWith s "the Piker still has the bonus" (S.powerToughnessOf first after) (Just (4, 2))
    -- CR 704.5m: a failed move must not leave the Aura in a state the
    -- state-based actions then punish -- it is still on a legal host.
    Spec.assertBool s (Set.member aura (GameState.battlefield settled)) "and the Aura is not buried afterwards"
  -- CR 303.4j through two printed cards. Setessan Training's "Enchant creature
  -- you control" is the first Face.enchant in the pool that narrows past
  -- "creature" (CR 702.5a: the enchant ability "restricts what an Aura spell can
  -- target and what an Aura can enchant"), and Crown of the Ages' destination
  -- filter is the bare "another creature" -- so the Crown really does offer a
  -- destination the Aura may not legally enchant, which is the situation CR
  -- 303.4j is about and which no pair of cards could produce before.
  --
  -- CR 109.5 fixes whose "you" that is: the AURA's controller, not the moving
  -- effect's. Pawl.Engine.Attach.attachmentFor asks Target.legalRecipients with
  -- Projection.controllerOf on the Aura for exactly that reason. Alice controls
  -- both cards here, so this board cannot tell the two readings apart -- nothing
  -- in the pool takes control of a noncreature artifact -- but attachmentFor is
  -- never handed the moving effect's source at all, so there is no second
  -- controller for it to read by mistake.
  --
  -- BOTH branches off one board and one activation, so the refusal cannot be
  -- the machinery declining to move anything: aimed at alice's own Mammoth the
  -- very same ability moves the Aura.
  Spec.it s "CR 303.4j whole cards: Crown of the Ages cannot move Setessan Training onto an opponent's creature" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    crown <- S.printingOf s registry "Crown of the Ages"
    -- {1}{G} for the Aura, {2} to cast the Crown, {4} to activate it.
    let base0 = S.landsInPlay forest 8
        (host, base1) = S.addCreature piker S.alice base0
        (mine, base2) = S.addCreature warMammoth S.alice base1
        (theirs, base3) = S.addCreature piker S.bob base2
        (withAura, auraSpell) = S.handOne setessanTraining base3
        castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature host)) withAura (S.cast S.alice auraSpell))
        enchanted = snd (Engine.runGamePure S.identityAnswer castAura Stack.resolveTop)
        (withCrown, crownSpell) = S.handOne crown enchanted
        castCrown = snd (Engine.runGamePure S.identityAnswer withCrown (S.cast S.alice crownSpell))
        settledIn = snd (Engine.runGamePure S.identityAnswer castCrown Stack.resolveTop)
        crowns = filter (\oid -> Game.cardOf oid settledIn == Just (Printing.card crown)) (Set.toList (GameState.battlefield settledIn))
    case (attachedTo host enchanted, crowns, Face.activatedAbilities (S.combinedFace crown)) of
      ([aura], [crownObj], [move]) -> do
        let ready = settledIn {GameState.priority = Just S.alice}
            run dest =
              let activated = snd (Engine.runGamePure (moveAura aura dest) ready (Activate.activateAbility S.alice crownObj move))
               in snd (Engine.runGamePure (moveAura aura dest) activated Stack.resolveTop)
            refused = run theirs
            moved = run mine
            stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
        Spec.assertEqWith s "before either activation the Piker is 2/1 + 1/+0" (S.powerToughnessOf host ready) (Just (3, 1))
        -- A FAILURE MODE, not a fizzle: the ability resolved, and the only
        -- thing that did not happen is the move.
        Spec.assertEqWith s "the Aura did not move onto bob's creature" (fmap Object.attachedTo (Game.lookupObject aura refused)) (Just (Just (Recipient.ToCreature host)))
        Spec.assertEqWith s "and was not restamped (CR 701.3c)" (stampOf refused) (stampOf ready)
        Spec.assertEqWith s "alice's Piker keeps the +1/+0" (S.powerToughnessOf host refused) (Just (3, 1))
        Spec.assertEqWith s "bob's Piker gains nothing" (S.powerToughnessOf theirs refused) (Just (2, 1))
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Trample theirs refused)) "and no trample"
        -- CR 704.5m: a refused move must not leave the Aura somewhere the
        -- state-based actions then punish.
        Spec.assertBool s (Set.member aura (GameState.battlefield (S.settleSba (S.settleSba refused)))) "the Aura survives the state-based actions"
        -- The control case, which is what stops the assertions above from
        -- passing for the wrong reason.
        Spec.assertEqWith s "onto a creature alice DOES control it moves" (fmap Object.attachedTo (Game.lookupObject aura moved)) (Just (Just (Recipient.ToCreature mine)))
        Spec.assertBool s (stampOf ready /= stampOf moved) "and was restamped"
        Spec.assertEqWith s "so the Mammoth is 3/3 + 1/+0" (S.powerToughnessOf mine moved) (Just (4, 3))
        Spec.assertBool s (Projection.hasKeyword Keyword.Trample mine moved) "with trample (CR 702.19)"
        Spec.assertEqWith s "and the Piker is a plain 2/1" (S.powerToughnessOf host moved) (Just (2, 1))
      _ -> Spec.assertFailure s "the fixture wanted one Aura on the Piker, one Crown on the battlefield, and one printed ability"

-- CR 303.4d's last two sentences, and CR 301.5c's, which are the same rule
-- written twice: "an Aura can't enchant more than one object or player. If a
-- spell or ability would cause an Aura to become attached to more than one
-- object or player, the Aura's controller chooses which object or player it
-- becomes attached to."
--
-- Synthetic Aura Diffusion is the producer -- "{T}: Attach target Aura attached
-- to a creature to each other permanent it can enchant" -- and it is synthetic
-- because these Scryfall queries turned up no printing that names more than one
-- destination:
-- `o:"attach" o:"to each"`, `o:"attached to each"`, `o:/attach[a-z]* .* to each/`,
-- `o:"attach" o:"to two"`, `o:"attach" o:"to any number of"` and
-- `o:"enchant more than one"`, 2026-08-19, no hit that attaches ONE permanent to
-- several. Unfinished Business would refute that -- it is the closest printed
-- shape, and it runs the other way, returning up to two Auras onto one creature.
--
-- WHAT IS ACTUALLY BEING PROVED is the chooser, since "pick one of N" is what
-- Attach.chooseHost already did for Crown of the Ages. So the Aura is BOB's and
-- the ability is ALICE's: under CR 608.2d alone alice would choose, and these two
-- rules take it away from her. Two seats is the minimum that can tell them
-- apart, and the answerer branches on the PlayerId the prompt names rather than
-- pinning one answer, so the two readings cannot collapse onto the same host.
arbitrationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
arbitrationSpec s registry =
  let -- The nth offered destination, one-based. Pinned BY INDEX rather than by
      -- searching for a legal option, so no mutation lets the answerer repair the
      -- assertion. Attach.hostsFor sorts ascending and the creatures go in in
      -- this order, so 1 is the Piker and 2 the Mammoth.
      nth n candidates = case drop (n - 1) (NonEmpty.toList candidates) of
        x : _ -> x
        [] -> NonEmpty.head candidates
      -- Bird Maiden 1/2 holds the Aura, Goblin Piker 2/1 and War Mammoth 3/3 are
      -- the two destinations, and Unholy Strength's +2/+1 lands on exactly one of
      -- them -- four distinct power/toughness pairs across the two readings, so
      -- no numeric coincidence can hide a wrong host.
      board maiden piker mammoth unholy diffusion =
        let base = Setup.emptyGame S.bothPlayers
            (host, g1) = S.addCreature maiden S.alice base
            (firstHost, g2) = S.addCreature piker S.alice g1
            (secondHost, g3) = S.addCreature mammoth S.alice g2
            (aura, g4) = S.addCreature unholy S.bob g3
            (artifact, g5) = S.addCreature diffusion S.alice g4
         in (host, firstHost, secondHost, aura, artifact, (S.attach aura host g5) {GameState.priority = Just S.alice})
      -- Targets the Aura, then routes the destination choice by WHO is asked.
      byChooser :: ObjectId.ObjectId -> Int -> Int -> Prompt.Prompt r -> r
      byChooser aura forBob forAlice p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToObject aura))) sets
        Prompt.ChooseAttachment _ chooser _ offered ->
          if chooser == S.bob then nth forBob offered else nth forAlice offered
        _ -> S.identityAnswer p
   in Spec.describe s "Arbitration" $ do
        Spec.it s "CR 303.4d the AURA's controller chooses which of the named hosts it keeps" $ do
          maiden <- S.printingOf s registry "Bird Maiden"
          piker <- S.printingOf s registry "Goblin Piker"
          mammoth <- S.printingOf s registry "War Mammoth"
          unholy <- S.printingOf s registry "Unholy Strength"
          diffusion <- S.printingOf s registry "Synthetic Aura Diffusion"
          let (host, firstHost, secondHost, aura, artifact, gs) = board maiden piker mammoth unholy diffusion
          case Face.activatedAbilities (S.combinedFace diffusion) of
            [] -> Spec.assertFailure s "Synthetic Aura Diffusion should print one activated ability"
            ability : _ -> do
              -- bob takes the SECOND destination, alice the first. Alice's answer
              -- is live on this board: it is what a CR 608.2d-only reading would use.
              let activated = S.runPure (byChooser aura 2 1) gs (Activate.activateAbility S.alice artifact ability)
                  after = S.runPure (byChooser aura 2 1) activated Stack.resolveTop
                  settled = S.settleSba (S.settleSba after)
              Spec.assertEqWith s "the Mammoth carries Unholy Strength's +2/+1, which is bob's answer" (S.powerToughnessOf secondHost after) (Just (5, 4))
              Spec.assertEqWith s "the Piker, which alice's answer named, is a plain 2/1" (S.powerToughnessOf firstHost after) (Just (2, 1))
              Spec.assertEqWith s "and the old host is back to 1/2" (S.powerToughnessOf host after) (Just (1, 2))
              Spec.assertEqWith s "and the Aura's one host is the creature bob named" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature secondHost)))
              -- CR 704.5m: it landed on a legal host, so nothing bins it, and CR
              -- 303.4e leaves it bob's on alice's creature.
              Spec.assertBool s (Set.member aura (GameState.battlefield settled)) "the Aura survives the state-based actions"
              Spec.assertEqWith s "still bob's" (Projection.controllerOf aura settled) (Just S.bob)
        -- The paired control, differing in ONE thing: bob's answer. If the engine
        -- were picking the host, the two legs could not disagree. Alice's answer
        -- is the other index in each leg, so a mutation that hands her the choice
        -- swaps both outcomes and reddens both cases rather than neither.
        Spec.it s "CR 303.4d another answer from the same player puts it elsewhere" $ do
          maiden <- S.printingOf s registry "Bird Maiden"
          piker <- S.printingOf s registry "Goblin Piker"
          mammoth <- S.printingOf s registry "War Mammoth"
          unholy <- S.printingOf s registry "Unholy Strength"
          diffusion <- S.printingOf s registry "Synthetic Aura Diffusion"
          let (host, firstHost, secondHost, aura, artifact, gs) = board maiden piker mammoth unholy diffusion
          case Face.activatedAbilities (S.combinedFace diffusion) of
            [] -> Spec.assertFailure s "Synthetic Aura Diffusion should print one activated ability"
            ability : _ -> do
              let activated = S.runPure (byChooser aura 1 2) gs (Activate.activateAbility S.alice artifact ability)
                  after = S.runPure (byChooser aura 1 2) activated Stack.resolveTop
              Spec.assertEqWith s "the Piker carries the +2/+1 now" (S.powerToughnessOf firstHost after) (Just (4, 2))
              Spec.assertEqWith s "and the Mammoth is a plain 3/3" (S.powerToughnessOf secondHost after) (Just (3, 3))
              Spec.assertEqWith s "the old host is still 1/2" (S.powerToughnessOf host after) (Just (1, 2))
              Spec.assertEqWith s "one host, not two" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature firstHost)))

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
auraGraftSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraGraftSpec s registry = Spec.describe s "AuraGraft" $ do
  -- The gameplay-level proof design.md section 4 asks for, and the one the
  -- control clause exists for: CR 303.4e says an Aura's controller is separate
  -- from the enchanted object's, so gaining the Aura and moving it are two
  -- changes -- and Control Magic's own static ability then hands the NEW host
  -- to the Aura's new controller. Three permanents change hands off one spell:
  -- alice takes the Aura, takes the Mammoth it lands on, and gets her own
  -- Piker back because the Aura left it.
  Spec.it s "CR 303.4e whole cards: Aura Graft takes bob's Control Magic and the creature it moves onto" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    controlMagic <- S.printingOf s registry "Control Magic"
    auraGraft <- S.printingOf s registry "Aura Graft"
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
        cast = snd (Engine.runGamePure (moveAura aura prize) gs (S.cast S.alice graft))
        after = snd (Engine.runGamePure (moveAura aura prize) cast Stack.resolveTop)
        settled = S.settleSba (S.settleSba after)
    Spec.assertEqWith s "before: bob's Control Magic holds alice's Piker" (Projection.controllerOf host stolen) (Just S.bob)
    Spec.assertEqWith s "before: bob controls the Mammoth too" (Projection.controllerOf prize stolen) (Just S.bob)
    Spec.assertEqWith s "before: and the Aura itself" (Projection.controllerOf aura stolen) (Just S.bob)
    -- CR 303.4e: the Aura's controller changed, and it is the thing the spell
    -- gained -- not the permanent it was attached to.
    Spec.assertEqWith s "alice controls the Aura now" (Projection.controllerOf aura after) (Just S.alice)
    Spec.assertEqWith s "and it sits on the Mammoth (CR 701.3a)" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature prize)))
    -- CR 613.1b: Control Magic's SetControllerToSource reads the AURA's
    -- controller, so the creature follows the Aura to alice.
    Spec.assertEqWith s "so alice controls the Mammoth" (Projection.controllerOf prize after) (Just S.alice)
    Spec.assertEqWith s "and her own Piker is hers again" (Projection.controllerOf host after) (Just S.alice)
    Spec.assertEqWith s "the creature nobody chose stays bob's" (Projection.controllerOf decoy after) (Just S.bob)
    -- CR 302.6: alice has not controlled the Mammoth continuously since her
    -- turn began. Nothing re-Sicks it -- the control came from a static
    -- ability, not from the GainControl opcode -- but Object.sickness names
    -- the player it settled under, so the answer is right anyway.
    Spec.assertBool s (not (Combat.canAttack S.alice prize after)) "the Mammoth cannot attack for her yet"
    -- CR 302.6 speaks only of creatures, so the re-Sick the GainControl arm
    -- applies to the Aura is inert -- pinned here because an enchantment is the
    -- first thing that opcode has been aimed at.
    Spec.assertEqWith s "the Aura is re-Sicked all the same" (fmap Object.sickness (Game.lookupObject aura after)) (Just Sickness.Sick)
    -- CR 704.5m: it landed on a creature, which its enchant ability admits.
    Spec.assertBool s (Set.member aura (GameState.battlefield settled)) "and it survives the state-based actions"
  -- "target Aura THAT'S ATTACHED TO A PERMANENT" (CR 601.2c), spelled
  -- `And [HasSubtype Aura, AttachedTo (And [])]` -- the TRIVIAL nest, which is
  -- what makes "attached to a permanent" fall out of the general atom: the host
  -- view is filled only for an object on the battlefield (CR 110.1), so the nest
  -- has nothing left to say. Three boundaries at once,
  -- all four Auras on one board: an Aura on a noncreature permanent is in --
  -- which is what makes the trivial nest wider than the Crown's
  -- `AttachedTo (HasCardType Creature)` --
  -- an Aura on a PLAYER is out (CR 303.4: an Aura is attached to "an object or
  -- player", and only one of those is a permanent), and an unattached one is
  -- out.
  --
  -- The Aura on a land is hand-built, as the Crown's own filter test above
  -- hand-builds one: CR 704.5m would bury it on the next state-based-action
  -- pass, so no sequence of card plays leaves one standing to be targeted.
  Spec.it s "CR 601.2c Aura Graft targets an Aura on any permanent, where Crown of the Ages needs one on a creature" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    curse <- S.printingOf s registry "Curse of Death's Hold"
    crown <- S.printingOf s registry "Crown of the Ages"
    auraGraft <- S.printingOf s registry "Aura Graft"
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
        graftOffers oid = fmap (Set.member (Recipient.ToObject oid) . (\theSlot -> Target.legalRecipients (Just S.alice) graft theSlot attached)) (S.spellTargetSlot auraGraft)
        crownOffers oid = fmap (Set.member (Recipient.ToObject oid) . (\theSlot -> Target.legalRecipients (Just S.alice) crownId theSlot attached)) (activatedTargetSlot crown)
    Spec.assertEqWith s "an Aura on a creature is a legal target" (graftOffers onCreature) (Just True)
    Spec.assertEqWith s "and so is one on a land" (graftOffers onLand) (Just True)
    -- The pair that makes the trivial nest do work the Crown's creature nest
    -- cannot: one board, one Aura, two cards, two answers.
    Spec.assertEqWith s "which Crown of the Ages will not have" (crownOffers onLand) (Just False)
    Spec.assertEqWith s "an Aura on a PLAYER is not attached to a permanent" (graftOffers onPlayer) (Just False)
    Spec.assertEqWith s "nor is an unattached one" (graftOffers loose) (Just False)
    -- Not vacuous: the Graft's own slot rejects a permanent that is not an Aura
    -- at all on the very board where it accepts three that are.
    Spec.assertEqWith s "and a plain creature is not an Aura" (graftOffers creature) (Just False)
  -- The window CR 704.3 leaves open between a host leaving and the pass that
  -- buries the Aura (CR 704.5m): the attachment is still recorded, and it
  -- still names an object, but that object is no longer on the battlefield --
  -- and CR 110.1 makes only a battlefield object a permanent. So "attached to
  -- a permanent" is False, and the atom has to read the battlefield rather
  -- than stopping at the stored recipient.
  --
  -- Not reachable while a player holds priority, because CR 704.3 runs the
  -- pass first -- which is exactly why the state has to be built by hand here.
  Spec.it s "CR 110.1 an Aura whose host has left the battlefield is not attached to a permanent" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    auraGraft <- S.printingOf s registry "Aura Graft"
    let base = Setup.emptyGame S.bothPlayers
        (creature, g1) = S.addCreature piker S.alice base
        (aura, g2) = S.addCreature unholyStrength S.alice g1
        (gs, graft) = S.handOne auraGraft (S.attach aura creature g2)
        bounced = S.runPure S.identityAnswer gs (Event.changeZone creature Zone.Hand)
        offers g = fmap (Set.member (Recipient.ToObject aura) . (\theSlot -> Target.legalRecipients (Just S.alice) graft theSlot g)) (S.spellTargetSlot auraGraft)
    Spec.assertEqWith s "while the Piker is there the Aura is a legal target" (offers gs) (Just True)
    -- CR 400.7 minted a new object in the hand, so the recipient the Aura
    -- still holds names an id nothing on the battlefield answers to.
    Spec.assertBool s (Maybe.isJust (Game.lookupObject aura bounced >>= Object.attachedTo)) "the Aura is still attached to the old id"
    Spec.assertEqWith s "but it is no longer attached to a PERMANENT" (offers bounced) (Just False)
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
  Spec.it s "the destination filter offers only a permanent the Aura can enchant, so a Mountain is never a destination" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    auraGraft <- S.printingOf s registry "Aura Graft"
    -- Gatherer, 2007-07-15: "You can target an Aura you already control just to
    -- move that Aura to a new permanent."
    let base0 = S.landsInPlay island 2
        (host, base1) = S.addCreature piker S.alice base0
        (land, base2) = S.addCreature mountain S.alice base1
        (elsewhere, base3) = S.addCreature warMammoth S.alice base2
        (aura, base4) = S.addCreature unholyStrength S.alice base3
        onPiker = S.attach aura host base4
        (gs, graft) = S.handOne auraGraft onPiker
        cast = snd (Engine.runGamePure (moveAura aura land) gs (S.cast S.alice graft))
        after = snd (Engine.runGamePure (moveAura aura land) cast Stack.resolveTop)
    Spec.assertEqWith s "before, the Piker carries the +2/+1" (S.powerToughnessOf host onPiker) (Just (4, 2))
    Spec.assertEqWith s "the Aura went to the Mammoth, not the Mountain" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature elsewhere)))
    Spec.assertEqWith s "so the Mammoth is 3/3 + 2/+1" (S.powerToughnessOf elsewhere after) (Just (5, 4))
    Spec.assertEqWith s "and the Piker is a plain 2/1" (S.powerToughnessOf host after) (Just (2, 1))
  -- CR 608.2c: "the spell or ability's controller follows its instructions in
  -- the order written". The control change is the FIRST instruction, so by the
  -- time the destination is chosen the Aura is alice's -- and CR 109.5 makes
  -- the "you" in Setessan Training's "Enchant creature you control" the Aura's
  -- controller, which is now her.
  --
  -- Discriminating with no prompt at all: exactly one destination is legal
  -- under each reading of the order, and they are different creatures. Run the
  -- attach first and the Aura lands on bob's Mammoth.
  Spec.it s "CR 608.2c the control change lands before the destination is chosen, so 'creature you control' means alice's" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    auraGraft <- S.printingOf s registry "Aura Graft"
    let base0 = S.landsInPlay island 2
        (host, base1) = S.addCreature piker S.bob base0
        (his, base2) = S.addCreature warMammoth S.bob base1
        (hers, base3) = S.addCreature piker S.alice base2
        (aura, base4) = S.addCreature setessanTraining S.bob base3
        onHis = S.attach aura host base4
        (gs, graft) = S.handOne auraGraft onHis
        cast = snd (Engine.runGamePure (moveAura aura his) gs (S.cast S.alice graft))
        after = snd (Engine.runGamePure (moveAura aura his) cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls the Aura" (Projection.controllerOf aura after) (Just S.alice)
    Spec.assertEqWith s "so it moved to HER creature" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature hers)))
    Spec.assertEqWith s "which now has +1/+0" (S.powerToughnessOf hers after) (Just (3, 1))
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample hers after) "and trample"
    Spec.assertEqWith s "bob's Mammoth was never a legal destination" (S.powerToughnessOf his after) (Just (3, 3))
  -- Gatherer, 2004-10-04: "If there is no legal place to move the enchantment,
  -- then it doesn't move but you still control it." CR 609.3 -- "if an effect
  -- attempts to do something impossible, it does only as much as possible" --
  -- for the move half, with the control half unaffected, so the two clauses
  -- really are independent, which is the point of CR 303.4e's "an Aura's
  -- controller is separate".
  Spec.it s "with no permanent it can enchant the Aura does not move, and alice still gains it" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    auraGraft <- S.printingOf s registry "Aura Graft"
    let base0 = S.landsInPlay island 2
        (host, base1) = S.addCreature piker S.alice base0
        (_, base2) = S.addCreature mountain S.alice base1
        (aura, base3) = S.addCreature controlMagic S.bob base2
        stolen = S.attach aura host base3
        (gs, graft) = S.handOne auraGraft stolen
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject aura)) gs (S.cast S.alice graft))
        after = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject aura)) cast Stack.resolveTop)
        stampOf g = fmap Object.timestamp (Game.lookupObject aura g)
    Spec.assertEqWith s "it is still on the only creature there is" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature host)))
    -- CR 701.3c restamps only a permanent that actually moved.
    Spec.assertEqWith s "and was not restamped" (stampOf after) (stampOf stolen)
    Spec.assertEqWith s "but alice controls it" (Projection.controllerOf aura after) (Just S.alice)
    Spec.assertEqWith s "so the creature it holds is hers" (Projection.controllerOf host after) (Just S.alice)

-- Miracle Worker, "{T}: Destroy target Aura attached to a creature you control",
-- which is the first card in the pool whose attachment narrowing COMPOSES with a
-- quality: `And [HasSubtype Aura, AttachedTo (And [HasCardType Creature,
-- ControlledBy You])]`. No nullary atom expresses it, which is what the general
-- Filter.AttachedTo is for.
--
-- CR 109.5 is the rule the composition rests on: "you" inside the nest is the
-- player who ACTIVATED the ability, not the host's own controller and not the
-- Aura's, so the nest is evaluated against the host's view and the OUTER context.
miracleWorkerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
miracleWorkerSpec s registry = Spec.describe s "MiracleWorker" $ do
  -- The target slot, read off the printed ability. BOTH Auras are alice's and both
  -- hosts are creatures, so the only axis that varies is the HOST's controller --
  -- which is what rules out the misreading "an Aura YOU control", under which the
  -- two answers would agree. Crown of the Ages asks the same question without the
  -- control conjunct and offers what the Worker refuses, on the same board, which
  -- is what rules out a negative passing for an unrelated legality.
  --
  -- The Aura on a land is hand-built, as the Crown's and the Graft's own filter
  -- tests hand-build theirs: CR 704.5m would bury it on the next
  -- state-based-action pass.
  Spec.it s "CR 109.5 Miracle Worker offers an Aura on your creature and not one on theirs" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    crown <- S.printingOf s registry "Crown of the Ages"
    worker <- S.printingOf s registry "Miracle Worker"
    let base = Setup.emptyGame S.bothPlayers
        (mine, g1) = S.addCreature piker S.alice base
        (theirs, g2) = S.addCreature piker S.bob g1
        (myLand, g3) = S.addCreature mountain S.alice g2
        (onMine, g4) = S.addCreature unholyStrength S.alice g3
        (onTheirs, g5) = S.addCreature unholyStrength S.alice g4
        (onLand, g6) = S.addCreature unholyStrength S.alice g5
        (loose, g7) = S.addCreature unholyStrength S.alice g6
        (workerId, g8) = S.addCreature worker S.alice g7
        (crownId, g9) = S.addCreature crown S.alice g8
        board =
          S.attach onLand myLand $
            S.attach onTheirs theirs (S.attach onMine mine g9)
        offers source slotOf oid =
          fmap
            (Set.member (Recipient.ToObject oid) . (\theSlot -> Target.legalRecipients (Just S.alice) source theSlot board))
            slotOf
        workerOffers = offers workerId (activatedTargetSlot worker)
        crownOffers = offers crownId (activatedTargetSlot crown)
    Spec.assertEqWith s "an Aura on alice's creature is a legal target" (workerOffers onMine) (Just True)
    Spec.assertEqWith s "one on bob's creature is not" (workerOffers onTheirs) (Just False)
    -- The pair that makes the composition do work no nullary atom can: one board,
    -- one Aura, two cards, two answers.
    Spec.assertEqWith s "which Crown of the Ages, asking only about creature-ness, will offer" (crownOffers onTheirs) (Just True)
    -- The CARD TYPE conjunct, on a host alice does control -- so this negative
    -- cannot be the control conjunct answering again.
    Spec.assertEqWith s "an Aura on alice's LAND is not on a creature" (workerOffers onLand) (Just False)
    Spec.assertEqWith s "nor is an unattached Aura attached to anything" (workerOffers loose) (Just False)
    -- Not vacuous: the slot rejects a permanent that is not an Aura at all on the
    -- very board where it accepts one that is.
    Spec.assertEqWith s "and the Worker is not an Aura" (workerOffers workerId) (Just False)
  -- design.md section 4's whole-card proof: activate the printed ability through
  -- the real activation path (CR 602.2a), let it resolve, and see the Aura go to
  -- its owner's graveyard (CR 701.8a). The Aura on bob's creature is the control:
  -- it sits on the same board and is untouched.
  Spec.it s "CR 701.8a whole card: Miracle Worker destroys the Aura on her own creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    worker <- S.printingOf s registry "Miracle Worker"
    let base = Setup.emptyGame S.bothPlayers
        (mine, g1) = S.addCreature piker S.alice base
        (theirs, g2) = S.addCreature piker S.bob g1
        (onMine, g3) = S.addCreature unholyStrength S.alice g2
        (onTheirs, g4) = S.addCreature unholyStrength S.alice g3
        (workerId, g5) = S.addCreature worker S.alice g4
        board = (S.attach onTheirs theirs (S.attach onMine mine g5)) {GameState.priority = Just S.alice}
    case Face.activatedAbilities (S.combinedFace worker) of
      [] -> Spec.assertFailure s "Miracle Worker should print one activated ability"
      ability : _ -> do
        let activated = snd (Engine.runGamePure (aimAtOffered onMine) board (Activate.activateAbility S.alice workerId ability))
            after = snd (Engine.runGamePure (aimAtOffered onMine) activated Stack.resolveTop)
            settled = S.settleSba (S.settleSba after)
        Spec.assertBool s (not (Set.member onMine (GameState.battlefield settled))) "the Aura on her creature is destroyed"
        Spec.assertBool s (Set.member onTheirs (GameState.battlefield settled)) "the one on bob's creature is untouched"
        -- The cost was really paid rather than the activation silently failing.
        Spec.assertEqWith s "and the Worker is tapped" (fmap Object.tapped (Game.lookupObject workerId settled)) (Just TapState.Tapped)

auraSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
auraSpec s registry = Spec.describe s "Aura" $ do
  Spec.it s "CR 303.4: a resolving Aura spell enters the battlefield attached to its target" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = S.landsInPlay swamp 1
        (creature, withCreature) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne unholyStrength withCreature
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        auras = filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after))
    Spec.assertEqWith s "one attached permanent on the battlefield" (length auras) 1
    Spec.assertEqWith s "attached to the creature" (fmap Object.attachedTo auras) [Just (Recipient.ToCreature creature)]
    Spec.assertEqWith s "the creature is a 4/2" (S.powerToughnessOf creature after) (Just (4, 2))
  -- CR 608.2b: an Aura spell is the first PERMANENT spell in this pool that
  -- can be countered on resolution. Before this task, Stack sent every
  -- permanent spell to the battlefield with no target check at all.
  Spec.it s "CR 608.2b: an Aura spell whose target left is countered on resolution" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = S.landsInPlay swamp 1
        (creature, withCreature) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne unholyStrength withCreature
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        -- The target leaves in response, so no legal target remains at resolution.
        bounced = S.runPure S.identityAnswer cast (Event.changeZone creature Zone.Hand)
        after = snd (Engine.runGamePure S.identityAnswer bounced Stack.resolveTop)
    Spec.assertEqWith s "nothing attached on the battlefield" (filter (\o -> Object.zone o == Zone.Battlefield && Maybe.isJust (Object.attachedTo o)) (Map.elems (GameState.objects after))) []
    Spec.assertEqWith s "the Aura is in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  -- CR 704.5m, and CR 704.3's repeat. SBAs are simultaneous, so the pass that
  -- buries the creature judged the Aura against a state in which that creature was
  -- still there; the Aura falls off on the NEXT pass. Asserting both passes is the
  -- point -- an implementation that dropped the Aura in pass one would be reading
  -- post-pass state, which is what CR 704.3's "simultaneously" forbids.
  Spec.it s "CR 704.5m: an Aura whose creature died falls off on the next SBA pass" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
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
    Spec.assertEqWith s "the creature is gone after pass one" (Game.lookupObject creature pass1) Nothing
    Spec.assertBool s (Set.member aura (GameState.battlefield pass1)) "the Aura is still on the battlefield after pass one"
    Spec.assertBool s (not (Set.member aura (GameState.battlefield pass2))) "the Aura is gone from the battlefield after pass two"
    Spec.assertEqWith s "and is in its OWNER's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice pass2)) 1
  -- CR 704.5m's remaining clause: unattached. Its third clause -- attached to an
  -- object the enchant slot no longer admits (CR 303.4c) -- is reached two ways:
  -- by a CONTROL change (the Control Magic and Setessan Training case below), and
  -- by the host ceasing to be a creature at all, which is the pair of cases
  -- immediately after this one.
  Spec.it s "CR 704.5m: an unattached Aura on the battlefield goes to the graveyard" $ do
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    let base = Setup.emptyGame S.bothPlayers
        (aura, gs) = S.addCreature unholyStrength S.alice base
        after = S.settleSba gs
    Spec.assertBool s (not (Set.member aura (GameState.battlefield after))) "never attached, so it falls off immediately"
  -- CR 303.4c through CR 704.5m, with the illegality coming from a layer-4 card
  -- type SET rather than from a control change or a death: Song of the Dryads
  -- makes its host a colorless Forest land (CR 205.1a), and Unholy Strength's
  -- "Enchant creature" no longer admits it.
  --
  -- A gameplay-level reader for the set, which is the point of running it through
  -- a state-based action rather than reading the projection: the enchant filter
  -- asks CR 205's question about the host, and the answer moves a card between
  -- zones. An implementation that ADDED the land type would leave the host a
  -- creature and Unholy Strength where it is.
  --
  -- The pair differs in exactly one thing: which permanent Song of the Dryads is
  -- attached to. Both boards carry the same two Auras, the same Piker and the same
  -- Forest, and in both the Song itself stays -- its own "enchant permanent"
  -- admits a land as readily as a creature, so neither leg can pass by the Song
  -- falling off instead.
  Spec.it s "CR 704.5m / 205.1a: a host that stops being a creature buries the Aura enchanting it" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    unholyStrength <- S.printingOf s registry "Unholy Strength"
    song <- S.printingOf s registry "Song of the Dryads"
    let base = S.landsInPlay forest 1
        landId = case Game.zoneMembers Zone.Battlefield S.alice base of
          i : _ -> i
          [] -> ObjectId.MkObjectId 999
        (creature, withCreature) = S.addCreature piker S.alice base
        (aura, withAura) = S.addCreature unholyStrength S.alice withCreature
        (songId, withSong) = S.addCreature song S.alice (S.attach aura creature withAura)
        -- ToObject rather than S.attach's ToCreature: "enchant permanent" is a
        -- Pool.Permanents slot, so those are the recipients casting would have
        -- left, and CR 704.5m's re-check compares against exactly those.
        songOn host = S.settleSba (S.attachTo songId (Recipient.ToObject host) withSong)
        onCreature = songOn creature
        onLand = songOn landId
    Spec.assertBool s (not (Projection.isCreatureOf creature onCreature)) "the Song made the Piker a land, so it is no longer a creature"
    Spec.assertBool s (not (Set.member aura (GameState.battlefield onCreature))) "and Unholy Strength, illegally attached, was buried"
    Spec.assertEqWith s "in its owner's graveyard" (length (Game.zoneMembers Zone.Graveyard S.alice onCreature)) 1
    Spec.assertBool s (Set.member songId (GameState.battlefield onCreature)) "the Song itself stays: enchant permanent admits a land"
    Spec.assertBool s (Set.member creature (GameState.battlefield onCreature)) "and the host is still on the battlefield -- it stopped being a creature, it did not die"
    Spec.assertBool s (Projection.isCreatureOf creature onLand) "the control: with the Song elsewhere the Piker is still a creature"
    Spec.assertBool s (Set.member aura (GameState.battlefield onLand)) "so Unholy Strength stays attached"
  -- Setessan Training's own three lines, at gameplay level (design.md section 4):
  -- "Enchant creature you control" (CR 702.5a) narrowing the enchant slot, "When
  -- this Aura enters, draw a card" firing, and "+1/+0 and has trample" (CR
  -- 702.19) landing on the enchanted creature.
  --
  -- The first pair of assertions is the one the filter exists for: CR 109.5's
  -- "you" on an enchant ability is the Aura's would-be controller while the
  -- spell is being cast, so alice's creature is offered and bob's is withheld.
  -- The enchant slot is read out of the committed card, never hand-built.
  Spec.it s "CR 702.5a whole card: Setessan Training enchants only its caster's creature, draws, and grants +1/+0 and trample" $ do
    forest <- S.printingOf s registry "Forest"
    piker <- S.printingOf s registry "Goblin Piker"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    let base0 = S.landsInPlay forest 2
        (mine, base1) = S.addCreature piker S.alice base0
        (theirs, base2) = S.addCreature piker S.bob base1
        -- Something to draw, so the trigger is observable rather than an
        -- attempted draw from an empty library (CR 704.5b).
        (_, base3) = S.addLibraryCard forest S.alice base2
        (gs, spellId) = S.handOne setessanTraining base3
        offered = fmap (\theSlot -> Target.legalRecipients (Just S.alice) spellId theSlot gs) (Card.enchantTargetSlot (S.combinedFace setessanTraining))
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature mine)) gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
        -- CR 704.3: the enters trigger waits until a player would get priority,
        -- which resolveTop alone never reaches.
        placed = snd (Engine.runGamePure S.identityAnswer after Engine.placePendingTriggers)
        drawn = snd (Engine.runGamePure S.identityAnswer placed Stack.resolveTop)
    Spec.assertEqWith s "alice's own creature is a legal host" (fmap (Set.member (Recipient.ToCreature mine)) offered) (Just True)
    Spec.assertEqWith s "bob's is not" (fmap (Set.member (Recipient.ToCreature theirs)) offered) (Just False)
    Spec.assertEqWith s "exactly one permanent entered attached to alice's creature" (length (attachedTo mine after)) 1
    Spec.assertEqWith s "and nothing is attached to bob's" (length (attachedTo theirs after)) 0
    Spec.assertEqWith s "the enchanted creature is a 2/1 plus 1/+0" (S.powerToughnessOf mine after) (Just (3, 1))
    Spec.assertBool s (Projection.hasKeyword Keyword.Trample mine after) "and has trample"
    Spec.assertBool s (not (Projection.hasKeyword Keyword.Trample theirs after)) "bob's Piker has neither"
    Spec.assertEqWith s "bob's Piker is still 2/1" (S.powerToughnessOf theirs after) (Just (2, 1))
    Spec.assertEqWith s "the cast emptied alice's hand" (S.handSize S.alice after) 0
    Spec.assertEqWith s "and the enters trigger refills it" (S.handSize S.alice drawn) 1
  -- CR 704.5m's third clause -- "attached to an illegal object ... as defined by
  -- its enchant ability and other applicable effects" (CR 303.4c) -- reached
  -- without touching the host's card types: Setessan Training says "enchant
  -- creature you control", so an opponent STEALING the creature is enough. CR
  -- 109.5 makes that "you" the Aura's controller (enchant is a static ability,
  -- CR 702.5a), which is why the answer changes when control does.
  --
  -- This is the case Pawl.Engine.Sba.stillLegalEnchant's Filter fallthrough exists for.
  -- Its Pool.Creatures-with-no-Filter reduction -- still a creature, on the
  -- battlefield, owned by a player still in the game -- would answer "legal"
  -- here, because none of those three facts changed.
  --
  -- Discriminating on one board and one pass: Control Magic's own enchant slot
  -- is a bare "enchant creature", so it stays attached to the very creature
  -- Setessan Training just fell off.
  Spec.it s "CR 704.5m whole cards: Control Magic steals the enchanted creature, so Setessan Training is buried and Control Magic is not" $ do
    forest <- S.printingOf s registry "Forest"
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    setessanTraining <- S.printingOf s registry "Setessan Training"
    controlMagic <- S.printingOf s registry "Control Magic"
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
        castAura = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) gs (S.cast S.alice auraSpell))
        enchanted = snd (Engine.runGamePure S.identityAnswer castAura Stack.resolveTop)
    case attachedTo creature enchanted of
      [training] -> do
        let (stealId, withSteal) = S.addHandCard controlMagic S.bob enchanted
            ready = withSteal {GameState.priority = Just S.bob, GameState.activePlayer = S.bob}
            castSteal = snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature creature)) ready (S.cast S.bob stealId))
            stolen = snd (Engine.runGamePure S.identityAnswer castSteal Stack.resolveTop)
            settled = S.settleSba stolen
            survivors = attachedTo creature settled
        -- The control case: with control unchanged the Aura is legal, so an SBA
        -- pass leaves it alone.
        Spec.assertBool s (S.onBattlefield training (S.settleSba enchanted)) "while alice still controls the creature the Aura survives a pass"
        Spec.assertEqWith s "the steal really moved control (CR 613.1b)" (Projection.controllerOf creature stolen) (Just S.bob)
        Spec.assertBool s (not (S.onBattlefield training settled)) "Setessan Training is off the battlefield"
        Spec.assertEqWith s "and in its OWNER's graveyard, not the thief's" (length (Game.zoneMembers Zone.Graveyard S.alice settled)) 1
        Spec.assertEqWith s "bob's graveyard is empty" (length (Game.zoneMembers Zone.Graveyard S.bob settled)) 0
        Spec.assertEqWith s "exactly one Aura is left on the creature" (length survivors) 1
        Spec.assertEqWith s "and it is Control Magic, whose enchant slot narrows nothing" (fmap (\oid -> Game.cardOf oid settled) survivors) [Just (Printing.card controlMagic)]
        Spec.assertEqWith s "so the creature is a plain 2/1 again" (S.powerToughnessOf creature settled) (Just (2, 1))
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Trample creature settled)) "and has lost trample"
      _ -> Spec.assertFailure s "Setessan Training should have entered attached to alice's Piker"
  -- CR 613.1b / 303.4e: Control Magic's static ability moves control of the
  -- enchanted creature to the AURA's controller, and leaves the Aura itself alone.
  Spec.it s "CR 613.1b: Control Magic gives the Aura's controller the creature" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.bob base
        (aura, withAura) = S.addCreature controlMagic S.alice withCreature
        attached = S.attach aura creature withAura
    Spec.assertEqWith s "unattached, bob still controls it" (Projection.controllerOf creature withAura) (Just S.bob)
    Spec.assertEqWith s "attached, alice controls it" (Projection.controllerOf creature attached) (Just S.alice)
    Spec.assertEqWith s "the Aura's own controller is unchanged" (Projection.controllerOf aura attached) (Just S.alice)
    Spec.assertBool s (elem creature (Projection.controls S.alice attached)) "and it is in alice's controls"
    Spec.assertBool s (notElem creature (Projection.controls S.bob attached)) "no longer in bob's"
  -- CR 704.5m plus layer 2: destroying the Aura reverts control on the next
  -- projection, because a static ability's effect exists only while its source is
  -- on the battlefield (CR 604.2).
  Spec.it s "CR 604.2: removing Control Magic reverts control" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.bob base
        (aura, withAura) = S.addCreature controlMagic S.alice withCreature
        attached = S.attach aura creature withAura
        gone = S.runPure S.identityAnswer attached (Event.changeZone aura Zone.Graveyard)
    Spec.assertEqWith s "alice controlled it" (Projection.controllerOf creature attached) (Just S.alice)
    Spec.assertEqWith s "bob controls it again" (Projection.controllerOf creature gone) (Just S.bob)
  -- The whole path: cast, target, enter attached, control moves.
  Spec.it s "CR 303.4: casting Control Magic takes the creature" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = S.landsInPlay island 4
        (creature, withCreature) = S.addCreature piker S.bob base
        (gs, spellId) = S.handOne controlMagic withCreature
        cast = snd (Engine.runGamePure S.identityAnswer gs (S.cast S.alice spellId))
        after = snd (Engine.runGamePure S.identityAnswer cast Stack.resolveTop)
    Spec.assertEqWith s "alice controls bob's creature" (Projection.controllerOf creature after) (Just S.alice)
  -- CR 302.6 across turns (#62): control from an Aura is INDEFINITE, so alice
  -- still holds the creature when her own untap step arrives. Engine.settleAll
  -- iterates Projection.controls, so it settles for the controller, and the
  -- creature can attack. Act of Treason could never test this -- its control ends
  -- at cleanup (CR 514.2), long before the thief's untap step.
  --
  -- The whole span, with nothing forced: the Piker settles under bob, the
  -- steal makes it sick again for alice, and only HER untap step settles it
  -- for her. The middle assertion is what #198 got wrong.
  Spec.it s "CR 302.6 (#62): a creature held under indefinite control settles at the thief's untap step" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    controlMagic <- S.printingOf s registry "Control Magic"
    let base = Setup.emptyGame S.bothPlayers
        (creature, withCreature) = S.addCreature piker S.bob base
        settledForBob = S.runPure S.identityAnswer withCreature (Engine.settleAll S.bob)
        (aura, withAura) = S.addCreature controlMagic S.alice settledForBob
        stolen = S.attach aura creature withAura
        settled = S.runPure S.identityAnswer stolen (Engine.settleAll S.alice)
    Spec.assertEqWith s "alice controls it" (Projection.controllerOf creature stolen) (Just S.alice)
    Spec.assertBool s (not (Combat.canAttack S.alice creature stolen)) "the turn she steals it, it cannot attack"
    Spec.assertBool s (Combat.canAttack S.alice creature settled) "and it has settled under her control, so it can attack"

-- CR 303.4's last sentence -- "other effects can limit what a permanent can be
-- enchanted by" -- and CR 301.5's equivalent for an Equipment, which
-- Pawl.Types.AttachRestriction carries and Pawl.Engine.AttachRestriction reads.
-- Consecrate Land ("enchanted land ... can't be enchanted by other Auras") and
-- Goblin Brawler ("this creature can't be equipped") are the pool's two printings
-- of it, and between them they reach both places the answer is asked: CR 701.3a's
-- move and destination offer (Pawl.Engine.Attach.attachmentFor) and CR
-- 704.5m/303.4c's re-check (Pawl.Engine.Sba.fallsOff).
--
-- TARGETING is deliberately absent, and both cards' Gatherer rulings say so --
-- Goblin Brawler's outright ("you can activate an equip ability that targets
-- Goblin Brawler, but the Equipment will fail to move onto it"). CR 702.5a gives
-- the enchant ability the targeting job and this restriction is not it; only
-- protection also forbids the targeting, in a clause of its own (CR 702.16b),
-- which pawl has no keyword for (#1731).
attachRestrictionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attachRestrictionSpec s registry = Spec.describe s "AttachRestriction" $ do
  -- CR 301.5b's last sentence, at the MOVE: "if an effect attempts to attach an
  -- Equipment to an object that can't be equipped by it, the Equipment doesn't
  -- move." The equip ability targets legally either way, so the two runs differ
  -- in exactly one thing -- which creature the target slot names -- and the
  -- Piker run is the positive control that says the board, the mana and the
  -- ability all work.
  Spec.it s "CR 301.5b whole cards: Bonesplitter equips the Piker and fails to move onto Goblin Brawler" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    brawler <- S.printingOf s registry "Goblin Brawler"
    bonesplitter <- S.printingOf s registry "Bonesplitter"
    let base0 = S.landsInPlay island 1
        (pikerId, base1) = S.addCreature piker S.alice base0
        (brawlerId, base2) = S.addCreature brawler S.alice base1
        (splitterId, base3) = S.addCreature bonesplitter S.alice base2
        ready = base3 {GameState.priority = Just S.alice}
        equip = case Face.activatedAbilities (S.combinedFace bonesplitter) of
          ability : _ -> Just ability
          [] -> Nothing
    case equip of
      Nothing -> Spec.assertFailure s "Bonesplitter should print one activated ability"
      Just ability -> do
        let run victim =
              snd
                ( Engine.runGamePure
                    (aimRecipient (Recipient.ToCreature victim))
                    (snd (Engine.runGamePure (aimRecipient (Recipient.ToCreature victim)) ready (Activate.activateAbility S.alice splitterId ability)))
                    Stack.resolveTop
                )
            ontoPiker = run pikerId
            ontoBrawler = run brawlerId
        Spec.assertEqWith s "before: a plain 2/1 Piker" (S.powerToughnessOf pikerId ready) (Just (2, 1))
        Spec.assertEqWith s "before: a plain 2/2 Brawler" (S.powerToughnessOf brawlerId ready) (Just (2, 2))
        -- CR 303.4/301.5's destination limit, at gameplay level: the +2/+0 never
        -- arrives, because the Equipment never moved.
        Spec.assertEqWith s "CR 301.5b: the Brawler is still a 2/2" (S.powerToughnessOf brawlerId ontoBrawler) (Just (2, 2))
        Spec.assertEqWith s "and the Bonesplitter is still unattached" (fmap Object.attachedTo (Game.lookupObject splitterId ontoBrawler)) (Just Nothing)
        -- The positive control on the same board: nothing about the equip
        -- ability, the mana or the target slot is broken.
        Spec.assertEqWith s "the Piker it CAN equip is a 4/1" (S.powerToughnessOf pikerId ontoPiker) (Just (4, 1))
        Spec.assertEqWith s "with the Bonesplitter on it (CR 701.3a)" (fmap Object.attachedTo (Game.lookupObject splitterId ontoPiker)) (Just (Just (Recipient.ToCreature pikerId)))
  -- CR 704.5m with CR 303.4c's "and other applicable effects": the Aura already
  -- on the land is not refused retroactively -- it is buried, on the state-based
  -- pass after Consecrate Land arrives. That is Consecrate Land's own Gatherer
  -- ruling, in one board.
  Spec.it s "CR 704.5m whole card: Consecrate Land buries the Aura already on the land" $ do
    plains <- S.printingOf s registry "Plains"
    mountain <- S.printingOf s registry "Mountain"
    song <- S.printingOf s registry "Song of the Dryads"
    consecrate <- S.printingOf s registry "Consecrate Land"
    let base0 = S.landsInPlay plains 1
        (landId, base1) = S.addCreature mountain S.alice base0
        (songId, base2) = S.addCreature song S.alice base1
        occupied = S.attachTo songId (Recipient.ToObject landId) base2
        (gs, spell) = S.handOne consecrate occupied
        cast = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject landId)) gs (S.cast S.alice spell))
        resolved = snd (Engine.runGamePure (aimRecipient (Recipient.ToObject landId)) cast Stack.resolveTop)
        settled = S.settleSba (S.settleSba resolved)
        arrived = filter (/= songId) (attachedTo landId resolved)
    Spec.assertBool s (Set.member songId (GameState.battlefield occupied)) "before: the Song of the Dryads is on the land"
    Spec.assertEqWith s "and Consecrate Land entered attached to that same land" (length arrived) 1
    -- The gameplay-level assertion this case exists for.
    Spec.assertBool s (not (Set.member songId (GameState.battlefield settled))) "CR 704.5m: the other Aura is put into its owner's graveyard"
    -- CR 400.7 minted a fresh id at the destination, so the Aura is counted by
    -- CARD in its owner's graveyard rather than looked up by the id it held on
    -- the battlefield.
    Spec.assertEqWith s "and it went to its owner's graveyard rather than anywhere else" (length (filter (\oid -> Game.cardOf oid settled == Just (Printing.card song)) (Foldable.toList (Map.findWithDefault Seq.empty S.alice (GameState.graveyard settled))))) 1
    -- CR 303.4d's "other": Consecrate Land is an Aura on the land it protects,
    -- and its own filter's Not IsSource is what leaves it standing.
    Spec.assertEqWith s "Consecrate Land itself survives" (filter (/= songId) (attachedTo landId settled)) arrived
  -- The DESTINATION OFFER, which is the same answer read through
  -- Filter.CanHostSubject: Aura Graft's "attach it to another permanent it can
  -- enchant" never offers the protected land, so the answerer's demand for it is
  -- discarded and the Aura lands on the first permanent that will have it.
  --
  -- Also where Consecrate Land's FIRST clause is proved, on the same board: the
  -- land it protects has indestructible.
  Spec.it s "CR 303.4/701.3a whole cards: Aura Graft will not move an Aura onto a land Consecrate Land protects" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    song <- S.printingOf s registry "Song of the Dryads"
    consecrate <- S.printingOf s registry "Consecrate Land"
    graft <- S.printingOf s registry "Aura Graft"
    let base0 = S.landsInPlay island 2
        (protectedLand, base1) = S.addCreature mountain S.alice base0
        (hostLand, base2) = S.addCreature mountain S.alice base1
        (consecrateId, base3) = S.addCreature consecrate S.alice base2
        guarded = S.attachTo consecrateId (Recipient.ToObject protectedLand) base3
        (songId, base4) = S.addCreature song S.alice guarded
        board = S.attachTo songId (Recipient.ToObject hostLand) base4
        (gs, spell) = S.handOne graft board
        cast = snd (Engine.runGamePure (moveAura songId protectedLand) gs (S.cast S.alice spell))
        after = snd (Engine.runGamePure (moveAura songId protectedLand) cast Stack.resolveTop)
    Spec.assertBool s (Projection.hasKeyword Keyword.Indestructible protectedLand board) "Consecrate Land's first clause: the land has indestructible"
    Spec.assertEqWith s "before: the Song of the Dryads sits on the other land" (fmap Object.attachedTo (Game.lookupObject songId board)) (Just (Just (Recipient.ToObject hostLand)))
    -- The gameplay-level assertion: the answerer asked for the protected land
    -- and did not get it, because CR 303.4's limit kept it out of the offer.
    Spec.assertBool s (notElem songId (attachedTo protectedLand after)) "CR 303.4: the protected land is not a destination"
    Spec.assertBool s (Maybe.isJust (Game.lookupObject songId after >>= Object.attachedTo)) "and the Aura did move, onto a permanent that will have it"

-- The one battlefield object of alice's whose card carries this name. Every
-- object here reaches the battlefield through CR 400.7, which mints a fresh id,
-- so a fixture id taken before the move names nothing afterwards.
battlefieldNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
battlefieldNamed wanted gs =
  List.find
    (\oid -> fmap Face.name (Game.faceOf oid gs) == Just wanted)
    (Game.zoneMembers Zone.Battlefield S.alice gs)

-- Records every search's candidate list, takes what the search offers off the
-- HEAD of it, and would put a CR 303.4f host choice on `other`.
--
-- The head and not a pinned card: the offer is what this case is about, so an
-- answerer that went looking for the legal Aura would find it again after a
-- mutation that widened the offer, and the board would come out identical. The
-- fixture stocks the library so that the Aura the filter must REJECT sits at the
-- head, which is what makes a widened offer change the board rather than only
-- the recording.
--
-- `other` is a creature the Aura could equally well enchant. Nothing should ever
-- ask: Pawl.Engine.Resolve.putFound seeds the entry with the host the effect
-- named, so CR 303.4f's choice is not this move's. An answer of `other` is how a
-- seed that went missing shows up as a wrong board rather than as a silence.
mageAnswer :: ObjectId.ObjectId -> Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
mageAnswer other p = case p of
  Prompt.SearchLibrary _ _ matches cap -> do
    State.modify' (<> [matches])
    pure (List.genericTake cap matches)
  Prompt.Shuffle library -> pure library
  Prompt.ChooseAttachment {} -> pure other
  _ -> pure (S.identityAnswer p)

-- CR 701.3a asked from the CANDIDATE's side -- "an Aura card that could enchant
-- it", where the host is fixed for the whole evaluation and the Aura varies per
-- candidate. Filter.CanHostSubject is the same rule with the two roles swapped,
-- and auraGraftSpec above is its case.
couldEnchantSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
couldEnchantSpec s registry = Spec.describe s "CouldEnchant" $ do
  -- The gameplay-level proof design.md section 4 asks for: cast the Mage, let
  -- its CR 603.2 trigger resolve, and see which Aura the search could reach.
  --
  -- The library holds three cards the HasSubtype conjunct and the CR 701.3a one
  -- separate differently: Consecrate Land is an Aura the Mage cannot host (CR
  -- 303.4's "enchant land") though the board holds six lands it could, Unholy
  -- Strength is one it can, and a Goblin Piker is no Aura at all. Consecrate Land
  -- is stocked LAST, so it is at the head of the library the search reads and is
  -- what an offer that had stopped asking rule 701.3a would hand back.
  Spec.it s "CR 701.3a whole card: Auratouched Mage finds only an Aura that could enchant it" $ do
    plains <- S.printingOf s registry "Plains"
    mage <- S.printingOf s registry "Auratouched Mage"
    strength <- S.printingOf s registry "Unholy Strength"
    consecrate <- S.printingOf s registry "Consecrate Land"
    piker <- S.printingOf s registry "Goblin Piker"
    let base0 = S.landsInPlay plains 6
        (pikerId, base1) = S.addCreature piker S.alice base0
        (_, base2) = S.addLibraryCard piker S.alice base1
        (_, base3) = S.addLibraryCard strength S.alice base2
        (_, base4) = S.addLibraryCard consecrate S.alice base3
        (gs, spell) = S.handOne mage base4
        ((_, after), searches) =
          State.runState
            (Engine.runGame (mageAnswer pikerId) gs (do S.cast S.alice spell; Engine.priorityLoop))
            []
        strengthName = S.nameOf (Printing.card strength)
        consecrateName = S.nameOf (Printing.card consecrate)
        -- Read off `gs`, the PRE-run board: every candidate a library search
        -- offers is an object that was already there and has not moved, so its
        -- id still names it -- which the found card's would not, CR 400.7 having
        -- minted a fresh one.
        named = fmap (Maybe.mapMaybe (fmap Face.name . flip Game.faceOf gs))
    Spec.assertEqWith s "before: nothing of alice's is enchanted" (S.countOnBattlefieldByName strengthName S.alice gs) 0
    case battlefieldNamed (S.nameOf (Printing.card mage)) after of
      Nothing -> Spec.assertFailure s "the Mage should have resolved onto the battlefield"
      Just mageId -> do
        -- THE gameplay-level assertion, and FIRST: the +2/+1 of the Aura the Mage
        -- can host arrived, which an offer that had handed back Consecrate Land
        -- instead could not have produced -- rule 303.4i would have left it in
        -- the library and the Mage would still be a printed 3/3. It reads the
        -- MAGE alone, so a run that put no Aura on the battlefield at all still
        -- reaches it rather than tripping a structural check ahead of it.
        Spec.assertEqWith s "CR 701.3a: the Mage is a 5/4, wearing the Aura it could host" (S.powerToughnessOf mageId after) (Just (5, 4))
        -- The seed, and CR 303.4f's prompt not being raised with it: the effect
        -- specified the host, so the Aura is on the MAGE rather than on the other
        -- creature the answerer stands ready to choose.
        Spec.assertEqWith
          s
          "and it entered attached to the Mage rather than to the other creature"
          (fmap (\auraId -> Game.lookupObject auraId after >>= Object.attachedTo) (battlefieldNamed strengthName after))
          (Just (Just (Recipient.ToCreature mageId)))
    -- The OFFER itself, which the board above can only report one card of: the
    -- search never showed the Aura rule 701.3a rejects.
    Spec.assertEqWith s "the search offered exactly the Aura that could enchant the Mage" (named searches) [[strengthName]]
    -- CR 701.23b: a search stating a quality may find fewer, so the rejected Aura
    -- stayed where it was rather than being found and refused at the move.
    Spec.assertEqWith s "and the Aura it rejected is still in the library" (S.countByName consecrateName S.alice after) 1
