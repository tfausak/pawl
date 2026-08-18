{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.FaceDown, Pawl.Engine.Card.faceDownFace, the
-- Effect.TurnFaceDown arm of Pawl.Engine.Resolve, and the face-down arms
-- threaded through Pawl.Engine.Game.faceOf, Pawl.Engine.Cast,
-- Pawl.Engine.Cost.costsFor, Pawl.Engine.Event.changeZoneFaceDown and
-- Pawl.Engine.Stack -- rule 708 as far as morph reaches it, plus CR 708.3's
-- other producer, the EntryRiders.faceDown rider Event.changeZoneEntering reads.
-- Also Pawl.Engine.Attach.turnUpHosts, since CR 303.4k's attachment choice is
-- made WHILE a permanent is being turned face up (CR 708.11) and nowhere else.
--
-- FOUR morph cards carry the CAST and TURN-FACE-UP halves of rule 708, one per
-- part of them this file reaches, another carries the TURN-FACE-DOWN half,
-- Cyber Conversion carries the half where the effect LISTS characteristics of
-- its own, Aven Farseer -- which has no morph ability at all -- is the WATCHER
-- of rule 708.7's other written form, and Soul Summons is the one card here that
-- reaches rule 708 without a cast at all.
--
-- Ainok Tracker is the SUBSTITUTION's card. {5}{R} Creature -- Dog Scout 3/3,
-- "First strike / Morph {4}{R}". Every axis CR 708.2a substitutes is observable
-- on it and none of them coincides with the face-down value: 3/3 against the
-- rule's 2/2, two subtypes against none, a keyword against none, a name against
-- none, and a mana value of 6 against CR 202.3a's 0. A 2/2 morph creature would
-- leave the headline assertion passing whether the substitution happened or not.
--
-- Misthoof Kirin is MEGAMORPH's card. {2}{W} Creature -- Kirin 2/1, "Flying,
-- vigilance / Megamorph {1}{W}". Chosen for its P/T and against every 1/1
-- megamorph creature in the pool: 1/1 plus CR 702.37b's counter is 2/2, which is
-- CR 708.2a's face-down printing exactly, so such a card could not tell a
-- counter that landed from one that did not. Misthoof Kirin ends a 3/2, which is
-- neither the face-down 2/2 nor the printed 2/1 -- and differs from the printed
-- pair on BOTH axes, so no single stale read produces it.
--
-- Gift of Doom is CR 303.4k's card, and the rule's ONLY printing: {4}{B}
-- Enchantment - Aura, "Enchant creature / Enchanted creature has deathtouch and
-- indestructible. / Morph-Sacrifice another creature. / As this Aura is turned
-- face up, you may attach it to a creature." It is the only card in Magic that
-- is both an Aura and a morph, which is what makes it the only card that can
-- reach a rule about what an Aura being turned face up attaches to. Its morph
-- cost is a SACRIFICE rather than mana, which the group below leans on twice:
-- it removes a creature from the board between the offer and the choice, and CR
-- 708.2a's face-down 2/2 makes the Aura itself a candidate for its own "another
-- creature" unless the cost's Filter is framed.
--
-- Skirk Marauder is the SELF-SCOPED trigger's card -- rule 708.7's first written
-- form, the one whose bearer is the permanent that turns over.
-- {1}{R} Creature -- Goblin 2/1, "Morph {2}{R} / When this creature is turned
-- face up, it deals 2 damage to any target." Three different 2s meet on it --
-- the damage, CR 708.2a's face-down 2/2 and the printed 2 power -- so the damage
-- is asserted as a LIFE-TOTAL DELTA on the chosen target and never as a board
-- fact, which is the one reading none of the others can fake.
--
-- Aven Farseer is the WATCHER's card, and the only printing rule 708.7's second
-- written form needs. {1}{W} Creature -- Bird Soldier 1/1, "Flying / Whenever a
-- permanent is turned face up, put a +1/+1 counter on this creature." It bears
-- no morph ability, so it can never be the permanent that turns over: the bearer
-- and the event's subject are two different objects by construction, which is
-- the whole content of this form. Ainok Tracker is what turns over in front of
-- it, chosen over Skirk Marauder (which carries a turned-face-up trigger of its
-- own, so the two conditions could not be told apart) and over Misthoof Kirin
-- (whose megamorph puts a SECOND +1/+1 counter on the board, on the permanent
-- the assertions have to prove the Farseer's counter did NOT land on).
--
-- Backslide is the TURN-FACE-DOWN half's card, and the Tracker is its victim
-- again for the same reason. {1}{U} Instant, "Turn target creature with a morph
-- ability face down. / Cycling {U}" -- CR 702.37e's keyword FAMILY on the
-- targeting side, which is what Goblin Piker (2/1, no keywords) is on the board
-- to prove: it is a creature in the same pool and only the family filter keeps
-- Backslide off it.
--
-- Soul Summons is MANIFEST's card, and the only one here whose permanent never
-- passes through the stack. {1}{W} Sorcery, "Manifest the top card of your
-- library" (CR 701.40a) -- one clause, transcribed whole, and preferred over
-- Write into Being, whose look-at-two-and-choose prompt is beside CR 708.3.
-- Thragtusk is the card underneath it; summonsBoard says why that one, and
-- Ainok Tracker is the card underneath it again for CR 701.40c, where the
-- Tracker's {5}{R} against its morph {4}{R} is the only thing that tells the
-- rule's two turn-face-up procedures apart.
module Pawl.FaceDownSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.FaceDown as FaceDown
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintedReplacement as PrintedReplacement
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure
import qualified Pawl.Types.TurnUpR as TurnUpR
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "FaceDown" $ do
  offerSpec s registry
  castSpec s registry
  turnFaceUpSpec s registry
  turnUpAttachSpec s registry
  turnFaceDownSpec s registry
  listedSpec s registry
  manifestSpec s registry

-- CR 303.4k: an Aura turned face up, choosing what it becomes attached to.
--
-- Gift of Doom is the whole group's card and the rule's only printing --
-- {4}{B} Enchantment - Aura, "Enchant creature / Enchanted creature has
-- deathtouch and indestructible. / Morph-Sacrifice another creature. / As this
-- Aura is turned face up, you may attach it to a creature." That last line is
-- the "effect [that] allows an Aura that's being turned face up to become
-- attached" CR 303.4k is conditional on, and no other card prints one.
--
-- THE BOARD, and every piece of it is a vacuity trap closed:
--
--   * TWO legal hosts, one under EACH player -- alice's War Mammoth and bob's
--     Goblin Piker. One host would let CR 303.4k's choice be elided as a
--     single-candidate one and never asked, so every assertion about the
--     destination would pass without a prompt ever being issued.
--   * A THIRD creature under alice, the Piker her morph cost sacrifices. It is
--     gone by the time the destinations are built, which is why the two hosts
--     above are the whole list.
--   * The face-down 2/2 is asserted BEFORE the turn-up. A cast that never
--     happened would leave every later assertion vacuous.
--   * The Aura is read again after a state-based pass. CR 303.4d buries an Aura
--     that is also a creature and CR 704.5m buries an unattached one; a
--     destination that was chosen and then not stored would go green here and
--     land in the graveyard on the next pass.
turnUpAttachSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnUpAttachSpec s registry = Spec.describe s "Turning an Aura face up" $ do
  Spec.it s "CR 303.4k Gift of Doom turned face up attaches to a creature its face-up enchant ability admits" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    gift <- S.printingOf s registry "Gift of Doom"
    case giftBoard swamp piker mammoth gift of
      Nothing -> Spec.assertFailure s "the morph cast of Gift of Doom did not reach the battlefield"
      Just (before, fodder, mine, theirs, aura) -> do
        -- CR 708.2a: a 2/2 CREATURE with no text, which is the state CR 303.4k's
        -- "as it would exist if it were face up" is measured against.
        Spec.assertEqWith s "CR 708.2a a 2/2 before" (S.powerToughnessOf aura before) (Just (2, 2))
        Spec.assertBool s (Projection.isCreatureOf aura before) "CR 708.2a a creature before"
        Spec.assertBool s (not (Set.member Subtype.Aura (Projection.subtypesOf aura before))) "CR 708.2a no Aura subtype before"
        Spec.assertEqWith s "and attached to nothing" (fmap Object.attachedTo (Game.lookupObject aura before)) (Just Nothing)
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [(aura, TurnUpProcedure.Morph)]
        -- The destination answered is BOB's creature, which is neither the
        -- lowest-sorting candidate nor alice's own: an implementation that
        -- offered one candidate, or that fell back to the head of the list,
        -- lands the Aura on the Mammoth instead and fails here.
        let after = S.runPure (giftAnswer fodder theirs) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph aura)
            settled = S.settleSba (S.settleSba after)
        Spec.assertEqWith s "CR 303.4k attached to the creature alice chose" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just (Just (Recipient.ToCreature theirs)))
        -- CR 613.1f: the Aura's own static ability, which is what says the
        -- attachment is real rather than a field nothing reads.
        Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch theirs after) "the enchanted creature has deathtouch"
        Spec.assertBool s (Projection.hasKeyword Keyword.Indestructible theirs after) "and indestructible"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch mine after)) "and the creature nobody chose has neither"
        -- CR 303.4d and CR 704.5m, the two state-based actions that would bury
        -- this Aura: it is no longer a creature, and it is attached.
        Spec.assertBool s (Set.member aura (GameState.battlefield settled)) "CR 704.5m the Aura is still on the battlefield"
        Spec.assertEqWith s "and still attached" (fmap Object.attachedTo (Game.lookupObject aura settled)) (Just (Just (Recipient.ToCreature theirs)))
        -- THE OTHER LEG of the same choice. Answering with alice's Mammoth puts
        -- the Aura there instead, so both candidates really were offered and the
        -- answer above was not the only outcome this board can produce.
        let mineInstead = S.settleSba (S.runPure (giftAnswer fodder mine) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph aura))
        Spec.assertEqWith s "CR 303.4k the other candidate is equally available" (fmap Object.attachedTo (Game.lookupObject aura mineInstead)) (Just (Just (Recipient.ToCreature mine)))
        Spec.assertBool s (Projection.hasKeyword Keyword.Deathtouch mine mineInstead) "and it is that creature that gains deathtouch"
  -- CR 303.4k's "AS IT WOULD EXIST IF IT WERE FACE UP", read directly off the
  -- destination builder. THE DISCRIMINATOR for the rule's actual content: the
  -- same board, the same Filter and the same function, asked once of the
  -- face-down permanent and once of the face-up one.
  --
  -- Face down it is CR 708.2a's 2/2 creature with no text -- no Aura subtype and
  -- no enchant ability -- so the list is EMPTY and, had the rewrite been applied
  -- before the status write, CR 704.5m would have buried the Aura. Face up the
  -- list is exactly the creatures its enchant ability admits.
  Spec.it s "CR 303.4k the destinations are read off the face-UP Aura, not the face-down 2/2" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    gift <- S.printingOf s registry "Gift of Doom"
    case giftBoard swamp piker mammoth gift of
      Nothing -> Spec.assertFailure s "the morph cast of Gift of Doom did not reach the battlefield"
      Just (before, fodder, mine, theirs, aura) -> case giftDestinationFilter gift of
        Nothing -> Spec.assertFailure s "Gift of Doom should print one CR 614.1e attach rewrite"
        Just filter_ -> do
          Spec.assertEqWith s "CR 708.2a face down, no enchant ability, so nothing is admitted" (Attach.turnUpHosts S.alice aura filter_ before) []
          -- The turn-up, which sacrifices the fodder; the list is then read on
          -- the face-up permanent, before the attachment it goes on to make.
          let up = S.runPure (giftAnswer fodder theirs) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph aura)
              unattached = up {GameState.objects = Map.adjust (\o -> o {Object.attachedTo = Nothing}) aura (GameState.objects up)}
          Spec.assertEqWith
            s
            "CR 303.4k face up, exactly the creatures the enchant ability admits"
            (Attach.turnUpHosts S.alice aura filter_ unattached)
            (List.sort [mine, theirs])
          -- The Swamps are on the same battlefield and are not creatures; the
          -- fodder was sacrificed. Neither shows up, so the list above is a
          -- narrowing rather than "every permanent".
          Spec.assertBool s (notElem fodder (Attach.turnUpHosts S.alice aura filter_ unattached)) "the sacrificed creature is not a candidate"
          Spec.assertEqWith s "and the whole battlefield is bigger than that" (length (Set.toList (GameState.battlefield unattached)) > 2) True
  -- CR 303.4k's "you MAY attach it": declining leaves the Aura attached to
  -- nothing, and CR 704.5m puts an unattached Aura into its owner's graveyard.
  -- The pair with the case above is what says the prompt is a real fork rather
  -- than a formality -- and S.identityAnswer's Script.declining is what answers
  -- it here, so this is also the control for "the may is asked at all".
  Spec.it s "CR 704.5m declining Gift of Doom's may leaves it unattached, and it is buried" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    gift <- S.printingOf s registry "Gift of Doom"
    case giftBoard swamp piker mammoth gift of
      Nothing -> Spec.assertFailure s "the morph cast of Gift of Doom did not reach the battlefield"
      Just (before, fodder, mine, _, aura) -> do
        let after = S.runPure (declining fodder) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph aura)
            settled = S.settleSba (S.settleSba after)
        Spec.assertEqWith s "CR 110.5 it did turn face up" (fmap Object.facing (Game.lookupObject aura after)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 303.4k and attached to nothing" (fmap Object.attachedTo (Game.lookupObject aura after)) (Just Nothing)
        Spec.assertBool s (not (Set.member aura (GameState.battlefield settled))) "CR 704.5m so the Aura is buried"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Deathtouch mine settled)) "and no creature gained deathtouch"
  -- CR 701.21a with CR 702.37e: Gift of Doom's morph cost is "sacrifice ANOTHER
  -- creature", and while it is face down the permanent paying that cost is
  -- itself a creature (CR 708.2a). "Another" is `Not IsSource`, so the candidate
  -- list is only right if the cost's Filter is matched against a context that
  -- knows what the source is -- the same fact Pawl.Engine.Cost.tapCandidates
  -- records for crew, where a Vehicle could otherwise crew itself.
  --
  -- ASSERTED EXACTLY. The Mammoth is on the same battlefield under the same
  -- player and is admitted; only the Aura is taken off, so a list that dropped
  -- everything would fail here too.
  Spec.it s "CR 701.21a a face-down Aura is not 'another creature' for its own morph cost" $ do
    swamp <- S.printingOf s registry "Swamp"
    piker <- S.printingOf s registry "Goblin Piker"
    mammoth <- S.printingOf s registry "War Mammoth"
    gift <- S.printingOf s registry "Gift of Doom"
    case giftBoard swamp piker mammoth gift of
      Nothing -> Spec.assertFailure s "the morph cast of Gift of Doom did not reach the battlefield"
      Just (before, fodder, mine, _, aura) ->
        case FaceDown.morphCostOf aura before of
          Nothing -> Spec.assertFailure s "Gift of Doom should have a morph cost"
          Just cost -> case Cost.components cost of
            [CostComponent.Sacrifice (Sacrifice.MkSacrifice _ criterion)] ->
              Spec.assertEqWith
                s
                "CR 701.21a every creature alice controls except the Aura itself"
                (Replacement.sacrificeCandidates S.alice (Just aura) criterion before)
                (List.sort [fodder, mine])
            _ -> Spec.assertFailure s "Gift of Doom's morph cost should be one sacrifice component"

-- CR 708.2a in the OTHER direction: a face-up permanent turned face down, which
-- is Backslide's Effect.TurnFaceDown.
--
-- ONE board carries every case. alice controls Ainok Tracker -- 3/3, first
-- strike, morph {4}{R} -- and Goblin Piker -- 2/1, no keywords at all -- and
-- holds Backslide, {1}{U} "Turn target creature with a morph ability face down".
-- The Tracker is the victim on purpose: 3/3 differs from CR 708.2a's 2/2 on BOTH
-- axes, so "it became a 2/2" cannot pass on a permanent nothing touched.
turnFaceDownSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnFaceDownSpec s registry = Spec.describe s "Turning face down" $ do
  -- THE PROVING TEST, and its discriminating leg is the target set: CR 702.37e's
  -- "a morph ability" is a keyword FAMILY, so Backslide reaches the Tracker
  -- whatever its morph cost happens to be and never reaches the Piker. Asserted
  -- EXACTLY rather than by membership, which is what makes the exclusion mean
  -- something -- the Piker is a creature on the same battlefield and the pool
  -- offers it; only the filter takes it off.
  Spec.it s "CR 702.37e / 708.2a Backslide turns the morph creature face down and leaves the other alone" $ do
    island <- S.printingOf s registry "Island"
    backslide <- S.printingOf s registry "Backslide"
    ainok <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, morphling, vanilla) = backslideBoard island backslide ainok piker
    case S.spellTargetSlot backslide of
      Nothing -> Spec.assertFailure s "Backslide declares no target slot"
      Just theSlot ->
        Spec.assertEqWith
          s
          "CR 702.37e only the creature with a morph ability is a legal target"
          (Target.legalRecipients (Just S.alice) spell theSlot gs)
          (Set.singleton (Recipient.ToCreature morphling))
    -- THE BEFORE control: the Tracker really is its printed self to begin with,
    -- so every assertion below is about the resolution and not about the fixture.
    Spec.assertEqWith s "the printed 3/3 before" (S.powerToughnessOf morphling gs) (Just (3, 3))
    Spec.assertEqWith s "face up before" (fmap Object.facing (Game.lookupObject morphling gs)) (Just Facing.FaceUp)
    let after = S.runPure (aimAtCreature morphling) gs (Cast.castSpell S.alice spell (S.printingName backslide) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "CR 708.2a it is face down" (fmap Object.facing (Game.lookupObject morphling after)) (Just (Facing.faceDown FaceDownReason.TurnedFaceDown))
    Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 3/3" (S.powerToughnessOf morphling after) (Just (2, 2))
    Spec.assertEqWith s "CR 708.2a no name" (Projection.namesOf morphling after) noNames
    Spec.assertEqWith s "CR 708.2a no subtypes, not Dog Scout" (Projection.subtypesOf morphling after) Set.empty
    Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike morphling after)) "CR 708.2a no text, so no first strike"
    -- The untargeted creature is untouched, which is CR 115.1 as much as rule 708:
    -- one target, one victim.
    Spec.assertEqWith s "the Piker is still face up" (fmap Object.facing (Game.lookupObject vanilla after)) (Just Facing.FaceUp)
    Spec.assertEqWith s "and still the printed 2/1" (S.powerToughnessOf vanilla after) (Just (2, 1))
    Spec.assertEqWith s "and still named" (Projection.namesOf vanilla after) (Set.singleton (S.printingName piker))

  -- CR 708.2a lists the copiable CHARACTERISTICS and nothing else, so everything
  -- that is not a characteristic rides through: marked damage, counters and the
  -- tap state all belong to the permanent rather than to its face.
  --
  -- THE DISCRIMINATOR for the opcode writing one status field rather than minting
  -- a CR 400.7 incarnation -- a fresh object would arrive undamaged, uncountered
  -- and untapped, and every assertion here would fail at once.
  --
  -- The +1/+1 counter is also CR 613.4c over the substituted values: 2/2 base
  -- plus the counter is 3/3, where the untouched permanent would have been 4/4.
  Spec.it s "CR 708.2a damage, counters and tap state survive the turn face down" $ do
    island <- S.printingOf s registry "Island"
    backslide <- S.printingOf s registry "Backslide"
    ainok <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    let (base, spell, morphling, _) = backslideBoard island backslide ainok piker
        gs = tap morphling (S.addCounter CounterKind.PlusOnePlusOne 1 morphling (S.markDamage morphling 1 base))
    Spec.assertEqWith s "the printed 3/3 plus a counter before" (S.powerToughnessOf morphling gs) (Just (4, 4))
    let after = S.runPure (aimAtCreature morphling) gs (Cast.castSpell S.alice spell (S.printingName backslide) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "CR 708.2a it is face down" (fmap Object.facing (Game.lookupObject morphling after)) (Just (Facing.faceDown FaceDownReason.TurnedFaceDown))
    Spec.assertEqWith s "CR 708.2a the marked damage survives" (S.damageOf morphling after) (Just 1)
    Spec.assertEqWith s "CR 708.2a the +1/+1 counter survives" (S.counterOf CounterKind.PlusOnePlusOne morphling after) 1
    Spec.assertEqWith s "CR 708.2a it is still tapped" (fmap Object.tapped (Game.lookupObject morphling after)) (Just TapState.Tapped)
    Spec.assertEqWith s "CR 613.4c the counter applies over the 2/2" (S.powerToughnessOf morphling after) (Just (3, 3))

-- alice with two untapped Islands, Backslide in hand, and two creatures on the
-- battlefield: the morph printing and the one with no keywords.
backslideBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
backslideBoard island backslide ainok piker =
  let (gs0, spell) = S.handOne backslide (S.landsInPlay island 2)
      (morphling, gs1) = S.addCreature ainok S.alice gs0
      (vanilla, gs2) = S.addCreature piker S.alice gs1
   in (gs2, spell, morphling, vanilla)

-- CR 701.26a's state, written straight onto the permanent: this file is about
-- rule 708 rather than about how the permanent came to be tapped.
tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

-- Backslide's one target slot, answered with the named creature. Not left to
-- S.identityAnswer: the point of the case is which creature was chosen.
aimAtCreature :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtCreature oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
  _ -> S.identityAnswer p

-- CR 708.2a's "no name", which a set says by being EMPTY: a face-down object
-- has no name for CR 709.4a's membership test to find, rather than one that
-- happens to match no printed card.
noNames :: Set.Set CardName.CardName
noNames = Set.empty

-- alice holds one card of a morph printing with `n` untapped lands in play, in
-- her own precombat main phase with priority. The land is a parameter because
-- the file needs two colours: Mountains for the two red morph cards and Plains
-- for CR 702.37b's white megamorph one.
morphBoard :: Printing.Printing -> Printing.Printing -> Int -> (GameState.GameState, ObjectId.ObjectId)
morphBoard land morph n = S.handOne morph (S.landsInPlay land n)

-- The permanent a move added to the battlefield between these two states, or
-- Nothing when it added none or several. Identifies the new incarnation without
-- asking what card is under it, which is the whole point on this board.
enteredOne :: GameState.GameState -> GameState.GameState -> Maybe ObjectId.ObjectId
enteredOne before after =
  case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
    [oid] -> Just oid
    _ -> Nothing

-- Cast the card with the given facing and let it resolve, returning the
-- permanent it became.
castAndResolve :: Printing.Printing -> Facing.Facing -> GameState.GameState -> ObjectId.ObjectId -> (GameState.GameState, Maybe ObjectId.ObjectId)
castAndResolve morph facing gs oid =
  let after =
        S.runPure
          S.identityAnswer
          gs
          (Cast.castSpell S.alice oid (S.printingName morph) facing >> Stack.resolveTop)
   in (after, enteredOne gs after)

-- CR 708.2's real shape: an effect that LISTS characteristics for the permanent
-- it turns face down, so what the object becomes is neither its printed self nor
-- CR 708.2a's default.
--
-- Cyber Conversion is the card, and the only printing that turns a creature face
-- down without naming a morph ability: {U}{U} Instant, "Turn target creature
-- face down. It's a 2/2 Cyberman artifact creature." Transcribed whole -- there
-- is no clause of it pawl cannot express.
--
-- ONE board carries every case. alice controls Ainok Tracker -- {5}{R} 3/3 Dog
-- Scout with first strike and morph {4}{R} -- and Goblin Piker -- 2/1 Goblin,
-- no keywords -- with four Islands and five Mountains, and holds the
-- Conversion and a Backslide. The Backslide is the CR 708.2b leg's first half:
-- it lists nothing, so it is what puts a permanent face down with a list the
-- Conversion would visibly overwrite.
--
-- THE TRACKER IS THE VICTIM because the listing has to be told apart from TWO
-- other readings at once, and it differs from both on every axis the rule names:
--
--   * against the PRINTED values -- 3/3 to the listed 2/2, Dog Scout to
--     Cyberman, creature to artifact creature, first strike to no text, a name
--     to none, red to colourless, mana value 6 to 0;
--   * against CR 708.2a's DEFAULTS -- no subtypes to Cyberman, creature alone to
--     artifact creature. The 2/2 is the same either way and proves nothing
--     against the defaults, which is why the subtype and the card type carry the
--     case.
--
-- The Piker is the untouched control on the same board, and the turn-face-up leg
-- is the same permanent differing in one thing: CR 708.8 reverts the copiable
-- values, so the listing has to disappear as cleanly as it arrived.
listedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
listedSpec s registry = Spec.describe s "Listed characteristics" $ do
  -- THE PROVING TEST.
  Spec.it s "CR 708.2 Cyber Conversion's listed characteristics replace both the printed ones and CR 708.2a's defaults" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    cyber <- S.printingOf s registry "Cyber Conversion"
    ainok <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    backslide <- S.printingOf s registry "Backslide"
    let (gs, spell, _, victim, bystander) = cyberBoard island mountain cyber backslide ainok piker
    -- THE BEFORE control: the Tracker is its printed self, so every assertion
    -- below is about the resolution rather than about the fixture.
    Spec.assertEqWith s "the printed 3/3 before" (S.powerToughnessOf victim gs) (Just (3, 3))
    Spec.assertEqWith s "the printed subtypes before" (Projection.subtypesOf victim gs) (Set.fromList [Subtype.Dog, Subtype.Scout])
    Spec.assertEqWith s "the printed card types before" (Projection.cardTypesOf victim gs) (Set.singleton CardType.Creature)
    let after = S.runPure (aimAtByFiltering victim) gs (Cast.castSpell S.alice spell (S.printingName cyber) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertBool s (maybe False (Facing.isFaceDown . Object.facing) (Game.lookupObject victim after)) "CR 708.2 it is face down"
    -- The two axes the listing wins on, and neither reading of the rule produces
    -- the other's answer.
    Spec.assertEqWith s "CR 708.2 the listed subtype, not Dog Scout and not CR 708.2a's none" (Projection.subtypesOf victim after) (Set.singleton Subtype.Cyberman)
    Spec.assertEqWith s "CR 708.2 the listed card types, not CR 708.2a's creature alone" (Projection.cardTypesOf victim after) (Set.fromList [CardType.Artifact, CardType.Creature])
    -- The listing's own 2/2, which is also CR 708.2a's, so it discriminates
    -- against the printed 3/3 alone.
    Spec.assertEqWith s "CR 708.2 the listed 2/2, not the printed 3/3" (S.powerToughnessOf victim after) (Just (2, 2))
    -- Everything the listing does NOT name is still CR 708.2's "no
    -- characteristics other than those listed".
    Spec.assertEqWith s "CR 708.2 no name" (Projection.namesOf victim after) noNames
    Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike victim after)) "CR 708.2 no text, so no first strike"
    Spec.assertEqWith s "CR 708.2 no mana cost, so colourless" (Projection.colorsOf victim after) Set.empty
    Spec.assertEqWith s "CR 202.3a no mana cost, so mana value 0" (Filter.manaValue (Projection.viewOfObject victim after)) (Just 0)
    -- CR 115.1: one target, one victim.
    Spec.assertEqWith s "the Piker is still face up" (fmap Object.facing (Game.lookupObject bystander after)) (Just Facing.FaceUp)
    Spec.assertEqWith s "and still the printed 2/1 Goblin" (S.powerToughnessOf bystander after) (Just (2, 1))
    Spec.assertEqWith s "and no Cyberman" (Projection.subtypesOf bystander after) (Set.fromList [Subtype.Goblin, Subtype.Warrior])

  -- CR 708.8: "as a face-down permanent is turned face up, its copiable values
  -- revert to its normal copiable values". The SAME permanent differing in one
  -- thing, which is what makes the case above about the facing: the listed set
  -- goes away entire and the printed one comes back entire.
  --
  -- CR 702.37e is what licenses the turn-up at all -- the Tracker's morph cost is
  -- what it "WOULD BE if it were face up", and the rule does not ask how the
  -- permanent came to be face down.
  Spec.it s "CR 708.8 turning it face up reverts the listed characteristics to the printed ones" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    cyber <- S.printingOf s registry "Cyber Conversion"
    ainok <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    backslide <- S.printingOf s registry "Backslide"
    let (gs, spell, _, victim, _) = cyberBoard island mountain cyber backslide ainok piker
        down = S.runPure (aimAtByFiltering victim) gs (Cast.castSpell S.alice spell (S.printingName cyber) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "the listed subtype while face down" (Projection.subtypesOf victim down) (Set.singleton Subtype.Cyberman)
    let up = S.runPure S.identityAnswer down (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph victim)
    Spec.assertEqWith s "CR 708.8 face up again" (fmap Object.facing (Game.lookupObject victim up)) (Just Facing.FaceUp)
    Spec.assertEqWith s "CR 708.8 the printed 3/3 is back" (S.powerToughnessOf victim up) (Just (3, 3))
    Spec.assertEqWith s "CR 708.8 the printed subtypes are back, and no Cyberman" (Projection.subtypesOf victim up) (Set.fromList [Subtype.Dog, Subtype.Scout])
    Spec.assertEqWith s "CR 708.8 the printed card types are back" (Projection.cardTypesOf victim up) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "CR 708.8 the printed name is back" (Projection.namesOf victim up) (Set.singleton (S.printingName ainok))
    Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike victim up) "CR 708.8 and first strike"

  -- CR 708.2b: "a face-down permanent can't be turned face down ... nothing
  -- happens and that effect doesn't change any of its characteristics or their
  -- copiable values".
  --
  -- Backslide FIRST, and that ordering is the whole discriminator: it lists
  -- nothing, so the Tracker goes face down with CR 708.2a's subtype-less
  -- default, and a Conversion that wrongly took effect would overwrite it with
  -- Cyberman. Two listings that differed in nothing could not tell "the rule
  -- stopped it" from "it happened again".
  --
  -- THE PAIR, and both halves run from the SAME post-Backslide board with the
  -- same spell, the same mana and the same answerer -- only the target's facing
  -- differs. A negative on its own board here would pass for want of the second
  -- {U}{U} rather than for the rule, which is what the control leg rules out:
  -- the Conversion demonstrably resolves against the face-up Piker off this very
  -- state. The offered set is asserted EXACTLY as well, since a face-down
  -- permanent that was never a legal target would make the case vacuous a second
  -- way.
  Spec.it s "CR 708.2b Cyber Conversion does nothing to a permanent that is already face down" $ do
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    cyber <- S.printingOf s registry "Cyber Conversion"
    backslide <- S.printingOf s registry "Backslide"
    ainok <- S.printingOf s registry "Ainok Tracker"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, spell, slide, victim, bystander) = cyberBoard island mountain cyber backslide ainok piker
        down = S.runPure (aimAtByFiltering victim) gs (Cast.castSpell S.alice slide (S.printingName backslide) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "CR 708.2a Backslide's default list, no subtypes" (Projection.subtypesOf victim down) Set.empty
    case S.spellTargetSlot cyber of
      Nothing -> Spec.assertFailure s "Cyber Conversion declares no target slot"
      Just theSlot ->
        Spec.assertEqWith
          s
          "CR 115.2 both creatures are legal targets, the face-down one included"
          (Target.legalRecipients (Just S.alice) spell theSlot down)
          (Set.fromList [Recipient.ToCreature victim, Recipient.ToCreature bystander])
    -- THE CONTROL: the same cast off the same state against the face-UP Piker.
    let control = S.runPure (aimAtByFiltering bystander) down (Cast.castSpell S.alice spell (S.printingName cyber) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "the Conversion resolves off this board and lists its Cyberman" (Projection.subtypesOf bystander control) (Set.singleton Subtype.Cyberman)
    -- THE CASE: one thing differs, and it is the target's facing.
    let after = S.runPure (aimAtByFiltering victim) down (Cast.castSpell S.alice spell (S.printingName cyber) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "CR 708.2b still no subtypes, not Cyberman" (Projection.subtypesOf victim after) Set.empty
    Spec.assertEqWith s "CR 708.2b still creature alone, not artifact creature" (Projection.cardTypesOf victim after) (Set.singleton CardType.Creature)
    Spec.assertEqWith s "CR 708.2b the copiable values are untouched" (fmap Object.facing (Game.lookupObject victim after)) (Just (Facing.faceDown FaceDownReason.TurnedFaceDown))

-- alice with four untapped Islands -- {U}{U} for the Conversion and {1}{U} for
-- the Backslide, whose generic the auto-tapper also pays in blue -- five
-- Mountains for the Tracker's {4}{R} morph cost, both
-- spells in hand, and the Tracker and the Piker on the battlefield. Returns the
-- board, the Conversion, the Backslide, the victim and the bystander.
--
-- Every leg is stocked for the LONGEST of them, so no negative here can pass for
-- want of mana.
cyberBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
cyberBoard island mountain cyber backslide ainok piker =
  let (gs0, spell) = S.handOne cyber (S.landsFor mountain S.alice 5 (S.landsInPlay island 4))
      (gs1, slide) = S.handOne backslide gs0
      (victim, gs2) = S.addCreature ainok S.alice gs1
      (bystander, gs3) = S.addCreature piker S.alice gs2
   in (gs3, spell, slide, victim, bystander)

-- A target slot answered by FILTERING the offered set down to the named
-- permanent. Never by building a Recipient: the pool decides which constructor
-- the offer wears, and a hand-built one of another shape is a different
-- recipient that CR 608.2b's re-read at resolution drops silently.
aimAtByFiltering :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aimAtByFiltering oid p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, offered) -> Set.filter (\r -> Recipient.objectOf r == Just oid) offered) sets
  _ -> S.identityAnswer p

-- CR 702.37d: "You can't normally cast a card face down. A morph ability allows
-- you to do so." Two casts of one card, offered side by side and gated apart.
offerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
offerSpec s registry = Spec.describe s "Offer" $ do
  -- CR 702.37a prices the face-down cast at {3} and CR 118.9's alternative
  -- replaces the mana cost, so three Mountains buy the morph cast and not the
  -- {5}{R} one. THE DISCRIMINATOR for Cost.faceDownCost: were the face-down
  -- candidate the card's printed cost, no cast at all would be offered here,
  -- and were it free the {5}{R} one would still be missing.
  Spec.it s "CR 702.37a a morph cast is offered for {3} where the printed cost is not" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (elem (Action.Type.Cast oid name (Facing.faceDown FaceDownReason.Morphed)) offered) "the face-down cast is offered"
    Spec.assertBool s (notElem (Action.Type.Cast oid name Facing.FaceUp) offered) "the {5}{R} cast is not"

  -- Six Mountains pay either, so both actions stand on the menu at once and the
  -- player picks. The engine makes no choice here (docs/design.md's second
  -- invariant): nothing collapses the pair.
  Spec.it s "CR 702.37d both casts are offered when both are affordable" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 6
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (elem (Action.Type.Cast oid name (Facing.faceDown FaceDownReason.Morphed)) offered) "the face-down cast is offered"
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceUp) offered) "and so is the {5}{R} one"

  -- A card with no morph ability offers no face-down cast at all: the offer
  -- comes from CR 702.37a's keyword and not from the rules.
  Spec.it s "CR 702.37d a card without morph offers no face-down cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = morphBoard mountain piker 6
        offered = Action.legalActions S.alice gs
    Spec.assertBool s (elem (Action.Type.Cast oid (S.printingName piker) Facing.FaceUp) offered) "the ordinary cast is offered"
    Spec.assertBool s (notElem (Action.Type.Cast oid (S.printingName piker) (Facing.faceDown FaceDownReason.Morphed)) offered) "and no face-down one is"

  -- CR 708.4: "effects that care about the characteristics of a spell will see
  -- only the face-down spell's characteristics", and CR 702.37c says the same of
  -- the prohibitions applied to the cast. A Null Chamber whose chosen name is
  -- "Ainok Tracker" therefore stops the {5}{R} cast and does not stop the morph
  -- one -- the face-down spell has no name (CR 708.2a) for the prohibition to
  -- match.
  --
  -- THE POSITIVE CONTROL is the face-up half of the same assertion: the same
  -- Chamber on the same board really does stop a cast, so "the morph cast is
  -- offered" is not passing because the Chamber does nothing.
  Spec.it s "CR 708.4 a prohibition naming the card stops the face-up cast and not the morph one" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    nullChamber <- S.printingOf s registry "Null Chamber"
    let (base, oid) = morphBoard mountain ainok 6
        (chamber, withChamber) = S.addCreature nullChamber S.alice base
        -- CR 614.1c's as-enters choice, written straight onto the permanent:
        -- the Chamber's own entry replacement prompts for it, and this file is
        -- about rule 708 rather than about that prompt (Pawl.PlayerEffectSpec
        -- covers the choice itself).
        gs = withChosenNames chamber (Set.singleton (S.printingName ainok)) withChamber
        offered = Action.legalActions S.alice gs
        name = S.printingName ainok
    Spec.assertBool s (notElem (Action.Type.Cast oid name Facing.FaceUp) offered) "the named card cannot be cast face up"
    Spec.assertBool s (elem (Action.Type.Cast oid name (Facing.faceDown FaceDownReason.Morphed)) offered) "but the morph cast is nameless and still offered"

-- Put a set of chosen card names onto a permanent (CR 201.4 / 614.1c).
withChosenNames :: ObjectId.ObjectId -> Set.Set CardName.CardName -> GameState.GameState -> GameState.GameState
withChosenNames oid names gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.chosenNames = names}) oid (GameState.objects gs)}

-- CR 708.2a / 708.4: what the face-down spell and the permanent it becomes are.
castSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
castSpec s registry = Spec.describe s "Cast" $ do
  -- THE PROVING TEST. Every axis rule 708.2a substitutes, read off the
  -- permanent the morph cast produced, against a card that differs from the
  -- rule's values on all of them.
  Spec.it s "CR 708.2a a morph cast becomes a 2/2 with no name, no subtypes and no abilities" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        (after, entered) = castAndResolve ainok (Facing.faceDown FaceDownReason.Morphed) gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 3/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 708.2a no name" (Projection.namesOf permanent after) noNames
        Spec.assertEqWith s "CR 708.2a no subtypes, not Dog Scout" (Projection.subtypesOf permanent after) Set.empty
        Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike permanent after)) "CR 708.2a no text, so no first strike"
        -- CR 110.5 / 708.4's last sentence: the permanent the spell becomes is a
        -- face-down permanent, so the status survived the stack-to-battlefield
        -- move CR 400.7 would otherwise forget.
        Spec.assertEqWith s "CR 708.4 it is face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just (Facing.faceDown FaceDownReason.Morphed))
    -- CR 702.37a: {3} was paid, not {5}{R}. Three Mountains were in play and all
    -- three are tapped, which is the whole board.
    Spec.assertEqWith s "CR 702.37a three mana paid" (S.tappedCount S.alice after) 3

  -- CR 202.3a through CR 708.2a's "no mana cost": a face-down object's mana
  -- value is 0, and not the 6 the card underneath prints. Read through the
  -- filter view every mana-value question goes through.
  Spec.it s "CR 708.2a / 202.3a a face-down permanent has mana value 0" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 3
        (after, entered) = castAndResolve ainok (Facing.faceDown FaceDownReason.Morphed) gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just permanent ->
        Spec.assertEqWith
          s
          "mana value 0"
          (Filter.manaValue (Projection.viewOfObject permanent after))
          (Just 0)

  -- The face-up cast of the same card off the same fixture, so every assertion
  -- above is known to be about the FACING and not about the fixture: cast face
  -- up for {5}{R} and the permanent is the printed 3/3 Dog Scout with first
  -- strike.
  Spec.it s "CR 110.5b the ordinary cast of the same card is a 3/3 Dog Scout" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let (gs, oid) = morphBoard mountain ainok 6
        (after, entered) = castAndResolve ainok Facing.FaceUp gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "the printed 3/3" (S.powerToughnessOf permanent after) (Just (3, 3))
        Spec.assertEqWith s "the printed name" (Projection.namesOf permanent after) (Set.singleton (S.printingName ainok))
        Spec.assertEqWith s "the printed subtypes" (Projection.subtypesOf permanent after) (Set.fromList [Subtype.Dog, Subtype.Scout])
        Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike permanent after) "and first strike"
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)

-- CR 116.2b / 702.37e / 708.8: the special action.
turnFaceUpSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
turnFaceUpSpec s registry = Spec.describe s "Turning face up" $ do
  -- CR 702.37e: the morph cost is REQUIRED. Ainok Tracker's is {4}{R}, so four
  -- Mountains left untapped after the {3} cast are one short and the action is
  -- not offered; five are enough and it is. THE DISCRIMINATOR for the
  -- payability conjunct in FaceDown.canTurnFaceUp -- drop it and the four-land
  -- board offers the action too.
  Spec.it s "CR 702.37e the morph cost is required before the action is offered" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    let short = faceDownWith mountain ainok 7
        enough = faceDownWith mountain ainok 8
    case (short, enough) of
      (Just (shortGs, shortId), Just (enoughGs, enoughId)) -> do
        -- Both boards really do carry a face-down permanent, so the empty
        -- answer below is the COST failing and not the permanent missing.
        Spec.assertEqWith s "the short board has a face-down permanent" (fmap Object.facing (Game.lookupObject shortId shortGs)) (Just (Facing.faceDown FaceDownReason.Morphed))
        Spec.assertEqWith s "four untapped Mountains cannot pay {4}{R}" (FaceDown.turnableFaceUp S.alice shortGs) []
        Spec.assertEqWith s "five can" (FaceDown.turnableFaceUp S.alice enoughGs) [(enoughId, TurnUpProcedure.Morph)]
      _ -> Spec.assertFailure s "the morph cast did not reach the battlefield"

  -- CR 702.37e / 708.8, in both directions off ONE board: the permanent is the
  -- face-down 2/2 before the action and the printed 3/3 Dog Scout with first
  -- strike after it, and the morph cost really was spent.
  Spec.it s "CR 702.37e turning face up regains the printed characteristics" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "a 2/2 before" (S.powerToughnessOf permanent before) (Just (2, 2))
        Spec.assertEqWith s "no subtypes before" (Projection.subtypesOf permanent before) Set.empty
        Spec.assertEqWith s "three lands tapped before" (S.tappedCount S.alice before) 3
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
        Spec.assertEqWith s "CR 708.8 the printed 3/3 after" (S.powerToughnessOf permanent after) (Just (3, 3))
        Spec.assertEqWith s "CR 708.8 the printed name after" (Projection.namesOf permanent after) (Set.singleton (S.printingName ainok))
        Spec.assertEqWith s "CR 708.8 the printed subtypes after" (Projection.subtypesOf permanent after) (Set.fromList [Subtype.Dog, Subtype.Scout])
        Spec.assertBool s (Projection.hasKeyword Keyword.FirstStrike permanent after) "CR 708.8 first strike after"
        Spec.assertEqWith s "CR 110.5 face up after" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
        -- CR 702.37e: "pay that cost". Five more Mountains went down for the
        -- {4}{R}, on top of the three the morph cast spent.
        Spec.assertEqWith s "CR 702.37e eight lands tapped after" (S.tappedCount S.alice after) 8

  -- CR 702.37e's "a face-down permanent YOU control": the opponent is never
  -- offered the action, whatever they could pay.
  Spec.it s "CR 702.37e only the controller may take the action" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (gs, permanent) -> do
        Spec.assertEqWith s "alice may" (FaceDown.turnableFaceUp S.alice gs) [(permanent, TurnUpProcedure.Morph)]
        Spec.assertEqWith s "bob may not" (FaceDown.turnableFaceUp S.bob gs) []

  -- CR 708.8's "any effects that have been applied to the face-down permanent
  -- still apply to the face-up permanent", read where it changes an outcome: two
  -- damage is lethal to the face-down 2/2 (CR 704.5g) and is not lethal to the
  -- 3/3 it turns into, and the damage is still marked either way.
  Spec.it s "CR 708.8 damage marked on the 2/2 carries onto the 3/3" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (gs, permanent) -> do
        let damaged = S.markDamage permanent 2 gs
            -- The control: left face down, CR 704.5g buries it.
            leftDown = S.settleSba damaged
            turned = S.runPure S.identityAnswer damaged (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
            settled = S.settleSba turned
        Spec.assertBool s (not (S.onBattlefield permanent leftDown)) "CR 704.5g two damage is lethal to the face-down 2/2"
        Spec.assertBool s (S.onBattlefield permanent settled) "CR 708.8 the 3/3 survives the same two damage"
        Spec.assertEqWith s "CR 708.8 and the damage is still marked" (S.damageOf permanent settled) (Just 2)

  -- THE PROVING TEST for CR 708.7. Skirk Marauder is cast face down for CR
  -- 702.37a's {3}, turned face up for its {2}{R} morph cost, and the ability it
  -- regains as it turns over deals its 2 to bob.
  --
  -- The damage is read as a LIFE-TOTAL DELTA and never off the board: 2 damage,
  -- CR 708.2a's face-down 2/2 and the printed 2 power are all the same number, so
  -- an assertion about the creature could not tell them apart. bob's 20 -> 18 can
  -- come from nothing else on this board.
  --
  -- bob is answered explicitly rather than left to S.identityAnswer, which picks
  -- the lowest-sorting candidate -- alice, the controller, which would make the
  -- positive case indistinguishable from the "no target was chosen" control.
  --
  -- THE BEFORE assertion is CR 708.4 through CR 708.2a: the face-down spell became
  -- a face-down permanent, which has no text at all, so neither the cast nor the
  -- battlefield entry could fire anything. That is what makes 18 the TURNING's
  -- doing rather than the entry's.
  Spec.it s "CR 708.7 turning Skirk Marauder face up deals its 2 to the chosen target" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    case faceDownWith mountain marauder 6 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "CR 708.3 the face-down entry fired nothing" (S.lifeOf S.bob before) (Just 20)
        -- The control: the action really is on offer, so a silent engine below
        -- cannot be a permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [(permanent, TurnUpProcedure.Morph)]
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 708.7 bob took the 2" (S.lifeOf S.bob after) (Just 18)
        -- CR 115.1: the ability TARGETS, so the damage went where it was aimed
        -- and was not broadcast at the table.
        Spec.assertEqWith s "and alice took none" (S.lifeOf S.alice after) (Just 20)
        Spec.assertEqWith s "CR 708.8 the printed 2/1" (S.powerToughnessOf permanent after) (Just (2, 1))
        Spec.assertEqWith s "CR 708.8 the printed name" (Projection.namesOf permanent after) (Set.singleton (S.printingName marauder))
        Spec.assertEqWith s "CR 110.5 face up" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
        -- {3} for the cast and {2}{R} for the morph cost: six, which is a
        -- multiple of neither printed cost alone.
        Spec.assertEqWith s "CR 702.37a/702.37e six mana in all" (S.tappedCount S.alice after) 6
        -- CR 702.37e's "a FACE-DOWN permanent you control", and the guard on the
        -- whole action: the permanent is face up now, so there is nothing left to
        -- turn over and asking again is a no-op. THE DISCRIMINATOR for the event
        -- being recorded inside FaceDown.turnFaceUp's paid branch rather than on
        -- entry -- record it unconditionally and this second call fires the
        -- ability again, since by now the permanent has its text back to see it
        -- with.
        let again = S.runPure (aimAt S.bob) after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 702.37e asking again turns nothing over and fires nothing" (S.lifeOf S.bob again) (Just 18)

  -- CR 708.8's last sentence, said the way Skirk Marauder can say it: a permanent
  -- that entered the battlefield FACE UP was never TURNED face up, so the same
  -- ability on the same card stays silent. Cast for the printed {1}{R} this time.
  --
  -- The stronger half of the pair with the case above. An engine that fired this
  -- ability on any arrival -- an entry, a Moved event into the battlefield --
  -- would pass every assertion up there and fail here.
  --
  -- Answered with aimAt as well, so a trigger that DID go on the stack would find
  -- its target and reach bob. Nothing is being kept quiet by an unanswerable
  -- prompt.
  Spec.it s "CR 708.8 a Skirk Marauder cast FACE UP was never turned face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    let (gs, oid) = morphBoard mountain marauder 2
        (cast, entered) = castAndResolve marauder Facing.FaceUp gs oid
        settled = S.runPure (aimAt S.bob) cast Engine.priorityLoop
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        -- The control: it really did arrive, face up and printed, so the silence
        -- below is CR 708.7 and not an empty battlefield.
        Spec.assertEqWith s "CR 110.5b the printed 2/1 arrived" (S.powerToughnessOf permanent settled) (Just (2, 1))
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent settled)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 702.37a the printed {1}{R} was paid, not {3}" (S.tappedCount S.alice settled) 2
    Spec.assertEqWith s "CR 708.7 nothing was turned face up, so bob took nothing" (S.lifeOf S.bob settled) (Just 20)

  -- CR 702.37e's "pay that cost", on the failure side: two Mountains left after
  -- the {3} cast cannot pay {2}{R}, FaceDown.turnFaceUp restores the state it
  -- began with, and a permanent that never turned over fires nothing.
  --
  -- The silence here is NOT evidence about where the event is recorded, and the
  -- case does not claim to be: CR 708.2a leaves the still-face-down permanent
  -- with no ability that could see the event, so an engine recording it in the
  -- unpaid branch too would pass this anyway. What this pins is the reject: the
  -- permanent is still face down, and mana that could not pay bought nothing. The
  -- second call in the proving test above is what discriminates the placement.
  Spec.it s "CR 702.37e an unpayable morph cost turns nothing over and fires nothing" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    case faceDownWith mountain marauder 5 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (before, permanent) -> do
        Spec.assertEqWith s "two Mountains cannot pay {2}{R}" (FaceDown.turnableFaceUp S.alice before) []
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 702.37e it is still face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just (Facing.faceDown FaceDownReason.Morphed))
        Spec.assertEqWith s "and bob took nothing" (S.lifeOf S.bob after) (Just 20)

  -- CR 603.2 through the bearer: the ability fires for the permanent it is ON and
  -- not for any permanent turning face up.
  --
  -- TWO Skirk Marauders, flipped one after the other. By the second flip the
  -- FIRST one is face up and carrying its ability again (CR 708.2a took it away
  -- only while it was face down), so a matcher that ignored the bearer would fire
  -- both abilities on that one event and cost bob 4 instead of 2. The running
  -- total is asserted after each flip, so the two flips are told apart.
  --
  -- Twelve Mountains: {3} + {3} for the casts and {2}{R} + {2}{R} for the morph
  -- costs.
  Spec.it s "CR 603.2 a face-up Skirk Marauder does not fire off the OTHER one turning face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    marauder <- S.printingOf s registry "Skirk Marauder"
    let (base, firstCard) = S.handOne marauder (S.landsInPlay mountain 12)
        (secondCard, both) = S.addHandCard marauder S.alice base
        (afterFirst, firstEntered) = castAndResolve marauder (Facing.faceDown FaceDownReason.Morphed) both firstCard
        (afterSecond, secondEntered) = castAndResolve marauder (Facing.faceDown FaceDownReason.Morphed) afterFirst secondCard
    case (firstEntered, secondEntered) of
      (Just one, Just two) -> do
        let flippedOne = S.runPure (aimAt S.bob) afterSecond (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph one >> Engine.priorityLoop)
            flippedTwo = S.runPure (aimAt S.bob) flippedOne (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph two >> Engine.priorityLoop)
        Spec.assertEqWith s "the first flip is worth 2" (S.lifeOf S.bob flippedOne) (Just 18)
        -- Both are face up now, and only the one that turned over fired.
        Spec.assertEqWith s "CR 603.2 the second flip is worth 2 more and not 4" (S.lifeOf S.bob flippedTwo) (Just 16)
        Spec.assertEqWith s "both are face up" (fmap Object.facing (Game.lookupObject two flippedTwo)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 702.37a/702.37e twelve mana in all" (S.tappedCount S.alice flippedTwo) 12
      _ -> Spec.assertFailure s "both morph casts did not reach the battlefield"

  -- THE PROVING TEST for CR 702.37b and CR 708.11. Misthoof Kirin is cast face
  -- down for CR 702.37a's {3}, turned face up for its megamorph {1}{W}, and
  -- arrives with the +1/+1 counter rule 702.37b's second clause puts on it.
  --
  -- MISTHOOF KIRIN AND NOT ANOTHER MEGAMORPH CREATURE, and the reason is a
  -- vacuity trap rather than taste: Gudul Lurker and Marang River Skeleton are
  -- 1/1, so 1/1 plus a +1/+1 counter is 2/2 -- exactly CR 708.2a's face-down
  -- printing. A test on either would read 2/2 whether the counter landed or not.
  -- Misthoof Kirin's 2/1 becomes 3/2, which differs from the face-down 2/2 on
  -- BOTH axes. Ainok Survivalist and Den Protector were rejected for a different
  -- reason: each carries a turned-face-up trigger, which would confound this with
  -- the CR 708.7 machinery the Skirk Marauder cases above already prove.
  --
  -- THE BEFORE assertions are the anti-vacuity control for the whole case. If
  -- Cast.castableSpells could not see a megamorph ability, no face-down cast
  -- would happen at all and every assertion after the turn-up would pass by
  -- never reaching a board -- so the 2/2 with no name and neither keyword is
  -- asserted explicitly first.
  --
  -- Five Plains: {3} for the face-down cast and {1}{W} for the megamorph cost.
  Spec.it s "CR 702.37b turning Misthoof Kirin face up for its megamorph cost puts a +1/+1 counter on it" $ do
    plains <- S.printingOf s registry "Plains"
    kirin <- S.printingOf s registry "Misthoof Kirin"
    case faceDownWith plains kirin 5 of
      Nothing -> Spec.assertFailure s "the megamorph cast did not reach the battlefield"
      Just (before, permanent) -> do
        -- CR 708.2a, on a card that differs from the rule's values on every axis
        -- asserted: a 2/1 read as 2/2, a name read as none, two keywords read as
        -- none.
        Spec.assertEqWith s "CR 708.2a a 2/2 before, not the printed 2/1" (S.powerToughnessOf permanent before) (Just (2, 2))
        Spec.assertEqWith s "CR 708.2a no name before" (Projection.namesOf permanent before) noNames
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying permanent before)) "CR 708.2a no flying before"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance permanent before)) "CR 708.2a no vigilance before"
        Spec.assertEqWith s "CR 702.37b no counter before" (S.counterOf CounterKind.PlusOnePlusOne permanent before) 0
        -- The control: the action really is on offer, so nothing below is a
        -- permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [(permanent, TurnUpProcedure.Morph)]
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
        -- CR 702.37b: "put a +1/+1 counter on it". ONE, not two -- CR 614.5 gives
        -- the minted row one opportunity.
        Spec.assertEqWith s "CR 702.37b exactly one +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne permanent after) 1
        -- CR 122.1a with CR 613.4c: the printed 2/1 plus the counter. 3/2 is
        -- neither the face-down 2/2 (the power differs) nor the printed 2/1 (both
        -- halves differ), so no reading that skipped either the turn-up or the
        -- counter produces it.
        Spec.assertEqWith s "CR 702.37b a 3/2, not the printed 2/1 and not the face-down 2/2" (S.powerToughnessOf permanent after) (Just (3, 2))
        -- CR 708.8: the face-up characteristics really came back, which is what
        -- makes the 3/2 above the printed 2/1 plus a counter rather than some
        -- other 3/2.
        Spec.assertEqWith s "CR 708.8 the printed name after" (Projection.namesOf permanent after) (Set.singleton (S.printingName kirin))
        Spec.assertBool s (Projection.hasKeyword Keyword.Flying permanent after) "CR 708.8 flying after"
        Spec.assertBool s (Projection.hasKeyword Keyword.Vigilance permanent after) "CR 708.8 vigilance after"
        Spec.assertEqWith s "CR 110.5 face up after" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceUp)
        -- {3} for the cast and {1}{W} for the megamorph cost: five, which is a
        -- multiple of neither alone.
        Spec.assertEqWith s "CR 702.37a/702.37b five mana in all" (S.tappedCount S.alice after) 5
        -- THE DISCRIMINATOR for CR 708.11's "while that permanent is being turned
        -- face up, NOT AFTERWARD". The permanent is face up now and so carries
        -- its megamorph ability again, so a replacement loop run outside
        -- FaceDown.turnFaceUp's paid branch -- after the turning rather than
        -- during it -- would find the same row on this second ask and put a
        -- SECOND counter on. The count staying at 1 is what says the counter was
        -- applied as part of the turning over.
        let again = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
        Spec.assertEqWith s "CR 708.11 asking again adds no second counter" (S.counterOf CounterKind.PlusOnePlusOne permanent again) 1
        Spec.assertEqWith s "CR 708.11 and it is still a 3/2" (S.powerToughnessOf permanent again) (Just (3, 2))

  -- THE PROVING TEST for CR 708.7's SECOND written form -- the watcher-scoped
  -- one. Aven Farseer stands on the battlefield doing nothing; Ainok Tracker is
  -- cast face down for CR 702.37a's {3} in front of it and turned face up for its
  -- {4}{R} morph cost, and the counter Aven Farseer's ability puts on lands on
  -- THE FARSEER.
  --
  -- ASSERTED BY OBJECT ID on both permanents, which is the vacuity trap this case
  -- is built around: the Farseer is a 1/1 and a face-down permanent is CR 708.2a's
  -- 2/2, so "some permanent is a 2/2" is true of this board before anything
  -- happens at all. The two are read separately and the Tracker's own counter
  -- count is asserted at zero, so a counter that landed on the wrong permanent
  -- fails here rather than passing as the right answer on the wrong object.
  --
  -- AINOK TRACKER AND NOT SKIRK MARAUDER, and not by taste: the Marauder bears a
  -- SelfTurnedFaceUp trigger, so on a board with both, a bug that fired the
  -- Farseer's ability under the self-scoped condition would be invisible. The
  -- Tracker bears no triggered ability at all, so the only condition that can
  -- fire on this board is the Farseer's. Misthoof Kirin was rejected for the
  -- other reason: CR 702.37b's megamorph counter would put a +1/+1 counter on the
  -- permanent that turned over, which is exactly the object this case has to show
  -- receives none.
  --
  -- THE BEFORE assertions are the negative: the Farseer entered the battlefield,
  -- the face-down Tracker entered it, and a whole priority round was run over
  -- both -- and the Farseer is still the printed 1/1 with no counter. A permanent
  -- ENTERING is not a permanent being TURNED face up (CR 708.8's last sentence),
  -- and nothing about the face-down entry (CR 708.3) is this condition's event.
  --
  -- Eight Mountains: {3} for the face-down cast and {4}{R} for the morph cost.
  -- The Farseer is seated on the battlefield rather than cast, its own {1}{W}
  -- being no part of what this proves.
  Spec.it s "CR 708.7 a permanent turning face up puts Aven Farseer's counter on the FARSEER" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    farseer <- S.printingOf s registry "Aven Farseer"
    case farseerBoard mountain farseer ainok S.alice 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (board, watcher, morphling) -> do
        -- The bearer and the event's subject are two different objects, which is
        -- the whole content of this written form. Stated outright so no assertion
        -- below can quietly be about one permanent twice.
        Spec.assertBool s (watcher /= morphling) "the watcher and the permanent that turns over are two objects"
        let before = S.runPure S.identityAnswer board Engine.priorityLoop
        Spec.assertEqWith s "CR 708.8 two entries and a priority round fired nothing" (S.counterOf CounterKind.PlusOnePlusOne watcher before) 0
        Spec.assertEqWith s "so the Farseer is still the printed 1/1" (S.powerToughnessOf watcher before) (Just (1, 1))
        Spec.assertEqWith s "CR 708.2a and the Tracker is the face-down 2/2" (S.powerToughnessOf morphling before) (Just (2, 2))
        -- The control: the action really is on offer, so a silent engine below
        -- cannot be a permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [(morphling, TurnUpProcedure.Morph)]
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph morphling >> Engine.priorityLoop)
        -- CR 708.7 through CR 603.2: the counter is on the WATCHER.
        Spec.assertEqWith s "CR 708.7 the Farseer took the +1/+1 counter" (S.counterOf CounterKind.PlusOnePlusOne watcher after) 1
        Spec.assertEqWith s "CR 613.4c so the 1/1 is a 2/2" (S.powerToughnessOf watcher after) (Just (2, 2))
        -- And NOT on the permanent that turned over, which is what separates this
        -- condition from SelfTurnedFaceUp.
        Spec.assertEqWith s "CR 708.7 the Tracker took none" (S.counterOf CounterKind.PlusOnePlusOne morphling after) 0
        Spec.assertEqWith s "CR 708.8 so it is the printed 3/3 and not a 4/4" (S.powerToughnessOf morphling after) (Just (3, 3))
        Spec.assertEqWith s "CR 110.5 face up" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceUp)
        -- {3} for the cast and {4}{R} for the morph cost: eight, which is a
        -- multiple of neither printed cost alone.
        Spec.assertEqWith s "CR 702.37a/702.37e eight mana in all" (S.tappedCount S.alice after) 8

  -- CR 708.8's last sentence again, from the WATCHER's side and with the stronger
  -- fixture: the same Ainok Tracker cast FACE UP for its printed {5}{R} was never
  -- turned face up, so the Farseer's ability stays silent.
  --
  -- The pair to the case above, and the second leg ruling out an arm that matched
  -- a battlefield ENTRY. It is the leg that survives the fixture: the case above
  -- catches such an arm on its before assertions, but only because the Tracker is
  -- cast face down on the same board -- move that cast out of the fixture and the
  -- before assertions go quiet, while this case still fails. A creature that
  -- arrived face up was never turned over, however it got there.
  Spec.it s "CR 708.8 an Ainok Tracker cast FACE UP was never turned face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    farseer <- S.printingOf s registry "Aven Farseer"
    let (base, card) = morphBoard mountain ainok 6
        (watcher, seated) = S.addCreature farseer S.alice base
        (cast, entered) = castAndResolve ainok Facing.FaceUp seated card
        settled = S.runPure S.identityAnswer cast Engine.priorityLoop
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        -- The control: it really did arrive, face up and printed, so the silence
        -- below is CR 708.7 and not an empty battlefield.
        Spec.assertEqWith s "CR 110.5b the printed 3/3 arrived" (S.powerToughnessOf permanent settled) (Just (3, 3))
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent settled)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 708.7 nothing was turned face up, so the Farseer took no counter" (S.counterOf CounterKind.PlusOnePlusOne watcher settled) 0
        Spec.assertEqWith s "and is still the printed 1/1" (S.powerToughnessOf watcher settled) (Just (1, 1))

  -- THE PROVING TEST for CR 400.7e's `became` slot under CR 708.7's condition.
  -- Pine Walker's "whenever this creature or another creature you control is
  -- turned face up, untap THAT CREATURE" is the first printing in the pool whose
  -- payload points at the permanent the event names rather than at itself, so it
  -- is what settles that the slot stretches this far.
  --
  -- Aven Farseer's case above is the same event read the same way and cannot
  -- stand in: its counter goes on the WATCHER, which CR 113.7a's source slot
  -- already names, so it would pass with no binding stamped at all.
  --
  -- BOTH PERMANENTS TAPPED FIRST (CR 701.26b: only a tapped permanent can be
  -- untapped), which is the vacuity guard. An engine that untapped nothing, one
  -- that untapped the bearer, and one that untapped everything are then three
  -- different boards, and each object is read by id.
  --
  -- AINOK TRACKER for the Farseer case's own reason -- it bears no triggered
  -- ability at all, so the only condition that can fire on this board is Pine
  -- Walker's, and a bug firing it under SelfTurnedFaceUp would be visible rather
  -- than masked by a second trigger.
  --
  -- Tap state and not P/T is the signal, deliberately: a face-down permanent is
  -- CR 708.2a's 2/2 and Pine Walker is a 5/5, so a power reading would be one
  -- coincidence away from proving nothing.
  Spec.it s "CR 708.7 Pine Walker untaps the permanent that turned face up, not itself" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    walker <- S.printingOf s registry "Pine Walker"
    case farseerBoard mountain walker ainok S.alice 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (board, watcher, morphling) -> do
        Spec.assertBool s (watcher /= morphling) "the watcher and the permanent that turns over are two objects"
        let before = S.runPure S.identityAnswer (S.tapObject watcher (S.tapObject morphling board)) Engine.priorityLoop
        Spec.assertEqWith s "CR 701.26b the subject starts tapped" (fmap Object.tapped (Game.lookupObject morphling before)) (Just TapState.Tapped)
        Spec.assertEqWith s "and so does the watcher" (fmap Object.tapped (Game.lookupObject watcher before)) (Just TapState.Tapped)
        -- The control: the action really is on offer, so a silent board below
        -- cannot be a permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [(morphling, TurnUpProcedure.Morph)]
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph morphling >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 110.5 it is face up" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceUp)
        -- CR 708.7 through CR 603.2, read through the bound slot: the SUBJECT.
        Spec.assertEqWith s "CR 400.7e the permanent that turned over untapped" (fmap Object.tapped (Game.lookupObject morphling after)) (Just TapState.Untapped)
        -- And NOT the bearer, which is what separates `became` from CR 113.7a's
        -- source slot -- the two the Farseer case cannot tell apart.
        Spec.assertEqWith s "CR 113.7a Pine Walker itself is still tapped" (fmap Object.tapped (Game.lookupObject watcher after)) (Just TapState.Tapped)

  -- CR 109.5's "you", and the negative half of the pair: the SAME board with the
  -- same alice-controlled Ainok Tracker turning over, differing only in that Pine
  -- Walker is bob's. "A creature YOU control" is then false of the subject, so the
  -- ability never triggers and nothing untaps.
  --
  -- The positive above is what makes this meaningful -- a tapped permanent is the
  -- default nothing-happened state, so an absence proves something only against a
  -- board where the presence was shown on the same fixture.
  Spec.it s "CR 109.5 a Pine Walker bob controls does not untap alice's permanent" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    walker <- S.printingOf s registry "Pine Walker"
    case farseerBoard mountain walker ainok S.bob 8 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (board, watcher, morphling) -> do
        -- The control: the watcher really is bob's, so the silence below is CR
        -- 109.5 and not a Pine Walker that failed to reach the battlefield.
        Spec.assertEqWith s "CR 110.2 the watcher is bob's" (Projection.controllerOf watcher board) (Just S.bob)
        let before = S.runPure S.identityAnswer (S.tapObject watcher (S.tapObject morphling board)) Engine.priorityLoop
            after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph morphling >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 110.5 it did turn face up" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 109.5 and stayed tapped" (fmap Object.tapped (Game.lookupObject morphling after)) (Just TapState.Tapped)
        Spec.assertEqWith s "as did the Pine Walker" (fmap Object.tapped (Game.lookupObject watcher after)) (Just TapState.Tapped)

  -- CR 708.8's last sentence for the new payload: an Ainok Tracker cast FACE UP
  -- was never turned face up, so alice's Pine Walker untaps nothing. The pair to
  -- the Farseer case one group up, and the leg that rules out a matcher that
  -- fired on a battlefield ENTRY -- which the case above cannot, its subject
  -- having entered face down on the same board.
  Spec.it s "CR 708.8 Pine Walker does not untap a creature cast face up" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    walker <- S.printingOf s registry "Pine Walker"
    let (base, card) = morphBoard mountain ainok 6
        (watcher, seated) = S.addCreature walker S.alice base
        (cast, entered) = castAndResolve ainok Facing.FaceUp seated card
    case entered of
      Nothing -> Spec.assertFailure s "the ordinary cast did not reach the battlefield"
      Just permanent -> do
        let settled = S.runPure S.identityAnswer (S.tapObject watcher (S.tapObject permanent cast)) Engine.priorityLoop
        -- The control: it really did arrive, face up and printed, so the silence
        -- below is CR 708.7 and not an empty battlefield.
        Spec.assertEqWith s "CR 110.5b the printed 3/3 arrived" (S.powerToughnessOf permanent settled) (Just (3, 3))
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject permanent settled)) (Just Facing.FaceUp)
        Spec.assertEqWith s "CR 708.8 nothing was turned face up, so it stayed tapped" (fmap Object.tapped (Game.lookupObject permanent settled)) (Just TapState.Tapped)
        Spec.assertEqWith s "and so did the Pine Walker" (fmap Object.tapped (Game.lookupObject watcher settled)) (Just TapState.Tapped)

-- `who` with the watcher printing already on the battlefield and one of alice's
-- face-down permanents of the morph printing beside it, on a board of `n` lands
-- three of which CR 702.37a's {3} has tapped. Nothing if the morph cast did not
-- land.
--
-- The watcher is seated BEFORE the cast on purpose: it is therefore watching when
-- the face-down permanent enters, which is what makes the before assertions a
-- real negative rather than a permanent that was not there yet.
--
-- The watcher's CONTROLLER is a parameter because CR 109.5 is what the Pine
-- Walker pair below turns on: the same board with the watcher under bob is the
-- negative, differing from the positive in exactly that one thing. The morph cast
-- stays alice's either way (castAndResolve is hers).
farseerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> Int -> Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
farseerBoard land farseer morph who n =
  let (gs0, card) = morphBoard land morph n
      (watcher, gs1) = S.addCreature farseer who gs0
      (after, entered) = castAndResolve morph (Facing.faceDown FaceDownReason.Morphed) gs1 card
   in fmap (\permanent -> (after, watcher, permanent)) entered

-- The one target slot of Skirk Marauder's ability, answered with `who` rather
-- than left to S.identityAnswer's lowest-sorting candidate -- which is alice, the
-- ability's own controller, and so the control rather than the positive case.
aimAt :: PlayerId.PlayerId -> Prompt.Prompt r -> r
aimAt who p = case p of
  Prompt.ChooseTargets _ _ _ sets -> fmap (const (Set.singleton (Recipient.ToPlayer who))) sets
  _ -> S.identityAnswer p

-- CR 303.4k's board. alice holds the Aura and has three Swamps for CR 702.37a's
-- {3}, a Goblin Piker her morph cost will sacrifice and a War Mammoth to
-- enchant; bob has a Goblin Piker of his own. Answers with the face-down
-- permanent, the fodder, and the two hosts. Nothing if the morph cast did not
-- land.
--
-- The hosts are a 3/3 and a 2/1 under DIFFERENT players, so neither "the only
-- candidate" nor "the only one alice controls" can produce the right answer by
-- accident.
giftBoard ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
giftBoard land piker mammoth gift =
  let (gs0, card) = morphBoard land gift 3
      (fodder, gs1) = S.addCreature piker S.alice gs0
      (mine, gs2) = S.addCreature mammoth S.alice gs1
      (theirs, gs3) = S.addCreature piker S.bob gs2
      (after, entered) = castAndResolve gift (Facing.faceDown FaceDownReason.Morphed) gs3 card
   in fmap (\aura -> (after, fodder, mine, theirs, aura)) entered

-- Both of Gift of Doom's turn-up questions: the morph cost's CR 701.21a
-- sacrifice, answered with the fodder rather than the Mammoth, and CR 303.4k's
-- destination, answered with the named host.
--
-- Neither is left to S.identityAnswer, and for opposite reasons: its
-- Script.declining would refuse CR 303.4k's printed "may" outright, and the
-- sacrifice it picked would decide which creature is left to enchant.
giftAnswer :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
giftAnswer fodder host p = case p of
  Prompt.ChooseSacrifices {} -> Set.singleton fodder
  Prompt.ChooseAttachment {} -> host
  Prompt.ChooseTurnUpAttachment {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- The morph cost paid and CR 303.4k's "may" refused. Everything else is
-- S.identityAnswer's, which is where the refusal comes from -- Script.declining
-- answers every optional that way -- so this differs from giftAnswer above in
-- exactly the one arm the case under it is about.
declining :: ObjectId.ObjectId -> Prompt.Prompt r -> r
declining fodder p = case p of
  Prompt.ChooseSacrifices {} -> Set.singleton fodder
  _ -> S.identityAnswer p

-- Gift of Doom's CR 614.1e destination text, read off the committed card rather
-- than restated here: a test asserting what the rewrite admits is then asserting
-- what the card really says.
giftDestinationFilter :: Printing.Printing -> Maybe (Filter.Type.Filter Keyword.Keyword)
giftDestinationFilter printing =
  case Face.replacementEffects (S.combinedFace printing) of
    [PrintedReplacement.MkPrintedReplacement _ (ReplacementEffect.TurnUpR (TurnUpR.MkTurnUpR _ (TurnUpRewrite.MayAttachTo f)))] -> Just f
    _ -> Nothing

-- CR 701.40a / 708.3: a permanent PUT onto the battlefield face down, which is
-- the other half of rule 708 from the morph cast above -- the object never
-- passes through the stack, so nothing in castSpec's route touches it.
manifestSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
manifestSpec s registry = Spec.describe s "Manifest" $ do
  -- THE PROVING TEST, gameplay level: alice casts Soul Summons off two Plains
  -- and the top card of her library is on the battlefield as CR 708.2a's 2/2.
  -- The POSITIVE facts are pinned first and deliberately -- a permanent on the
  -- battlefield, the library one card shorter -- because the CR 708.3 assertion
  -- below is an ABSENCE, and an absence passes for free on a board where the
  -- manifest never happened at all.
  Spec.it s "CR 701.40a manifest puts the top card of the library onto the battlefield as a 2/2" $ do
    (before, after, topId) <- summonsBoard s registry
    case Set.toList (Set.difference (GameState.battlefield after) (GameState.battlefield before)) of
      [permanent] -> do
        Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 5/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 708.2a no name, not Thragtusk" (Projection.namesOf permanent after) noNames
        Spec.assertEqWith s "CR 708.2a no subtypes, not Beast" (Projection.subtypesOf permanent after) Set.empty
        Spec.assertEqWith s "CR 110.5 it is face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just (Facing.faceDown FaceDownReason.Manifested))
        -- CR 400.7: the card that left the library is gone, and the permanent is
        -- a new incarnation of it rather than the same object relabelled.
        Spec.assertBool s (permanent /= topId) "the permanent is a new incarnation (CR 400.7)"
      permanents -> Spec.assertFailure s ("expected exactly one new permanent, got " <> show (length permanents))
    Spec.assertEqWith s "one card left the library" (length (Game.zoneMembers Zone.Library S.alice after)) (length (Game.zoneMembers Zone.Library S.alice before) - 1)
    Spec.assertBool s (notElem topId (Game.zoneMembers Zone.Library S.alice after)) "and it was the top one"

  -- CR 708.3, the rule this unit exists for: "objects that are put onto the
  -- battlefield face down are turned face down before they enter the
  -- battlefield, so the permanent's enters-the-battlefield abilities won't
  -- trigger". Thragtusk's is "when this creature enters, you gain 5 life", so
  -- the rule is a life total that did not move.
  --
  -- Read through the whole priority loop, so a trigger that had been placed
  -- would have resolved by now rather than sitting unresolved on a stack this
  -- assertion does not look at.
  Spec.it s "CR 708.3 the manifested card's enters-the-battlefield ability does not trigger" $ do
    (_, after, _) <- summonsBoard s registry
    Spec.assertEqWith s "alice gained no life" (S.lifeOf S.alice after) (Just 20)
    Spec.assertEqWith s "and nothing is waiting on the stack" (length (GameState.stack after)) 0

  -- THE PAIR. The same board, the same card, the same door -- everything held
  -- fixed but the one rider -- driven through Event.changeZoneEntering rather
  -- than through a cast, because that is the narrowest path the rule is visible
  -- on. Face up, Thragtusk enters as itself and the trigger pays 5 life; face
  -- down, it is a nameless 2/2 and nothing happens. A board with no permanent on
  -- it could not pass either half.
  Spec.it s "CR 110.5b the same card entering FACE UP is a 5/3 Beast whose trigger does fire" $ do
    (before, _, topId) <- summonsBoard s registry
    thragtusk <- S.printingOf s registry "Thragtusk"
    let (up, upId) = putOntoBattlefield False topId before
        (down, downId) = putOntoBattlefield True topId before
    case (upId, downId) of
      (Just faceUp, Just faceDown) -> do
        Spec.assertEqWith s "the printed 5/3" (S.powerToughnessOf faceUp up) (Just (5, 3))
        Spec.assertEqWith s "the printed name" (Projection.namesOf faceUp up) (Set.singleton (S.printingName thragtusk))
        Spec.assertEqWith s "the printed subtype" (Projection.subtypesOf faceUp up) (Set.singleton Subtype.Beast)
        Spec.assertEqWith s "face up" (fmap Object.facing (Game.lookupObject faceUp up)) (Just Facing.FaceUp)
        Spec.assertEqWith s "and the enters trigger paid 5 life" (S.lifeOf S.alice up) (Just 25)
        Spec.assertEqWith s "CR 708.2a the 2/2 on the other board" (S.powerToughnessOf faceDown down) (Just (2, 2))
        Spec.assertEqWith s "CR 708.3 whose trigger paid nothing" (S.lifeOf S.alice down) (Just 20)
      _ -> Spec.assertFailure s "the card did not reach the battlefield"

  -- CR 701.40c, the rule the two-procedure shape exists for: "if a card with
  -- morph is manifested, its controller may turn that card face up using EITHER
  -- the procedure described in rule 702.37e ... OR the procedure described
  -- above". Both stand on the menu at once and the engine picks neither
  -- (docs/design.md's second invariant).
  --
  -- Ainok Tracker is the card because the two procedures are DISTINGUISHABLE on
  -- it and on nothing cheaper: mana cost {5}{R} against morph cost {4}{R}, six
  -- mana against five. A morph creature whose two costs agreed would leave every
  -- assertion below passing whichever procedure actually ran.
  Spec.it s "CR 701.40c a manifested morph card offers both procedures" $ do
    ainok <- S.printingOf s registry "Ainok Tracker"
    (after, entered) <- manifestedBoard s registry ainok 6
    case entered of
      Nothing -> Spec.assertFailure s "the manifest did not reach the battlefield"
      Just permanent -> do
        -- The POSITIVE fixture facts first: this really is a face-down permanent
        -- alice controls, so neither assertion below can pass for want of one.
        Spec.assertEqWith s "CR 701.40a it is face down, manifested" (fmap Object.facing (Game.lookupObject permanent after)) (Just (Facing.faceDown FaceDownReason.Manifested))
        Spec.assertEqWith s "CR 708.2a and a 2/2, not the printed 3/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        let offered = Action.legalActions S.alice after
        Spec.assertBool s (elem (Action.Type.TurnFaceUp permanent TurnUpProcedure.Morph) offered) "CR 702.37e the morph procedure is offered"
        Spec.assertBool s (elem (Action.Type.TurnFaceUp permanent TurnUpProcedure.Manifest) offered) "CR 701.40b the manifest procedure is offered"

  -- THE PAIR, and the prices are what tells the procedures apart. One board, two
  -- actions: CR 701.40b charges Ainok Tracker's MANA COST of {5}{R} -- six lands
  -- -- and CR 702.37e its MORPH COST of {4}{R} -- five. Both end with the same
  -- printed 3/3 Dog Scout, so "it is face up" alone could not discriminate; the
  -- tapped count is the only thing that can.
  Spec.it s "CR 701.40b the manifest procedure pays the mana cost, CR 702.37e the morph cost" $ do
    ainok <- S.printingOf s registry "Ainok Tracker"
    (after, entered) <- manifestedBoard s registry ainok 6
    case entered of
      Nothing -> Spec.assertFailure s "the manifest did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "the {1}{W} sorcery tapped two lands and no more" (S.tappedCount S.alice after) 2
        let manifested = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Manifest permanent)
            morphed = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
        Spec.assertEqWith s "CR 701.40b six more lands went down for {5}{R}" (S.tappedCount S.alice manifested) 8
        Spec.assertEqWith s "CR 702.37e five for {4}{R}" (S.tappedCount S.alice morphed) 7
        -- CR 708.8 off both roads: the permanent regains its normal
        -- characteristics either way, which is why the price is the discriminator
        -- and this is the control.
        Spec.assertEqWith s "CR 708.8 the printed 3/3 after the manifest procedure" (S.powerToughnessOf permanent manifested) (Just (3, 3))
        Spec.assertEqWith s "CR 708.8 and after the morph one" (S.powerToughnessOf permanent morphed) (Just (3, 3))
        Spec.assertEqWith s "CR 708.8 the printed name after the manifest procedure" (Projection.namesOf permanent manifested) (Set.singleton (S.printingName ainok))
        Spec.assertEqWith s "CR 110.5 face up after the manifest procedure" (fmap Object.facing (Game.lookupObject permanent manifested)) (Just Facing.FaceUp)

  -- THE PRICE PAIR as a gate, so the six is the rule's and not the fixture's:
  -- five lands left after the sorcery pay {4}{R} and not {5}{R}, so only the
  -- morph procedure is offered; six pay either. Everything else is held fixed.
  Spec.it s "CR 701.40b the mana cost is required before the manifest procedure is offered" $ do
    ainok <- S.printingOf s registry "Ainok Tracker"
    (short, shortEntered) <- manifestedBoard s registry ainok 5
    (enough, enoughEntered) <- manifestedBoard s registry ainok 6
    case (shortEntered, enoughEntered) of
      (Just shortId, Just enoughId) -> do
        Spec.assertEqWith s "five lands buy the morph procedure alone" (FaceDown.turnableFaceUp S.alice short) [(shortId, TurnUpProcedure.Morph)]
        Spec.assertEqWith s "six buy both" (FaceDown.turnableFaceUp S.alice enough) [(enoughId, TurnUpProcedure.Morph), (enoughId, TurnUpProcedure.Manifest)]
      _ -> Spec.assertFailure s "the manifest did not reach the battlefield"

  -- THE REASON GUARD, which is the whole of what CR 701.40a's "that permanent is
  -- a MANIFESTED permanent" buys: the same card, face down on the battlefield for
  -- the same cost, offered only CR 702.37e's procedure because it got there by
  -- CR 702.37c's cast instead. Nine Mountains leave six untapped after the {3},
  -- which is exactly what {5}{R} would need -- so the manifest procedure is
  -- withheld for the REASON and demonstrably not for the money.
  Spec.it s "CR 702.37c a morph-cast creature is not offered the manifest procedure" $ do
    mountain <- S.printingOf s registry "Mountain"
    ainok <- S.printingOf s registry "Ainok Tracker"
    case faceDownWith mountain ainok 9 of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just (gs, permanent) -> do
        Spec.assertEqWith s "CR 702.37c it is face down, morphed" (fmap Object.facing (Game.lookupObject permanent gs)) (Just (Facing.faceDown FaceDownReason.Morphed))
        Spec.assertEqWith s "six lands are untapped, enough for {5}{R}" (S.tappedCount S.alice gs) 3
        Spec.assertEqWith s "CR 701.40b only the morph procedure is offered" (FaceDown.turnableFaceUp S.alice gs) [(permanent, TurnUpProcedure.Morph)]

  -- CR 701.40b's parenthesis, first half: "if the card representing that
  -- permanent ISN'T A CREATURE CARD ... it can't be turned face up this way."
  --
  -- Lightning Bolt is the card because it isolates that half and nothing else:
  -- {R} Instant, so it HAS a mana cost and fails only the card-type guard, and
  -- the six untapped MOUNTAINS on this board would pay that cost several times
  -- over. A blue or black noncreature card here would be refused for want of the
  -- colour instead, and the case would pass whether the guard existed or not.
  Spec.it s "CR 701.40b a manifested noncreature card offers no procedure at all" $ do
    bolt <- S.printingOf s registry "Lightning Bolt"
    (after, entered) <- manifestedBoard s registry bolt 6
    case entered of
      Nothing -> Spec.assertFailure s "the manifest did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "CR 701.40a it is face down, manifested" (fmap Object.facing (Game.lookupObject permanent after)) (Just (Facing.faceDown FaceDownReason.Manifested))
        Spec.assertEqWith s "CR 708.2a and a 2/2 like any other" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 701.40b no procedure is offered" (FaceDown.turnableFaceUp S.alice after) []

  -- CR 702.37b's condition, which CR 701.40c is what makes reachable: "put a
  -- +1/+1 counter on it IF ITS MEGAMORPH COST WAS PAID to turn it face up". A
  -- manifested megamorph card has two roads face up and only one of them pays
  -- that cost.
  --
  -- THE PAIR, both legs off ONE board, differing in the procedure alone. Misthoof
  -- Kirin is the card because its two prices and its two outcomes BOTH differ:
  -- {2}{W} against megamorph {1}{W}, and the printed 2/1 against the 3/2 the
  -- counter makes. A card whose costs agreed, or whose counter landed on a
  -- symmetric body, could not tell the roads apart.
  Spec.it s "CR 702.37b the manifest procedure pays no megamorph cost, so no counter lands" $ do
    kirin <- S.printingOf s registry "Misthoof Kirin"
    plains <- S.printingOf s registry "Plains"
    (after, entered) <- manifestedWith s registry plains kirin 3
    case entered of
      Nothing -> Spec.assertFailure s "the manifest did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "CR 708.2a the face-down 2/2 before either road" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 701.40c both roads are open" (FaceDown.turnableFaceUp S.alice after) [(permanent, TurnUpProcedure.Morph), (permanent, TurnUpProcedure.Manifest)]
        let manifested = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Manifest permanent)
            morphed = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice TurnUpProcedure.Morph permanent)
        Spec.assertEqWith s "CR 701.40b three lands went down for {2}{W}" (S.tappedCount S.alice manifested) 5
        Spec.assertEqWith s "CR 702.37b two for the megamorph {1}{W}" (S.tappedCount S.alice morphed) 4
        Spec.assertEqWith s "CR 702.37b no counter down the manifest road" (S.counterOf CounterKind.PlusOnePlusOne permanent manifested) 0
        Spec.assertEqWith s "CR 702.37b and the printed 2/1, not the 3/2" (S.powerToughnessOf permanent manifested) (Just (2, 1))
        -- THE CONTROL, and what stops the leg above passing because the row was
        -- never minted: the same permanent, the same board, the megamorph cost
        -- paid, and the counter lands.
        Spec.assertEqWith s "CR 702.37b the counter down the megamorph road" (S.counterOf CounterKind.PlusOnePlusOne permanent morphed) 1
        Spec.assertEqWith s "CR 613.4c and the 3/2 it makes" (S.powerToughnessOf permanent morphed) (Just (3, 2))

-- alice with two Plains and Soul Summons in hand, her library holding Thragtusk
-- on top of a Goblin Piker, returned as (the board before the cast, the board
-- after it has fully resolved, the library card that was on top).
--
-- Thragtusk is the card underneath and every one of its printed values is
-- chosen against CR 708.2a's: a 5/3 where the rule says 2/2, a Beast where it
-- says no subtypes, a name where it says none, and an enters-the-battlefield
-- trigger paying 5 life where CR 708.3 says silence. A 2/2 with no trigger could
-- not tell the rule from the fixture.
--
-- The Piker underneath it keeps CR 104.3c off the board and gives the library a
-- second member, so "one card left the library" is a delta rather than an
-- emptying.
summonsBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, GameState.GameState, ObjectId.ObjectId)
summonsBoard s registry = do
  summons <- S.printingOf s registry "Soul Summons"
  plains <- S.printingOf s registry "Plains"
  thragtusk <- S.printingOf s registry "Thragtusk"
  piker <- S.printingOf s registry "Goblin Piker"
  let (g1, summonsId) = S.handOne summons (S.landsInPlay plains 2)
      (_, g2) = S.addLibraryCard piker S.alice g1
      (topId, before) = S.addLibraryCard thragtusk S.alice g2
      cast = S.runPure S.identityAnswer before (S.cast S.alice summonsId)
  pure (before, S.runPure S.identityAnswer cast Engine.priorityLoop, topId)

-- alice manifests `top` off her library with Soul Summons, off two Plains and
-- `mountains` Mountains, returned as (the board with the manifested permanent on
-- it, that permanent). Resolved through Stack.resolveTop rather than the priority
-- loop, which is the narrowest path that reaches CR 701.40a and leaves the tapped
-- count answering for the sorcery alone.
--
-- The Piker under `top` keeps CR 104.3c off the board and leaves the library
-- non-empty, summonsBoard's reason.
--
-- The two Plains are the sorcery's own {1}{W} and are counted against it: the
-- board has `extra` + 2 lands and the cast takes two, so what is left for a
-- turn-face-up procedure is `extra` lands of the second colour either way the
-- auto-tapper spends the generic.
manifestedBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Printing.Printing -> Int -> m (GameState.GameState, Maybe ObjectId.ObjectId)
manifestedBoard s registry top extra = do
  mountain <- S.printingOf s registry "Mountain"
  manifestedWith s registry mountain top extra

-- manifestedBoard with the second colour named, for a manifested card whose own
-- costs are not red.
manifestedWith :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Printing.Printing -> Printing.Printing -> Int -> m (GameState.GameState, Maybe ObjectId.ObjectId)
manifestedWith s registry land top extra = do
  summons <- S.printingOf s registry "Soul Summons"
  plains <- S.printingOf s registry "Plains"
  piker <- S.printingOf s registry "Goblin Piker"
  let (g1, summonsId) = S.handOne summons (S.landsFor land S.alice extra (S.landsInPlay plains 2))
      (_, g2) = S.addLibraryCard piker S.alice g1
      (_, before) = S.addLibraryCard top S.alice g2
      after = S.runPure S.identityAnswer before (S.cast S.alice summonsId >> Stack.resolveTop)
  pure (after, enteredOne before after)

-- One card from a library onto the battlefield through the door an
-- Effect.MoveToZone takes, with CR 708.3's rider set or not and nothing else
-- differing -- then settled and run to a stable board so any enters trigger has
-- resolved. Nothing if the move did not land.
putOntoBattlefield :: Bool -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, Maybe ObjectId.ObjectId)
putOntoBattlefield faceDown oid gs =
  let riders = EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = faceDown}
      (entered, moved) = Engine.runGamePure S.identityAnswer gs (Event.changeZoneEntering oid Zone.Battlefield LibraryPosition.defaultValue riders (Just S.alice))
   in (S.runPure S.identityAnswer moved Engine.priorityLoop, entered)

-- A resolved face-down permanent of a morph printing on a board of `n` lands,
-- three of which CR 702.37a's {3} has tapped. Nothing if the cast did not land.
faceDownWith :: Printing.Printing -> Printing.Printing -> Int -> Maybe (GameState.GameState, ObjectId.ObjectId)
faceDownWith land morph n =
  let (gs, oid) = morphBoard land morph n
      (after, entered) = castAndResolve morph (Facing.faceDown FaceDownReason.Morphed) gs oid
   in fmap (\permanent -> (after, permanent)) entered
