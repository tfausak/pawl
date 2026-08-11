{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.FaceDown, Pawl.Engine.Card.faceDownFace, the
-- Effect.TurnFaceDown arm of Pawl.Engine.Resolve, and the face-down arms
-- threaded through Pawl.Engine.Game.faceOf, Pawl.Engine.Cast,
-- Pawl.Engine.Cost.costsFor, Pawl.Engine.Event.changeZoneFaceDown and
-- Pawl.Engine.Stack -- rule 708 as far as morph reaches it. Also
-- Pawl.Engine.Attach.turnUpHosts, since CR 303.4k's attachment choice is made
-- WHILE a permanent is being turned face up (CR 708.11) and nowhere else.
--
-- FOUR morph cards carry the CAST and TURN-FACE-UP halves of rule 708, one per
-- part of them this file reaches, a fifth card carries the TURN-FACE-DOWN
-- half, and a sixth -- Aven Farseer, which has no morph ability at all -- is the
-- WATCHER of rule 708.7's other written form.
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
module Pawl.FaceDownSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Attach as Attach
import qualified Pawl.Engine.Cast as Cast
import qualified Pawl.Engine.Engine as Engine
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
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "FaceDown" $ do
  offerSpec s registry
  castSpec s registry
  turnFaceUpSpec s registry
  turnUpAttachSpec s registry
  turnFaceDownSpec s registry

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
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [aura]
        -- The destination answered is BOB's creature, which is neither the
        -- lowest-sorting candidate nor alice's own: an implementation that
        -- offered one candidate, or that fell back to the head of the list,
        -- lands the Aura on the Mammoth instead and fails here.
        let after = S.runPure (giftAnswer fodder theirs) before (FaceDown.turnFaceUp S.alice aura)
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
        let mineInstead = S.settleSba (S.runPure (giftAnswer fodder mine) before (FaceDown.turnFaceUp S.alice aura))
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
          let up = S.runPure (giftAnswer fodder theirs) before (FaceDown.turnFaceUp S.alice aura)
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
        let after = S.runPure (declining fodder) before (FaceDown.turnFaceUp S.alice aura)
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
            [CostComponent.Sacrifice _ criterion] ->
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
    case S.spellTargetSpec backslide of
      Nothing -> Spec.assertFailure s "Backslide declares no target slot"
      Just theSpec ->
        Spec.assertEqWith
          s
          "CR 702.37e only the creature with a morph ability is a legal target"
          (Target.legalRecipients (Just S.alice) spell theSpec gs)
          (Set.singleton (Recipient.ToCreature morphling))
    -- THE BEFORE control: the Tracker really is its printed self to begin with,
    -- so every assertion below is about the resolution and not about the fixture.
    Spec.assertEqWith s "the printed 3/3 before" (S.powerToughnessOf morphling gs) (Just (3, 3))
    Spec.assertEqWith s "face up before" (fmap Object.facing (Game.lookupObject morphling gs)) (Just Facing.FaceUp)
    let after = S.runPure (aimAtCreature morphling) gs (Cast.castSpell S.alice spell (S.printingName backslide) Facing.FaceUp >> Stack.resolveTop)
    Spec.assertEqWith s "CR 708.2a it is face down" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceDown)
    Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 3/3" (S.powerToughnessOf morphling after) (Just (2, 2))
    Spec.assertEqWith s "CR 708.2a no name" (Projection.nameOf morphling after) noName
    Spec.assertEqWith s "CR 708.2a no subtypes, not Dog Scout" (Projection.subtypesOf morphling after) Set.empty
    Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike morphling after)) "CR 708.2a no text, so no first strike"
    -- The untargeted creature is untouched, which is CR 115.1 as much as rule 708:
    -- one target, one victim.
    Spec.assertEqWith s "the Piker is still face up" (fmap Object.facing (Game.lookupObject vanilla after)) (Just Facing.FaceUp)
    Spec.assertEqWith s "and still the printed 2/1" (S.powerToughnessOf vanilla after) (Just (2, 1))
    Spec.assertEqWith s "and still named" (Projection.nameOf vanilla after) (S.printingName piker)

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
    Spec.assertEqWith s "CR 708.2a it is face down" (fmap Object.facing (Game.lookupObject morphling after)) (Just Facing.FaceDown)
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

-- CR 708.2a's name: the empty one, which matches no printed card.
noName :: CardName.CardName
noName = CardName.MkCardName Text.empty

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
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "the face-down cast is offered"
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
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "the face-down cast is offered"
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceUp) offered) "and so is the {5}{R} one"

  -- A card with no morph ability offers no face-down cast at all: the offer
  -- comes from CR 702.37a's keyword and not from the rules.
  Spec.it s "CR 702.37d a card without morph offers no face-down cast" $ do
    mountain <- S.printingOf s registry "Mountain"
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, oid) = morphBoard mountain piker 6
        offered = Action.legalActions S.alice gs
    Spec.assertBool s (elem (Action.Type.Cast oid (S.printingName piker) Facing.FaceUp) offered) "the ordinary cast is offered"
    Spec.assertBool s (notElem (Action.Type.Cast oid (S.printingName piker) Facing.FaceDown) offered) "and no face-down one is"

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
    Spec.assertBool s (elem (Action.Type.Cast oid name Facing.FaceDown) offered) "but the morph cast is nameless and still offered"

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
        (after, entered) = castAndResolve ainok Facing.FaceDown gs oid
    case entered of
      Nothing -> Spec.assertFailure s "the morph cast did not reach the battlefield"
      Just permanent -> do
        Spec.assertEqWith s "CR 708.2a a 2/2, not the printed 3/3" (S.powerToughnessOf permanent after) (Just (2, 2))
        Spec.assertEqWith s "CR 708.2a no name" (Projection.nameOf permanent after) noName
        Spec.assertEqWith s "CR 708.2a no subtypes, not Dog Scout" (Projection.subtypesOf permanent after) Set.empty
        Spec.assertBool s (not (Projection.hasKeyword Keyword.FirstStrike permanent after)) "CR 708.2a no text, so no first strike"
        -- CR 110.5 / 708.4's last sentence: the permanent the spell becomes is a
        -- face-down permanent, so the status survived the stack-to-battlefield
        -- move CR 400.7 would otherwise forget.
        Spec.assertEqWith s "CR 708.4 it is face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceDown)
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
        (after, entered) = castAndResolve ainok Facing.FaceDown gs oid
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
        Spec.assertEqWith s "the printed name" (Projection.nameOf permanent after) (S.printingName ainok)
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
        Spec.assertEqWith s "the short board has a face-down permanent" (fmap Object.facing (Game.lookupObject shortId shortGs)) (Just Facing.FaceDown)
        Spec.assertEqWith s "four untapped Mountains cannot pay {4}{R}" (FaceDown.turnableFaceUp S.alice shortGs) []
        Spec.assertEqWith s "five can" (FaceDown.turnableFaceUp S.alice enoughGs) [enoughId]
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
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice permanent)
        Spec.assertEqWith s "CR 708.8 the printed 3/3 after" (S.powerToughnessOf permanent after) (Just (3, 3))
        Spec.assertEqWith s "CR 708.8 the printed name after" (Projection.nameOf permanent after) (S.printingName ainok)
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
        Spec.assertEqWith s "alice may" (FaceDown.turnableFaceUp S.alice gs) [permanent]
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
            turned = S.runPure S.identityAnswer damaged (FaceDown.turnFaceUp S.alice permanent)
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
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [permanent]
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 708.7 bob took the 2" (S.lifeOf S.bob after) (Just 18)
        -- CR 115.1: the ability TARGETS, so the damage went where it was aimed
        -- and was not broadcast at the table.
        Spec.assertEqWith s "and alice took none" (S.lifeOf S.alice after) (Just 20)
        Spec.assertEqWith s "CR 708.8 the printed 2/1" (S.powerToughnessOf permanent after) (Just (2, 1))
        Spec.assertEqWith s "CR 708.8 the printed name" (Projection.nameOf permanent after) (S.printingName marauder)
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
        let again = S.runPure (aimAt S.bob) after (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
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
        let after = S.runPure (aimAt S.bob) before (FaceDown.turnFaceUp S.alice permanent >> Engine.priorityLoop)
        Spec.assertEqWith s "CR 702.37e it is still face down" (fmap Object.facing (Game.lookupObject permanent after)) (Just Facing.FaceDown)
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
        (afterFirst, firstEntered) = castAndResolve marauder Facing.FaceDown both firstCard
        (afterSecond, secondEntered) = castAndResolve marauder Facing.FaceDown afterFirst secondCard
    case (firstEntered, secondEntered) of
      (Just one, Just two) -> do
        let flippedOne = S.runPure (aimAt S.bob) afterSecond (FaceDown.turnFaceUp S.alice one >> Engine.priorityLoop)
            flippedTwo = S.runPure (aimAt S.bob) flippedOne (FaceDown.turnFaceUp S.alice two >> Engine.priorityLoop)
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
        Spec.assertEqWith s "CR 708.2a no name before" (Projection.nameOf permanent before) noName
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Flying permanent before)) "CR 708.2a no flying before"
        Spec.assertBool s (not (Projection.hasKeyword Keyword.Vigilance permanent before)) "CR 708.2a no vigilance before"
        Spec.assertEqWith s "CR 702.37b no counter before" (S.counterOf CounterKind.PlusOnePlusOne permanent before) 0
        -- The control: the action really is on offer, so nothing below is a
        -- permanent that simply never turned over.
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [permanent]
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice permanent)
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
        Spec.assertEqWith s "CR 708.8 the printed name after" (Projection.nameOf permanent after) (S.printingName kirin)
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
        let again = S.runPure S.identityAnswer after (FaceDown.turnFaceUp S.alice permanent)
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
    case farseerBoard mountain farseer ainok 8 of
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
        Spec.assertEqWith s "CR 702.37e the action is available" (FaceDown.turnableFaceUp S.alice before) [morphling]
        let after = S.runPure S.identityAnswer before (FaceDown.turnFaceUp S.alice morphling >> Engine.priorityLoop)
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

-- alice with Aven Farseer already on the battlefield and a face-down permanent
-- of the morph printing beside it, on a board of `n` lands three of which CR
-- 702.37a's {3} has tapped. Nothing if the morph cast did not land.
--
-- The Farseer is seated BEFORE the cast on purpose: it is therefore watching when
-- the face-down permanent enters, which is what makes the before assertions a
-- real negative rather than a permanent that was not there yet.
farseerBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> Maybe (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
farseerBoard land farseer morph n =
  let (gs0, card) = morphBoard land morph n
      (watcher, gs1) = S.addCreature farseer S.alice gs0
      (after, entered) = castAndResolve morph Facing.FaceDown gs1 card
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
      (after, entered) = castAndResolve gift Facing.FaceDown gs3 card
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
    [ReplacementEffect.TurnUpR _ (TurnUpRewrite.MayAttachTo f)] -> Just f
    _ -> Nothing

-- A resolved face-down permanent of a morph printing on a board of `n` lands,
-- three of which CR 702.37a's {3} has tapped. Nothing if the cast did not land.
faceDownWith :: Printing.Printing -> Printing.Printing -> Int -> Maybe (GameState.GameState, ObjectId.ObjectId)
faceDownWith land morph n =
  let (gs, oid) = morphBoard land morph n
      (after, entered) = castAndResolve morph Facing.FaceDown gs oid
   in fmap (\permanent -> (after, permanent)) entered
