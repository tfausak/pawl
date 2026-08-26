{-# LANGUAGE GADTs #-}

-- Covers: CR 101.2's COUNTER PROHIBITION on both axes -- the object axis
-- (Pawl.Types.CounterRestriction, the Bool Pawl.Engine.CounterRestriction
-- answers, and the two places it is asked: Pawl.Engine.Event.settleCounters, the
-- door both of CR 122.6's roads share, and Pawl.Engine.Resolve's
-- Effect.MoveCounters arm for rule 122.5's third impossibility) and the player
-- axis (Pawl.Types.PlayerEffect's CantGetCounters, the Bool
-- Pawl.Engine.PlayerEffect.prohibitsCounters answers, asked at
-- Pawl.Engine.Event.putPlayerCounters).
--
-- Two fixtures, chosen for the two ends of the scoping axis:
--
--   * Solemnity {2}{W} Enchantment (Hour of Devastation; name, cost, type line
--     and oracle text checked against Scryfall 2026-08-25):
--
--       Players can't get counters.
--       Counters can't be put on artifacts, creatures, enchantments, or lands.
--
--     The BROAD shape -- every player, every kind, every permanent type a card
--     can be.
--
--   * Melira, Sylvok Outcast {1}{G} Legendary Creature - Human Scout, 2/2 (New
--     Phyrexia; same check, same date):
--
--       You can't get poison counters.
--       Creatures you control can't have -1/-1 counters put on them.
--       Creatures your opponents control lose infect.
--
--     The NARROW shape -- one player, one player-counter kind, one object
--     counter kind, and one controller. It is what proves the kind field and the
--     affected filter are read at all: Solemnity alone leaves both at the value
--     an implementation ignoring them would produce.
--
-- THE BOARD SHAPE that makes these cases discriminating:
--
--   * A PAIRED BOARD for every refusal, differing in exactly that one permanent.
--     An absence passes for free on a board where the placement never happened.
--   * Melira's cases put the SAME spell on a creature alice controls and on one
--     bob controls, so "creatures you control" is read rather than assumed.
--   * Melira's -1/-1 case is paired with a +1/+1 case on THE SAME creature, so
--     the kind field is read rather than assumed.
--   * POWER AND TOUGHNESS are asserted beside the counter tally, since CR 613.4c
--     is what a player would actually see: a counter written and then ignored
--     would still move a 2/1 to 3/2.
module Pawl.CounterRestrictionSpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "CR 101.2 counter prohibitions" $ do
  solemnitySpec s registry
  meliraSpec s registry
  moveSpec s registry

-- Aim every target slot at one object, and take a printed "may". The optional
-- half is Agent's Toolkit's, in moveSpec below; S.identityAnswer declines every
-- optional clause, which would settle that group's cases for the wrong reason.
aiming :: ObjectId.ObjectId -> Prompt.Prompt r -> r
aiming victim p = case p of
  Prompt.ChooseTargets _ _ _ sets -> S.preferring ((== Just victim) . Recipient.objectOf) sets
  Prompt.ChooseOptional {} -> OptionalDecision.Exercises
  _ -> S.identityAnswer p

-- What a player would actually see of a counter on a creature: the tally, and
-- the CR 613.4c power and toughness it feeds. Read as a triple because a
-- placement that was written and then ignored by the layer system, and one that
-- was refused outright, differ in the first component alone.
seenOn :: CounterKind.CounterKind Keyword.Keyword -> ObjectId.ObjectId -> GameState.GameState -> (Natural.Natural, Maybe Integer, Maybe Integer)
seenOn kind oid gs =
  ( S.counterOf kind oid gs,
    Projection.powerOf oid gs,
    Projection.toughnessOf oid gs
  )

-- Stock `pid`'s library with three Plains, so nothing here decks a player that
-- draws (CR 104.3c). Instill Infection and Prologue to Phyresis each draw a card.
stock :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
stock plains pid gs = iterate (snd . S.addLibraryCard plains pid) gs !! 3

-- The newest battlefield object whose printed card has this name. CR 400.7 mints
-- a fresh id at the destination, so a permanent that entered during the run can
-- only be found this way.
newestNamed :: CardName.CardName -> GameState.GameState -> Maybe ObjectId.ObjectId
newestNamed wanted gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just wanted
   in Maybe.listToMaybe (List.sortOn Ord.Down (filter named (Set.toList (GameState.battlefield gs))))

toolkitName :: CardName.CardName
toolkitName = CardName.MkCardName (Text.pack "Agent's Toolkit")

solemnitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
solemnitySpec s registry = Spec.describe s "Solemnity" $ do
  let -- alice: two Forests and three Plains, a Goblin Piker (2/1) settled on the
      -- battlefield, and Battlegrowth ({G} instant: put a +1/+1 counter on target
      -- creature) plus Agent's Toolkit ({1}{G}{U}, which ENTERS with four
      -- counters) in hand. `enchanted` says whether Solemnity is on the
      -- battlefield, and is the ONLY difference between the two boards.
      board enchanted = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        piker <- S.printingOf s registry "Goblin Piker"
        battlegrowth <- S.printingOf s registry "Battlegrowth"
        toolkit <- S.printingOf s registry "Agent's Toolkit"
        prologue <- S.printingOf s registry "Prologue to Phyresis"
        solemnity <- S.printingOf s registry "Solemnity"
        let base = S.landsFor forest S.alice 2 (S.landsFor island S.alice 2 (S.landsFor island S.bob 2 (S.landsInPlay plains 3)))
            (target, g1) = S.addCreature piker S.alice base
            (heldGrowth, g2) = S.addHandCard battlegrowth S.alice g1
            (heldToolkit, g3) = S.addHandCard toolkit S.alice g2
            (heldPrologue, g4) = S.addHandCard prologue S.bob g3
            g5 = stock plains S.bob g4
            g6 = if enchanted then snd (S.addCreature solemnity S.alice g5) else g5
        pure (target, heldGrowth, heldToolkit, heldPrologue, g6)
  -- THE HEADLINE, and the case this unit exists for. CR 101.2: Battlegrowth
  -- "allows or directs" a counter onto the creature and Solemnity says it can't
  -- happen, so the "can't" takes precedence. The power and toughness are asserted
  -- beside the tally because CR 613.4c would otherwise show a counter that was
  -- written and then ignored.
  Spec.it s "CR 101.2 a +1/+1 counter can't be put on a creature" $ do
    (target, heldGrowth, _, _, ready) <- board True
    let after = S.runPure (aiming target) ready (S.cast S.alice heldGrowth >> Stack.resolveTop)
    Spec.assertEqWith s "no counter, and the creature is still the 2/1 it was printed" (seenOn CounterKind.PlusOnePlusOne target after) (0, Just 2, Just 1)
  -- The same board without the enchantment. Differs in exactly that one
  -- permanent, which is what makes the refusal above the enchantment's doing.
  Spec.it s "and the same spell on the same board without it puts one on" $ do
    (target, heldGrowth, _, _, ready) <- board False
    let after = S.runPure (aiming target) ready (S.cast S.alice heldGrowth >> Stack.resolveTop)
    Spec.assertEqWith s "one counter, and CR 613.4c shows it" (seenOn CounterKind.PlusOnePlusOne target after) (1, Just 3, Just 2)
  -- CR 122.6's SECOND road: "counters put on that object while it's on the
  -- battlefield AND ... an object that's given counters as it enters". Agent's
  -- Toolkit enters with four counters, which reach the same door through
  -- Pawl.Engine.Event.flushEnteringCounters.
  Spec.it s "CR 122.6 an artifact given counters as it enters is given none" $ do
    (_, _, heldToolkit, _, ready) <- board True
    let after = S.runPure S.identityAnswer ready (S.cast S.alice heldToolkit >> Stack.resolveTop)
    case newestNamed toolkitName after of
      Just toolkit -> Spec.assertEqWith s "the entry line placed none of its four" (S.counterOf CounterKind.PlusOnePlusOne toolkit after, S.counterOf CounterKind.Shield toolkit after) (0, 0)
      Nothing -> Spec.assertFailure s "the artifact did not reach the battlefield"
  Spec.it s "and the same entry on the same board without it places them" $ do
    (_, _, heldToolkit, _, ready) <- board False
    let after = S.runPure S.identityAnswer ready (S.cast S.alice heldToolkit >> Stack.resolveTop)
    case newestNamed toolkitName after of
      Just toolkit -> Spec.assertEqWith s "the entry line placed all four" (S.counterOf CounterKind.PlusOnePlusOne toolkit after, S.counterOf CounterKind.Shield toolkit after) (1, 1)
      Nothing -> Spec.assertFailure s "the artifact did not reach the battlefield"
  -- The card's FIRST line, which is the PLAYER axis: Solemnity names every player
  -- and every kind, where Melira below names one of each. bob casts Prologue to
  -- Phyresis ({1}{U} instant: target opponent gets a poison counter, draw a card)
  -- at alice.
  Spec.it s "CR 101.2 players can't get counters" $ do
    (_, _, _, heldPrologue, ready) <- board True
    let after = S.runPure S.identityAnswer ready (S.cast S.bob heldPrologue >> Stack.resolveTop)
    Spec.assertEqWith s "no poison counter" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  Spec.it s "and the same spell on the same board without it gives one" $ do
    (_, _, _, heldPrologue, ready) <- board False
    let after = S.runPure S.identityAnswer ready (S.cast S.bob heldPrologue >> Stack.resolveTop)
    Spec.assertEqWith s "one poison counter" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 1

meliraSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
meliraSpec s registry = Spec.describe s "Melira, Sylvok Outcast" $ do
  let -- alice: four Swamps and two Forests, a Goblin Piker (2/1) of her own and
      -- Instill Infection ({3}{B} instant: put a -1/-1 counter on target
      -- creature, draw a card) plus Battlegrowth in hand; bob: two Islands, a
      -- Goblin Piker of his own and Prologue to Phyresis ({1}{U} instant: target
      -- opponent gets a poison counter, draw a card) in hand. Both libraries hold
      -- three Plains, since both spells draw. `outcast` says whether Melira is on
      -- the battlefield under alice's control.
      board outcast = do
        forest <- S.printingOf s registry "Forest"
        island <- S.printingOf s registry "Island"
        plains <- S.printingOf s registry "Plains"
        swamp <- S.printingOf s registry "Swamp"
        piker <- S.printingOf s registry "Goblin Piker"
        elf <- S.printingOf s registry "Glistener Elf"
        infection <- S.printingOf s registry "Instill Infection"
        battlegrowth <- S.printingOf s registry "Battlegrowth"
        prologue <- S.printingOf s registry "Prologue to Phyresis"
        melira <- S.printingOf s registry "Melira, Sylvok Outcast"
        let base = S.landsFor forest S.alice 2 (S.landsFor island S.bob 2 (S.landsInPlay swamp 4))
            (mine, g1) = S.addCreature piker S.alice base
            (theirs, g2) = S.addCreature piker S.bob g1
            (myElf, g3) = S.addCreature elf S.alice g2
            (theirElf, g4) = S.addCreature elf S.bob g3
            (heldInfection, g5) = S.addHandCard infection S.alice g4
            (heldGrowth, g6) = S.addHandCard battlegrowth S.alice g5
            (heldPrologue, g7) = S.addHandCard prologue S.bob g6
            g8 = stock plains S.alice (stock plains S.bob g7)
            g9 = if outcast then snd (S.addCreature melira S.alice g8) else g8
        pure (mine, theirs, myElf, theirElf, heldInfection, heldGrowth, heldPrologue, g9)
  -- THE KIND SCOPING, as a pair of casts on ONE board and ONE creature. A
  -- prohibition that ignored Pawl.Types.CounterRestriction's kind field would
  -- refuse both.
  Spec.it s "CR 101.2 a -1/-1 counter can't be put on a creature you control" $ do
    (mine, _, _, _, heldInfection, _, _, ready) <- board True
    let after = S.runPure (aiming mine) ready (S.cast S.alice heldInfection >> Stack.resolveTop)
    Spec.assertEqWith s "no -1/-1 counter, and the creature is still the 2/1 it was printed" (seenOn CounterKind.MinusOneMinusOne mine after) (0, Just 2, Just 1)
  Spec.it s "while a +1/+1 counter on that same creature still lands" $ do
    (mine, _, _, _, _, heldGrowth, _, ready) <- board True
    let after = S.runPure (aiming mine) ready (S.cast S.alice heldGrowth >> Stack.resolveTop)
    Spec.assertEqWith s "one +1/+1 counter, and CR 613.4c shows it" (seenOn CounterKind.PlusOnePlusOne mine after) (1, Just 3, Just 2)
  -- THE CONTROLLER SCOPING. The same spell, the same board, a creature bob
  -- controls: "creatures YOU control" is a filter and not a wipe.
  Spec.it s "CR 101.2 a -1/-1 counter still lands on a creature an opponent controls" $ do
    (_, theirs, _, _, heldInfection, _, _, ready) <- board True
    let after = S.runPure (aiming theirs) ready (S.cast S.alice heldInfection >> Stack.resolveTop)
    Spec.assertEqWith s "one -1/-1 counter, and CR 613.4c shows it" (seenOn CounterKind.MinusOneMinusOne theirs after) (1, Just 1, Just 0)
  -- The card's FIRST line, on the player axis, and the same two scopings again:
  -- Melira names one player ("you") and one kind (poison).
  Spec.it s "CR 101.2 you can't get poison counters" $ do
    (_, _, _, _, _, _, heldPrologue, ready) <- board True
    let after = S.runPure S.identityAnswer ready (S.cast S.bob heldPrologue >> Stack.resolveTop)
    Spec.assertEqWith s "Melira's controller got none" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 0
  Spec.it s "and the same spell on the same board without her gives one" $ do
    (_, _, _, _, _, _, heldPrologue, ready) <- board False
    let after = S.runPure S.identityAnswer ready (S.cast S.bob heldPrologue >> Stack.resolveTop)
    Spec.assertEqWith s "one poison counter" (S.playerCounterOf PlayerCounterKind.Poison S.alice after) 1
  -- The card's THIRD line, which is what makes the transcription whole. A pair
  -- of Glistener Elves, one on each side of the table.
  Spec.it s "CR 613.1f creatures your opponents control lose infect" $ do
    (_, _, myElf, theirElf, _, _, _, ready) <- board True
    Spec.assertEqWith s "bob's Elf lost it, alice's kept it" (Projection.hasKeyword Keyword.Infect theirElf ready, Projection.hasKeyword Keyword.Infect myElf ready) (False, True)

moveSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
moveSpec s registry = Spec.describe s "CR 122.5 moving a counter" $ do
  let -- Agent's Toolkit ("whenever a creature you control enters, you may move a
      -- counter from this artifact onto that creature") already on the
      -- battlefield bearing ONE +1/+1 counter, alice holding a Goblin Piker to
      -- make a creature enter, and Solemnity present or not. The counter is
      -- placed by the fixture rather than by the card's own entry line, because
      -- Solemnity would refuse that placement too and both boards must start with
      -- the same counter on the artifact.
      board enchanted = do
        forest <- S.printingOf s registry "Forest"
        mountain <- S.printingOf s registry "Mountain"
        plains <- S.printingOf s registry "Plains"
        toolkit <- S.printingOf s registry "Agent's Toolkit"
        piker <- S.printingOf s registry "Goblin Piker"
        solemnity <- S.printingOf s registry "Solemnity"
        let base = S.landsFor forest S.alice 2 (S.landsFor mountain S.alice 2 (S.landsInPlay plains 3))
            (artifact, g1) = S.addCreature toolkit S.alice base
            g2 = S.addCounter CounterKind.PlusOnePlusOne 1 artifact g1
            (heldPiker, g3) = S.addHandCard piker S.alice g2
            g4 = if enchanted then snd (S.addCreature solemnity S.alice g3) else g3
        pure (artifact, heldPiker, g4)
      -- Cast the creature, let it enter, then let the artifact's trigger reach
      -- the stack and resolve. settleForPriority is where CR 704's state-based
      -- actions run and the trigger is placed, in that order.
      play (artifact, heldPiker, ready) =
        let after =
              S.runPure
                (aiming artifact)
                ready
                (S.cast S.alice heldPiker >> Stack.resolveTop >> Engine.settleForPriority >> Stack.resolveTop)
         in (artifact, newestNamed pikerName after, after)
  -- CR 122.5's THIRD impossibility -- "the second object can't have counters put
  -- onto it" -- with rule 122.5's atomicity read against it: "if either of these
  -- actions isn't possible, it's not possible to move a counter, and NO COUNTER
  -- IS REMOVED FROM or put onto anything". The counter staying on the artifact is
  -- the whole assertion; a gate at the placement alone would have taken it off.
  Spec.it s "a destination that can't have counters put onto it moves nothing" $ do
    built <- board True
    case play built of
      (artifact, Just entered, after) -> do
        Spec.assertEqWith s "the counter is still on the artifact, not removed from it" (S.counterOf CounterKind.PlusOnePlusOne artifact after) 1
        Spec.assertEqWith s "and the creature that entered got none" (seenOn CounterKind.PlusOnePlusOne entered after) (0, Just 2, Just 1)
      _ -> Spec.assertFailure s "the creature did not reach the battlefield"
  Spec.it s "and the same trigger on the same board without it moves the counter" $ do
    built <- board False
    case play built of
      (artifact, Just entered, after) -> do
        Spec.assertEqWith s "the counter left the artifact" (S.counterOf CounterKind.PlusOnePlusOne artifact after) 0
        Spec.assertEqWith s "and landed on the creature that entered" (seenOn CounterKind.PlusOnePlusOne entered after) (1, Just 3, Just 2)
      _ -> Spec.assertFailure s "the creature did not reach the battlefield"

pikerName :: CardName.CardName
pikerName = CardName.MkCardName (Text.pack "Goblin Piker")
