{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Engine.Ring (CR 701.54, "the Ring tempts you"), the two fields it
-- writes -- Pawl.Types.Object's ringBearerFor and Pawl.Types.Player's
-- ringTemptations -- and Pawl.Engine.Resolve's Effect.TemptWithTheRing arm.
--
-- Gameplay-level throughout: every case casts Birthday Escape ({U} Sorcery, "Draw
-- a card. The Ring tempts you.") through the stack rather than calling `tempt`
-- directly, so what is asserted is the whole path from card JSON to designation.
module Pawl.RingSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone

-- How many times the Ring has tempted this player (CR 701.54c).
temptationsOf :: PlayerId -> GameState.GameState -> Maybe Natural.Natural
temptationsOf pid gs = fmap Player.ringTemptations (Map.lookup pid (GameState.players gs))

-- Every command-zone object of this player's that is an emblem named The Ring (CR
-- 701.54c). A LIST rather than a Bool, because "did a second temptation mint a
-- second emblem?" is a question one case below asks and a Bool cannot answer.
theRingsOf :: PlayerId -> GameState.GameState -> [ObjectId]
theRingsOf pid gs =
  let named oid = fmap Face.name (Game.faceOf oid gs) == Just Ring.theRingName
   in filter named (Game.zoneMembers Zone.Command pid gs)

-- The objects carrying this player's Ring-bearer designation, read straight off
-- the field rather than through Ring.isRingBearerOf. The two differ exactly where
-- CR 701.54e's control clause bites, and the Act of Treason case below turns on
-- telling "the mark is gone" from "the mark is merely unreadable".
markedFor :: PlayerId -> GameState.GameState -> [ObjectId]
markedFor pid gs =
  filter
    (\oid -> fmap Object.ringBearerFor (Game.lookupObject oid gs) == Just (Just pid))
    (Map.keys (GameState.objects gs))

-- Cast the spell and resolve it, settling afterwards so CR 701.54a's
-- control-change sample (Ring.endOnControlChange) has run.
castAndResolve :: (forall r. Prompt.Prompt r -> r) -> PlayerId -> ObjectId -> GameState.GameState -> GameState.GameState
castAndResolve answer pid spellId gs =
  let cast = S.runPure answer gs (S.cast pid spellId)
   in S.runPure answer cast (Stack.resolveTop >> Engine.settleForPriority)

-- Answers Prompt.ChooseRingBearer with the LAST candidate offered, delegating
-- everything else to S.identityAnswer (which answers it with the FIRST).
--
-- The discriminator, and the reason it is not just S.identityAnswer: the candidate
-- list Ring.tempt builds is ascending, so an implementation that never prompted --
-- or that ignored the answer -- would designate the first creature. A case
-- asserting the SECOND one passes only for an implementation that asked and
-- honoured the reply.
lastCandidate :: Prompt.Prompt r -> r
lastCandidate p = case p of
  Prompt.ChooseRingBearer _ _ candidates -> NonEmpty.last candidates
  _ -> S.identityAnswer p

-- alice controls two creatures, holds one Birthday Escape, has `lands` untapped
-- lands and one card left in her library to draw.
--
-- TWO creatures, never one: CR 701.54a's choice is only a real prompt with two or
-- more, so a one-creature board would let a never-prompts implementation pass.
twoCreatureBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId, ObjectId, ObjectId, GameState.GameState)
twoCreatureBoard island piker escape lands =
  let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island lands)
      (a, g1) = S.addPermanent piker S.alice g0
      (b, g2) = S.addPermanent piker S.alice g1
      (gs, spellId) = S.handOne escape g2
      -- Ring.tempt sorts its candidates, so name them in that order rather than in
      -- the order they were added.
      (lower, higher) = if a < b then (a, b) else (b, a)
   in (lower, higher, spellId, gs)

-- CR 205.4: is this object legendary? Read off the PROJECTION, which is where CR
-- 613.1d's layer 4 writes a granted supertype -- never off the printed type line,
-- which for the two cases below says nothing about it either way.
isLegendary :: ObjectId -> GameState.GameState -> Bool
isLegendary oid gs = Set.member Supertype.Legendary (Projection.supertypesOf oid gs)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Ring" $ do
  -- The gate. Birthday Escape's other half (draw a card) was already implemented,
  -- so everything else asserted here is the temptation and nothing but.
  Spec.it s "CR 701.54 Birthday Escape draws, mints the emblem, and designates the creature its controller chose" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    let (firstCreature, secondCreature, spellId, gs) = twoCreatureBoard island piker escape 1
        after = castAndResolve lastCandidate S.alice spellId gs
    Spec.assertEqWith s "the other half still draws" (S.handSize S.alice after) 1
    -- CR 701.54c: the emblem, and exactly one of it.
    Spec.assertEqWith s "alice has an emblem named The Ring" (length (theRingsOf S.alice after)) 1
    -- CR 701.54a: the creature ALICE chose, not the first one on the board.
    Spec.assertEqWith s "the chosen creature is the Ring-bearer" (markedFor S.alice after) [secondCreature]
    -- CR 701.54e, through the rule's own predicate.
    Spec.assertBool s (Ring.isRingBearerOf S.alice secondCreature after) "CR 701.54e holds of the chosen creature"
    Spec.assertBool s (not (Ring.isRingBearerOf S.alice firstCreature after)) "and not of the other one"
    -- CR 701.54d: one temptation, and only for the player tempted.
    Spec.assertEqWith s "alice has been tempted once" (temptationsOf S.alice after) (Just 1)
    Spec.assertEqWith s "bob has not been tempted" (temptationsOf S.bob after) (Just 0)
    Spec.assertEqWith s "and bob got no emblem" (theRingsOf S.bob after) []
  -- CR 701.54d's whole point: "the Ring tempts a player whenever they complete the
  -- actions in 701.54a, even if some or all of those actions were impossible."
  --
  -- The case that breaks an implementation treating an unaskable choice as a failed
  -- temptation. Note what it still asserts: the emblem arrives (CR 701.54c is not
  -- conditional on the choice) and the count moves.
  Spec.it s "CR 701.54d a player with no creatures is still tempted" $ do
    island <- S.printingOf s registry "Island"
    escape <- S.printingOf s registry "Birthday Escape"
    let (_, g1) = S.addLibraryCard escape S.alice (S.landsInPlay island 1)
        (gs, spellId) = S.handOne escape g1
        after = castAndResolve S.identityAnswer S.alice spellId gs
    Spec.assertEqWith s "nobody is a Ring-bearer" (markedFor S.alice after) []
    Spec.assertEqWith s "the emblem arrived anyway" (length (theRingsOf S.alice after)) 1
    Spec.assertEqWith s "and the temptation counted" (temptationsOf S.alice after) (Just 1)
    -- The ABILITY-facing half of the same sentence, and the only reading of it a
    -- creatureless board can carry: the event a "whenever the Ring tempts you"
    -- trigger matches is recorded here too. It cannot be driven to a trigger on
    -- this board, the pool's one observer (Nazgul) being a creature its own
    -- controller would then have to choose.
    Spec.assertBool s (elem (GameEvent.RingTempted S.alice) (S.eventsOf after)) "and it recorded the temptation all the same"

  -- CR 701.54c's "if a player doesn't have an emblem named The Ring". The count
  -- climbs while the emblem does not multiply, which is what makes the two separate
  -- pieces of state rather than one.
  Spec.it s "CR 701.54c a second temptation counts again but mints no second emblem" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    let withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) (S.landsInPlay island 2) [1 .. (2 :: Int)]
        (_, g1) = S.addPermanent piker S.alice withLibrary
        (g2, firstSpell) = S.handOne escape g1
        (secondSpell, g3) = S.addHandCard escape S.alice g2
        once = castAndResolve S.identityAnswer S.alice firstSpell g3
        twice = castAndResolve S.identityAnswer S.alice secondSpell once
    Spec.assertEqWith s "one emblem after the first temptation" (length (theRingsOf S.alice once)) 1
    Spec.assertEqWith s "still one emblem after the second" (length (theRingsOf S.alice twice)) 1
    Spec.assertEqWith s "but two temptations" (temptationsOf S.alice twice) (Just 2)
  -- CR 701.54e's SECOND conjunct, "under your control", which
  -- Ring.theRingIsLegendary spells as a ControlledBy You beside the designation
  -- atom. The only window in which that conjunct is observable at all: after the
  -- control change and BEFORE Ring.endOnControlChange's settle-loop sample lifts the
  -- mark. Once the sample has run there is no designation left for the conjunct to
  -- reject, which is why this case resolves Act of Treason WITHOUT settling
  -- afterwards -- every other case here goes through castAndResolve, which settles.
  --
  -- What it pins: dropping the conjunct makes alice's stolen creature legendary FOR
  -- ALICE here, and CR 701.54e says it is not hers to read at all once bob controls
  -- it. The three assertions above the claim are the anti-vacuity set -- the grant
  -- has to have been there to begin with, the mark has to still be there, and
  -- control has to have actually moved, or "not legendary" is true for a reason that
  -- is not the control clause.
  Spec.it s "CR 701.54e a stolen Ring-bearer is not legendary while its mark survives" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    treason <- S.printingOf s registry "Act of Treason"
    mountain <- S.printingOf s registry "Mountain"
    let (_, withLibrary) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (bearer, g1) = S.addPermanent piker S.alice withLibrary
        withBobsLands = List.foldl' (\g _ -> snd (S.addPermanent mountain S.bob g)) g1 [1 .. (3 :: Int)]
        (g2, escapeId) = S.handOne escape withBobsLands
        (treasonId, g3) = S.addHandCard treason S.bob g2
        designated = castAndResolve S.identityAnswer S.alice escapeId g3
        bobsTurn = designated {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
        cast = S.runPure S.identityAnswer bobsTurn (S.cast S.bob treasonId)
        unsettled = S.runPure S.identityAnswer cast Stack.resolveTop
    Spec.assertBool s (isLegendary bearer designated) "alice's Ring-bearer is legendary before the theft"
    Spec.assertEqWith s "the mark has not been lifted yet" (markedFor S.alice unsettled) [bearer]
    Spec.assertEqWith s "but bob controls it already (CR 613.1b)" (Projection.controllerOf bearer unsettled) (Just S.bob)
    Spec.assertBool s (not (isLegendary bearer unsettled)) "CR 701.54e's control clause refuses it to alice"

  -- CR 701.54d's ability-facing half: "some abilities trigger 'Whenever the Ring
  -- tempts you'". Nazgul is its own tempt source and its own observer, so one
  -- printing drives the whole path -- the CR 603.6a entry trigger, the temptation
  -- it performs, the GameEvent.RingTempted that records it, and the trigger that
  -- reads it back.
  --
  -- TWO Nazgul and one Goblin Piker, which is what makes the counters
  -- discriminating rather than merely present. Both Nazgul are on the battlefield
  -- when the temptation happens, so CR 603.2 gives TWO triggers, and each puts one
  -- counter on EACH Wraith: a 1/2 that ends 3/4 is a Wraith both triggers reached,
  -- where a self-scoped effect would leave 2/3 and a single firing 2/3 as well.
  -- The Piker is the Filter's negative -- a creature alice controls that is not a
  -- Wraith -- and it also makes CR 701.54a's choice a real prompt.
  Spec.it s "CR 701.54d a temptation fires every 'whenever the Ring tempts you' watching it" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    nazgul <- S.printingOf s registry "Nazgûl"
    let (base, _) = S.handOne piker (S.landsInPlay island 0)
        (bystander, g1) = S.addPermanent piker S.alice base
        (standing, g2) = S.addPermanent nazgul S.alice g1
        (entrant, g3) = S.entersWithTrigger nazgul S.alice g2
        after = S.runPure S.identityAnswer g3 (Monad.replicateM_ (4 :: Int) (Engine.settleForPriority >> Stack.resolveTop) >> Engine.settleForPriority)
    Spec.assertEqWith s "CR 701.54d the entering Nazgul took a counter from each trigger" (S.powerToughnessOf entrant after) (Just (3, 4))
    Spec.assertEqWith s "and so did the Nazgul that was already there" (S.powerToughnessOf standing after) (Just (3, 4))
    Spec.assertEqWith s "while the Goblin Piker, no Wraith, took none" (S.powerToughnessOf bystander after) (Just (2, 1))
    -- Anti-vacuity: the temptation really happened, so "the counters arrived" is
    -- not "the entry trigger did something else".
    Spec.assertEqWith s "alice was tempted once" (temptationsOf S.alice after) (Just 1)
    Spec.assertEqWith s "and bob, who watched, was not tempted at all" (temptationsOf S.bob after) (Just 0)
