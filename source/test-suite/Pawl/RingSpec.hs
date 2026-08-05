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

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Expiry as Expiry
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Ring as Ring
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
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

-- Answers the as-enters copy choice (CR 614.12a) with `wanted` when it is offered,
-- delegating everything else to S.identityAnswer -- which DECLINES to copy, and a
-- Clone that copies nothing is a 0/0 that CR 704.5f removes before anything can be
-- asserted about it.
copyThe :: ObjectId -> Prompt.Prompt r -> r
copyThe wanted p = case p of
  Prompt.ChooseCopyTarget _ _ _ candidates -> if List.elem wanted candidates then Just wanted else Nothing
  _ -> S.identityAnswer p

-- alice controls two creatures, holds one Birthday Escape, has `lands` untapped
-- lands and one card left in her library to draw.
--
-- TWO creatures, never one: CR 701.54a's choice is only a real prompt with two or
-- more, so a one-creature board would let a never-prompts implementation pass.
twoCreatureBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Int -> (ObjectId, ObjectId, ObjectId, GameState.GameState)
twoCreatureBoard island piker escape lands =
  let (_, g0) = S.addLibraryCard piker S.alice (S.landsInPlay island lands)
      (a, g1) = S.addCreature piker S.alice g0
      (b, g2) = S.addCreature piker S.alice g1
      (gs, spellId) = S.handOne escape g2
      -- Ring.tempt sorts its candidates, so name them in that order rather than in
      -- the order they were added.
      (lower, higher) = if a < b then (a, b) else (b, a)
   in (lower, higher, spellId, gs)

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
  -- CR 701.54c's "if a player doesn't have an emblem named The Ring". The count
  -- climbs while the emblem does not multiply, which is what makes the two separate
  -- pieces of state rather than one.
  Spec.it s "CR 701.54c a second temptation counts again but mints no second emblem" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    let withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) (S.landsInPlay island 2) [1 .. (2 :: Int)]
        (_, g1) = S.addCreature piker S.alice withLibrary
        (g2, firstSpell) = S.handOne escape g1
        (secondSpell, g3) = S.addHandCard escape S.alice g2
        once = castAndResolve S.identityAnswer S.alice firstSpell g3
        twice = castAndResolve S.identityAnswer S.alice secondSpell once
    Spec.assertEqWith s "one emblem after the first temptation" (length (theRingsOf S.alice once)) 1
    Spec.assertEqWith s "still one emblem after the second" (length (theRingsOf S.alice twice)) 1
    Spec.assertEqWith s "but two temptations" (temptationsOf S.alice twice) (Just 2)
  -- CR 701.54a's FIRST ending: "until another creature becomes your Ring-bearer".
  -- The designation MOVES rather than accumulating, so `markedFor` stays a
  -- singleton and changes which creature it names.
  Spec.it s "CR 701.54a a second designation lifts the first" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    let (firstCreature, secondCreature, firstSpell, g1) = twoCreatureBoard island piker escape 2
        (secondSpell, g2) = S.addHandCard escape S.alice g1
        -- The first temptation takes the LAST candidate, the second the FIRST, so
        -- the designation has to move backwards along the list. An implementation
        -- that only ever added a mark would leave both set.
        once = castAndResolve lastCandidate S.alice firstSpell g2
        twice = castAndResolve S.identityAnswer S.alice secondSpell once
    Spec.assertEqWith s "the second creature was designated first" (markedFor S.alice once) [secondCreature]
    Spec.assertEqWith s "and the first creature holds it alone afterwards" (markedFor S.alice twice) [firstCreature]
  -- CR 701.54a's SECOND ending: "or another player gains control of it". Act of
  -- Treason's grant is UntilEndOfTurn, so this also pins that the designation does
  -- not come BACK when the loan does -- the reason Object.ringBearerFor remembers a
  -- player rather than being a bare flag, and the reason the sample only ever
  -- clears.
  Spec.it s "CR 701.54a another player gaining control ends the designation, and it does not return" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    treason <- S.printingOf s registry "Act of Treason"
    mountain <- S.printingOf s registry "Mountain"
    let (_, withLibrary) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (bearer, g1) = S.addCreature piker S.alice withLibrary
        -- bob's own lands, one at a time: S.landsInPlay only ever fills alice's
        -- side (S.addCreature puts a printing onto the battlefield whatever its
        -- card types are, despite the name). Mountains, because Act of Treason
        -- costs {2}{R}.
        withBobsLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.bob g)) g1 [1 .. (3 :: Int)]
        (g2, escapeId) = S.handOne escape withBobsLands
        (treasonId, g3) = S.addHandCard treason S.bob g2
        designated = castAndResolve S.identityAnswer S.alice escapeId g3
        bobsTurn = designated {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
        stolen = castAndResolve S.identityAnswer S.bob treasonId bobsTurn
        -- CR 514.2: the grant is armed AtCleanup, so dropping the cleanup-scoped
        -- effects is reaching the end of the turn as far as control is concerned.
        returned = S.runPure S.identityAnswer (Expiry.dropAtCleanup stolen) Engine.settleForPriority
    Spec.assertEqWith s "alice's Ring-bearer before the theft" (markedFor S.alice designated) [bearer]
    Spec.assertEqWith s "the steal really moved control (CR 613.1b)" (Projection.controllerOf bearer stolen) (Just S.bob)
    Spec.assertEqWith s "the designation ended when bob took the creature" (markedFor S.alice stolen) []
    Spec.assertBool s (not (Ring.isRingBearerOf S.bob bearer stolen)) "and bob did not inherit it"
    Spec.assertEqWith s "control came home" (Projection.controllerOf bearer returned) (Just S.alice)
    Spec.assertEqWith s "and the designation stayed ended" (markedFor S.alice returned) []
  -- CR 701.54b's second sentence: "Being a Ring-bearer is not a copiable value."
  -- CR 707.2's copiable values are what a Clone acquires, and the designation is
  -- not among them.
  Spec.it s "CR 701.54b a Clone of the Ring-bearer is not a Ring-bearer" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    clone <- S.printingOf s registry "Clone"
    let (_, withLibrary) = S.addLibraryCard piker S.alice (S.landsInPlay island 5)
        (bearer, g1) = S.addCreature piker S.alice withLibrary
        (g2, escapeId) = S.handOne escape g1
        (cloneId, g3) = S.addHandCard clone S.alice g2
        designated = castAndResolve S.identityAnswer S.alice escapeId g3
        copied = castAndResolve (copyThe bearer) S.alice cloneId designated
        -- The Clone is the newest object on the battlefield.
        newest = Maybe.listToMaybe (List.sortOn Ord.Down (Set.toList (GameState.battlefield copied)))
    Spec.assertBool s (newest /= Just bearer) "the Clone really is a different object"
    Spec.assertEqWith s "the original is still the Ring-bearer" (markedFor S.alice copied) [bearer]
    Spec.assertEqWith s "and nothing else carries the designation" (length (markedFor S.alice copied)) 1
