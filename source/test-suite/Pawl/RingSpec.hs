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

-- CR 205.4 off the PROJECTION, which is where CR 613.1d's layer 4 writes a granted
-- supertype -- never off the printed type line, which is what the Ring-bearer case
-- below is about.
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
  -- CR 701.54a's "YOUR Ring-bearer", which is what makes the first ending
  -- per-player: alice designating a creature ends alice's previous designation and
  -- must not touch bob's. The guard that does it is `designate`'s `== Just pid`,
  -- and every other case here tempts one player only -- so a blanket clear would
  -- pass all of them.
  Spec.it s "CR 701.54a two players each keep their own Ring-bearer" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    let (_, g1) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (aliceCreature, g2) = S.addCreature piker S.alice g1
        -- bob's own side, one object at a time: S.landsInPlay only ever fills
        -- alice's, and Birthday Escape's draw half needs bob a library to draw from
        -- or CR 704.5b takes him out of the game before anything can be asserted.
        (_, g3) = S.addCreature island S.bob g2
        (_, g4) = S.addLibraryCard piker S.bob g3
        (bobCreature, g5) = S.addCreature piker S.bob g4
        (g6, aliceSpell) = S.handOne escape g5
        (bobSpell, g7) = S.addHandCard escape S.bob g6
        aliceTempted = castAndResolve S.identityAnswer S.alice aliceSpell g7
        bobsTurn = aliceTempted {GameState.activePlayer = S.bob, GameState.priority = Just S.bob}
        bothTempted = castAndResolve S.identityAnswer S.bob bobSpell bobsTurn
    -- Anti-vacuity: bob's temptation has to have happened at all, or "alice kept
    -- hers" is true of a board where nothing touched it.
    Spec.assertEqWith s "bob really was tempted" (temptationsOf S.bob bothTempted) (Just 1)
    Spec.assertEqWith s "bob designated his own creature" (markedFor S.bob bothTempted) [bobCreature]
    Spec.assertEqWith s "and alice still has hers" (markedFor S.alice bothTempted) [aliceCreature]
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
    -- Anti-vacuity: "only the original is marked" is trivially true of a board
    -- where the Clone never entered, or entered as a 0/0 and died to CR 704.5f. It
    -- entered AND copied exactly when a permanent that is not the bearer projects
    -- the bearer's name (CR 707.2 makes the name copiable; a Clone that copied
    -- nothing projects "Clone"). Both assertions fail if the copy answer is
    -- replaced by S.identityAnswer, which declines.
    Spec.assertBool s (newest /= Just bearer) "the Clone really is a different object"
    Spec.assertEqWith
      s
      "and it really copied the Ring-bearer"
      (fmap (\oid -> Projection.nameOf oid copied) newest)
      (Just (Projection.nameOf bearer copied))
    Spec.assertEqWith s "the original is still the Ring-bearer" (markedFor S.alice copied) [bearer]
    Spec.assertEqWith s "and nothing else carries the designation" (length (markedFor S.alice copied)) 1
  -- CR 701.54c's base tier: the emblem has "Your Ring-bearer is legendary and can't
  -- be blocked by creatures with greater power." The FIRST clause is what this case
  -- proves -- a layer-4 supertype grant (CR 613.1d, CR 205.4b) whose affected set is
  -- CR 701.54e's Ring-bearer, carried by an emblem in the command zone (CR 114.4).
  --
  -- Observed through CR 205.4e's legendary-spell cast restriction rather than
  -- through CR 704.5j's legend rule: making ONE creature legendary does not fire the
  -- legend rule, which needs two same-named legendary permanents under one
  -- controller, while CR 205.4e reads "controls a legendary creature" off the
  -- PROJECTION and so sees the grant.
  --
  -- Both creatures are Goblin Pikers, which is deliberate on both counts: nothing
  -- printed here is legendary, so assertion one has nothing else to satisfy it, and
  -- only one of the two is ever the Ring-bearer, so CR 704.5j never has a pair to
  -- act on.
  --
  -- ANTI-VACUITY. "The legendary sorcery is not castable" is a cast gate, and a cast
  -- gate reads False for a dozen reasons that have nothing to do with CR 205.4e --
  -- wrong phase, unpayable cost, a non-empty stack. `withThalia` is the same board
  -- plus the pool's one printed legendary creature, with no Ring anywhere: it
  -- asserts True, so the negative below discriminates.
  Spec.it s "CR 701.54c the Ring-bearer is legendary, which CR 205.4e sees" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    sorcery <- S.printingOf s registry "Synthetic Legendary Sorcery"
    thalia <- S.printingOf s registry "Thalia, Guardian of Thraben"
    let withLibrary = List.foldl' (\g _ -> snd (S.addLibraryCard piker S.alice g)) (S.landsInPlay island 2) [1 .. (2 :: Int)]
        (a, g1) = S.addCreature piker S.alice withLibrary
        (b, g2) = S.addCreature piker S.alice g1
        -- Ring.tempt sorts its candidates, so name them in that order.
        (lower, higher) = if a < b then (a, b) else (b, a)
        (g3, firstEscape) = S.handOne escape g2
        (secondEscape, g4) = S.addHandCard escape S.alice g3
        (sorceryId, gs) = S.addHandCard sorcery S.alice g4
        withThalia = snd (S.addCreature thalia S.alice gs)
        -- The first temptation takes the LAST candidate, the second the FIRST, so
        -- the grant has to move backwards along the list.
        tempted = castAndResolve lastCandidate S.alice firstEscape gs
        moved = castAndResolve S.identityAnswer S.alice secondEscape tempted
    Spec.assertBool s (not (S.castable S.alice sorceryId gs)) "CR 205.4e refuses the legendary sorcery before the Ring"
    Spec.assertBool s (S.castable S.alice sorceryId withThalia) "but allows it beside a printed legendary creature, so the refusal above is CR 205.4e's"
    Spec.assertEqWith s "the chosen creature is the Ring-bearer" (markedFor S.alice tempted) [higher]
    Spec.assertBool s (isLegendary higher tempted) "CR 701.54c makes the Ring-bearer legendary"
    Spec.assertBool s (not (isLegendary lower tempted)) "and reaches nothing else"
    Spec.assertBool s (S.castable S.alice sorceryId tempted) "so CR 205.4e now allows the legendary sorcery"
    -- CR 701.54a's first ending, read through the grant: the emblem's affected set is
    -- re-derived every projection, so moving the designation moves the supertype.
    Spec.assertEqWith s "the designation moved" (markedFor S.alice moved) [lower]
    Spec.assertBool s (isLegendary lower moved) "the new Ring-bearer is legendary"
    Spec.assertBool s (not (isLegendary higher moved)) "and the old one stopped being"
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
  -- it. The two assertions above the claim are the anti-vacuity pair -- the mark has
  -- to still be there, and control has to have actually moved, or "not legendary" is
  -- true for a reason that is not the control clause.
  Spec.it s "CR 701.54e a stolen Ring-bearer is not legendary while its mark survives" $ do
    island <- S.printingOf s registry "Island"
    piker <- S.printingOf s registry "Goblin Piker"
    escape <- S.printingOf s registry "Birthday Escape"
    treason <- S.printingOf s registry "Act of Treason"
    mountain <- S.printingOf s registry "Mountain"
    let (_, withLibrary) = S.addLibraryCard piker S.alice (S.landsInPlay island 1)
        (bearer, g1) = S.addCreature piker S.alice withLibrary
        withBobsLands = List.foldl' (\g _ -> snd (S.addCreature mountain S.bob g)) g1 [1 .. (3 :: Int)]
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
