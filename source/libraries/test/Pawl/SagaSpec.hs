{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Saga, rule 714: the chapter ability CR 714.2b writes out,
-- the lore counter CR 714.3a makes a Saga enter with, the turn-based action CR
-- 505.4 / 703.4f / 714.3c runs as a precombat main phase begins, and the
-- state-based action CR 704.5s performs once the story is told.
--
-- Also the pieces rule 714 needed underneath it, exercised here because this is
-- where a card reaches them: CounterKind.Lore, GameEvent.CountersPut (the CR
-- 122.6 record) and TriggerCondition.SelfCountersReached.
--
-- Two cards. History of Benalia carries every group but the last: chapters I and
-- II create a 2/2 white Knight token with vigilance -- CR 714.2c's "I, II --"
-- shorthand, written as the two abilities that rule says it means -- and chapter
-- III gives Knights its controller controls +2/+1 until end of turn. Love Song of
-- Night and Day carries the Read ahead group, and rule 702.155 is the whole
-- reason it is here.
--
-- `data/cards` holds a third Saga, Old Fat Spider Can't See Me, whose four
-- chapters run under Pawl.ExpirySpec's OldFatSpiderCantSeeMe group: rule 714 is
-- read here, and that group reads what its chapters' CR 611.2b durations do.
module Pawl.SagaSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Saga as Saga
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Saga" $ do
  entrySpec s registry
  chapterSpec s registry
  advanceSpec s registry
  sacrificeSpec s registry
  readAheadSpec s registry

knightToken :: CardName.CardName
knightToken = CardName.MkCardName (Text.pack "Knight Token")

birdToken :: CardName.CardName
birdToken = CardName.MkCardName (Text.pack "Bird Token")

-- One lore counter goes on as the Saga enters (CR 714.3a), which fires chapter I
-- (CR 714.2b). Both halves are visible from a cast, which is what makes this the
-- gameplay-level test rather than a projection one.
entrySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
entrySpec s registry = Spec.describe s "Entry" $ do
  Spec.it s "CR 714.3a History of Benalia enters with a lore counter on it" $ do
    plains <- S.printingOf s registry "Plains"
    benalia <- S.printingOf s registry "History of Benalia"
    let (gs, spellId) = S.handOne benalia (S.landsInPlay plains 3)
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        after = S.runPure S.identityAnswer cast Stack.resolveTop
    case sagaOf after of
      Nothing -> Spec.assertFailure s "History of Benalia did not reach the battlefield"
      Just oid ->
        Spec.assertEqWith s "one lore counter" (S.counterOf CounterKind.Lore oid after) 1
  Spec.it s "CR 714.2b that counter fires chapter I, which makes a 2/2 Knight with vigilance" $ do
    plains <- S.printingOf s registry "Plains"
    benalia <- S.printingOf s registry "History of Benalia"
    let (gs, spellId) = S.handOne benalia (S.landsInPlay plains 3)
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        resolved = S.runPure S.identityAnswer cast Stack.resolveTop
        -- The chapter ability is gathered and placed by the settle loop, then
        -- resolved by the priority round: CR 714.3a's counter goes on inside the
        -- zone change, so nothing about this is special-cased for entry.
        after = S.runPure S.identityAnswer resolved Engine.priorityLoop
    Spec.assertEqWith s "one Knight token" (S.countOnBattlefieldByName knightToken S.alice after) 1
    case S.tokensOf after of
      [token] -> do
        Spec.assertEqWith s "2/2" (S.powerToughnessOf token after) (Just (2, 2))
        Spec.assertEqWith s "a Knight" (Projection.subtypesOf token after) (Set.singleton Subtype.Knight)
        Spec.assertBool s (Map.member Keyword.Vigilance (Projection.keywordsOf token after)) "with vigilance"
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))

-- CR 714.2b's threshold crossing, at the level Pawl.Engine.Saga states it. These
-- are the arithmetic THE SBA AND THE TRIGGER MATCHER SHARE, so a drift between
-- them shows up here first.
chapterSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
chapterSpec s registry = Spec.describe s "Chapters" $ do
  Spec.it s "CR 714.2b a chapter is crossed going up through it and never again" $ do
    -- Chapter II: crossed by 1 -> 2 and by 0 -> 3, not by 2 -> 3 or 2 -> 2.
    Spec.assertBool s (Saga.crossed 1 2 2) "1 -> 2 crosses II"
    Spec.assertBool s (Saga.crossed 0 3 2) "0 -> 3 crosses II as well as I and III"
    Spec.assertBool s (not (Saga.crossed 2 3 2)) "2 -> 3 does NOT cross II again"
    Spec.assertBool s (not (Saga.crossed 2 2 2)) "and a placement of nothing crosses nothing"
    -- THE FALSIFIER for writing the condition as "the count is now at least N",
    -- which would re-fire every chapter on every later counter.
    Spec.assertBool s (not (Saga.crossed 3 4 1)) "chapter I is not re-crossed at four counters"
  Spec.it s "CR 714.2d the final chapter number is the greatest chapter, and 0 with none" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    plains <- S.printingOf s registry "Plains"
    let (oid, gs) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        (landId, withLand) = S.addCreature plains S.bob gs
        pcs = Projection.projectAll withLand
    case Map.lookup oid pcs of
      Nothing -> Spec.assertFailure s "the Saga has no projection"
      Just pc -> do
        Spec.assertEqWith s "chapters I, II and III" (Saga.chaptersOf pc) [1, 2, 3]
        Spec.assertEqWith s "final chapter III" (Saga.finalChapterOf pc) 3
        Spec.assertBool s (Saga.isSaga pc) "and it is a Saga"
        Spec.assertBool s (Saga.tracksLore pc) "with chapter abilities, so it advances"
    -- CR 714.2d's second sentence, on a permanent that is no Saga at all: the
    -- fold answers 0 rather than being undefined over an empty set, which is the
    -- rule legislating the empty maximum card-shape by card-shape.
    case Map.lookup landId pcs of
      Nothing -> Spec.assertFailure s "the Plains has no projection"
      Just pc -> do
        Spec.assertEqWith s "a Plains has no chapters" (Saga.finalChapterOf pc) 0
        Spec.assertBool s (not (Saga.isSaga pc)) "and is not a Saga"
        Spec.assertBool s (not (Saga.tracksLore pc)) "so nothing advances it"

-- CR 505.4 / 703.4f / 714.3c: the turn-based action.
advanceSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
advanceSpec s registry = Spec.describe s "The precombat main phase" $ do
  -- Driven through Engine.runStep rather than by calling runTurnBasedActions,
  -- which is what proves the arm is actually WIRED: every other case in this
  -- group calls the action directly, and an arm the step machinery stopped
  -- reaching would leave all of them green.
  Spec.it s "CR 505.4 the turn machinery itself runs the action as the step begins" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        withCounter = S.addCounter CounterKind.Lore 1 oid base
        gs = precombatMainOf S.alice withCounter
        after = S.runPure S.identityAnswer gs Engine.runStep
    Spec.assertEqWith s "the step put the second lore counter on" (S.counterOf CounterKind.Lore oid after) 2
    Spec.assertEqWith s "and the step really was the precombat main phase" (GameState.phase gs) Phase.PrecombatMain
  Spec.it s "CR 714.3c the active player puts a lore counter on each Saga they control" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        withCounter = S.addCounter CounterKind.Lore 1 oid base
        gs = precombatMainOf S.alice withCounter
        after = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions Phase.PrecombatMain)
    Spec.assertEqWith s "a second lore counter" (S.counterOf CounterKind.Lore oid after) 2
  Spec.it s "CR 714.3c and it fires the chapter that counter crossed, not the ones below it" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        withCounter = S.addCounter CounterKind.Lore 1 oid base
        gs = precombatMainOf S.alice withCounter
        advanced = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions Phase.PrecombatMain)
        after = S.runPure S.identityAnswer advanced Engine.priorityLoop
    -- Chapter II made one Knight. Chapter I did NOT fire again, which is the
    -- whole content of CR 714.2b's "was less than N": the count went 1 -> 2, and
    -- one is not less than one.
    Spec.assertEqWith s "exactly one Knight token" (S.countOnBattlefieldByName knightToken S.alice after) 1
  -- CR 614.16 decides which of the two placements Doubling Season reaches, and
  -- they differ: the ENTRY counter comes from CR 714.3a's replacement effect, which
  -- CR 614.16 admits by name ("they also apply if another replacement or prevention
  -- effect does so"), while the turn-based action is the result of no spell or
  -- ability at all (CR 609.1) and so is reached by nothing.
  --
  -- The FALSIFIER for putting every counter placement through the CR 616.1 loop,
  -- which is what Pawl.Types.CounterCause exists to prevent.
  Spec.it s "CR 614.16 Doubling Season doubles the entering lore counter but NOT the turn-based one" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    doublingSeason <- S.printingOf s registry "Doubling Season"
    plains <- S.printingOf s registry "Plains"
    let (_, withSeason) = S.addCreature doublingSeason S.alice (S.landsInPlay plains 3)
        (gs, spellId) = S.handOne benalia withSeason
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        entered = S.runPure S.identityAnswer cast Stack.resolveTop
    case sagaOf entered of
      Nothing -> Spec.assertFailure s "History of Benalia did not reach the battlefield"
      Just oid -> do
        -- CR 714.3a's one counter, doubled: chapters I and II both cross at once.
        Spec.assertEqWith s "two lore counters on entry" (S.counterOf CounterKind.Lore oid entered) 2
        let advanced = S.runPure S.identityAnswer (precombatMainOf S.alice entered) (Engine.runTurnBasedActions Phase.PrecombatMain)
        -- Three, not four. A doubled turn-based action would land on four and take
        -- the Saga straight past its final chapter.
        Spec.assertEqWith s "and the turn-based action adds ONE, not two" (S.counterOf CounterKind.Lore oid advanced) 3
  -- The OTHER side of the same distinction, and why the cause reaches the rows
  -- rather than the funnel's door (#847): Vorinclex, Monstrous Raider says "if YOU
  -- would put one or more counters on a permanent", and CR 714.3c has "that
  -- player" put the turn-based lore counter -- so this clause reaches it where
  -- Doubling Season's "if an effect would" does not.
  Spec.it s "CR 714.3c Vorinclex DOES double the turn-based lore counter" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    vorinclex <- S.printingOf s registry "Vorinclex, Monstrous Raider"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        (_, withPraetor) = S.addCreature vorinclex S.alice base
        gs = precombatMainOf S.alice withPraetor
        after = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions Phase.PrecombatMain)
        bare = S.runPure S.identityAnswer (precombatMainOf S.alice base) (Engine.runTurnBasedActions Phase.PrecombatMain)
    Spec.assertEqWith s "two lore counters, not one" (S.counterOf CounterKind.Lore oid after) 2
    Spec.assertEqWith s "and one without the praetor" (S.counterOf CounterKind.Lore oid bare) 1
  Spec.it s "CR 714.3c a Saga its controller does not control the turn of stays put" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.bob (Setup.emptyGame S.bothPlayers)
        withCounter = S.addCounter CounterKind.Lore 1 oid base
        -- ALICE's precombat main phase. CR 505.4 names the active player, and
        -- bob's Saga is not theirs to advance.
        gs = precombatMainOf S.alice withCounter
        after = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions Phase.PrecombatMain)
    Spec.assertEqWith s "bob's Saga still has one lore counter" (S.counterOf CounterKind.Lore oid after) 1

-- CR 704.5s / 714.4: the state-based action.
sacrificeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sacrificeSpec s registry = Spec.describe s "The final chapter" $ do
  Spec.it s "CR 704.5s a Saga at its final chapter number is sacrificed by its controller" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        -- Three counters and NO unscanned placement event, so CR 704.5s's third
        -- conjunct is satisfied: nothing of this Saga's has triggered.
        gs = S.addCounter CounterKind.Lore 3 oid base
        after = S.settleSba gs
    Spec.assertBool s (not (S.onBattlefield oid after)) "the Saga left the battlefield"
    Spec.assertEqWith s "and is in its owner's graveyard, a sacrifice being a move to it (CR 701.21a)" (length (Game.zoneMembers Zone.Graveyard S.alice after)) 1
  Spec.it s "CR 704.5s but NOT while a chapter ability of its own is still on the stack" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    let (oid, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        withCounters = S.addCounter CounterKind.Lore 2 oid base
        gs = precombatMainOf S.alice withCounters
        -- The turn-based action takes it to three, which fires chapter III. The
        -- settle loop's SBA pass runs BEFORE placePendingTriggers, so this is the
        -- window CR 704.5s's exemption exists for.
        advanced = S.runPure S.identityAnswer gs (Engine.runTurnBasedActions Phase.PrecombatMain)
        settled = S.runPure S.identityAnswer advanced Engine.settleForPriority
    Spec.assertEqWith s "three lore counters" (S.counterOf CounterKind.Lore oid settled) 3
    -- Not merely "the stack is non-empty": the object on it must be a chapter
    -- ability OF THIS SAGA, which is the only thing CR 704.5s's exemption excuses.
    Spec.assertEqWith s "this Saga's chapter III is what is on the stack" (chaptersOnStackFrom oid settled) [3]
    Spec.assertBool s (S.onBattlefield oid settled) "and the Saga is still on the battlefield under it"
    -- THE FALSIFIER for dropping either half of Saga.awaitingChapter: without the
    -- unscanned-event half the Saga is gone before this line, and without the
    -- stack half it is gone by the next one.
    let resolved = S.runPure S.identityAnswer settled Engine.priorityLoop
    Spec.assertBool s (not (S.onBattlefield oid resolved)) "once chapter III has resolved, it is sacrificed"
  -- The whole story, chapter by chapter, which is the case that proves the four
  -- rules hang together rather than each working alone. Two precombat main
  -- phases, so the Knight chapter III pumps is one chapter II actually made.
  Spec.it s "CR 714 the Saga runs II then III, pumps its own Knight, and is sacrificed" $ do
    benalia <- S.printingOf s registry "History of Benalia"
    piker <- S.printingOf s registry "Goblin Piker"
    let (sagaId, base) = S.addCreature benalia S.alice (Setup.emptyGame S.bothPlayers)
        -- Goblin Piker is the CONTROL: a 2/1 alice controls that is no Knight,
        -- so chapter III's "Knights you control" must leave it alone.
        (pikerId, withPiker) = S.addCreature piker S.alice base
        withCounter = S.addCounter CounterKind.Lore 1 sagaId withPiker
        advance g = S.runPure S.identityAnswer (precombatMainOf S.alice g) (Engine.runTurnBasedActions Phase.PrecombatMain)
        resolveAll g = S.runPure S.identityAnswer g Engine.priorityLoop
        -- One to two: chapter II mints the Knight.
        afterTwo = resolveAll (advance withCounter)
        -- Two to three: chapter III pumps it, and CR 704.5s then takes the Saga.
        afterThree = resolveAll (advance afterTwo)
    Spec.assertEqWith s "chapter II made one Knight" (S.countOnBattlefieldByName knightToken S.alice afterTwo) 1
    Spec.assertBool s (S.onBattlefield sagaId afterTwo) "and the Saga is still telling its story"
    case S.tokensOf afterThree of
      [token] -> do
        Spec.assertEqWith s "the Knight is 4/3 -- 2/2 plus chapter III's +2/+1" (S.powerToughnessOf token afterThree) (Just (4, 3))
        Spec.assertEqWith s "and still a Knight" (Projection.subtypesOf token afterThree) (Set.singleton Subtype.Knight)
      other -> Spec.assertFailure s ("expected exactly one Knight token, got " <> show (length other))
    Spec.assertEqWith s "the Goblin Piker, no Knight, is untouched at 2/1" (S.powerToughnessOf pikerId afterThree) (Just (2, 1))
    Spec.assertBool s (not (S.onBattlefield sagaId afterThree)) "and CR 704.5s has taken the Saga, its final chapter told"

-- The battlefield's one History of Benalia, by the subtype rule 714 keys on.
sagaOf :: GameState.GameState -> Maybe ObjectId.ObjectId
sagaOf gs =
  let pcs = Projection.projectAll gs
      sagas = filter (\oid -> maybe False Saga.isSaga (Map.lookup oid pcs)) (Set.toAscList (GameState.battlefield gs))
   in case sagas of
        [oid] -> Just oid
        _ -> Nothing

-- The chapter numbers of `oid`'s own chapter abilities currently on the stack --
-- CR 704.5s's "the source of a chapter ability that has triggered but not yet left
-- the stack", read back so a test can name WHICH chapter is waiting.
chaptersOnStackFrom :: ObjectId.ObjectId -> GameState.GameState -> [Natural]
chaptersOnStackFrom oid gs =
  let from sid = case fmap Object.source (Game.lookupObject sid gs) of
        Just (Source.OfTrigger triggered) | TriggeredAbilitySource.source triggered == oid -> Saga.chapterOf (TriggeredAbilitySource.ability triggered)
        _ -> Nothing
   in Maybe.mapMaybe from (GameState.stack gs)

-- A board sitting in `pid`'s precombat main phase, which is the moment CR 505.4
-- names.
precombatMainOf :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
precombatMainOf pid gs =
  gs
    { GameState.phase = Phase.PrecombatMain,
      GameState.activePlayer = pid,
      GameState.priority = Just pid
    }

-- CR 702.155 / 714.3b: read ahead. Love Song of Night and Day is the producer --
-- {2}{W}, read ahead, chapter I "you and target opponent each draw two cards",
-- chapter II a 1\/1 white Bird with flying, chapter III a +1\/+1 counter on each of
-- up to two target creatures.
--
-- The chapter ANSWERED is II, and the choice of board is the whole test. Answering
-- I makes every quantity below identical under the read-ahead reading and the
-- unimplemented one -- one lore counter, chapter I fires, no Bird -- so the prompt
-- would be raised and its answer unobservable.
readAheadSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
readAheadSpec s registry = Spec.describe s "Read ahead" $ do
  Spec.it s "CR 714.3b / 702.155a chapter II is chosen, so the Saga enters on II and chapter I never triggers" $ do
    plains <- S.printingOf s registry "Plains"
    loveSong <- S.printingOf s registry "Love Song of Night and Day"
    let (gs, spellId) = S.handOne loveSong (stocked plains)
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        resolved = S.runPure (answeringChapter 2) cast Stack.resolveTop
        -- CR 704.5 and the trigger scan both run at the settle, and the priority
        -- round is what drains whatever the scan placed. Without it "chapter I
        -- did not trigger" could not be told from "has not resolved yet".
        after = S.runPure (answeringChapter 2) resolved Engine.priorityLoop
    case sagaOf after of
      Nothing -> Spec.assertFailure s "Love Song of Night and Day did not reach the battlefield"
      Just oid -> Spec.assertEqWith s "CR 714.3b two lore counters, the chosen chapter" (S.counterOf CounterKind.Lore oid after) 2
    -- CR 702.155a: the 0 -> 2 placement crosses chapter I under rule 714.2b's
    -- "at least N", and read ahead is what stops it triggering. Asserted BEFORE
    -- the Bird, so a fix that places the counters but never narrows the matcher
    -- reddens here rather than being absorbed downstream.
    Spec.assertEqWith s "CR 702.155a chapter I did not trigger, so alice drew nothing" (S.handSize S.alice after) 0
    Spec.assertEqWith s "CR 702.155a and neither did bob" (S.handSize S.bob after) 0
    -- CR 702.155a's "unless it has exactly the number of lore counters ...": the
    -- chapter that DOES match still triggers, so the narrowing is a narrowing and
    -- not a blanket suppression.
    Spec.assertEqWith s "CR 714.2b chapter II did trigger" (S.countOnBattlefieldByName birdToken S.alice after) 1
    case S.tokensOf after of
      [token] -> Spec.assertEqWith s "a 1/1 Bird" (S.powerToughnessOf token after) (Just (1, 1))
      other -> Spec.assertFailure s ("expected exactly one token, got " <> show (length other))
  Spec.it s "CR 704.5s entering on the final chapter still resolves it before the Saga is sacrificed" $ do
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    loveSong <- S.printingOf s registry "Love Song of Night and Day"
    let (pikerId, board) = S.addCreature piker S.alice (stocked plains)
        (gs, spellId) = S.handOne loveSong board
        cast = S.runPure S.identityAnswer gs (S.cast S.alice spellId)
        resolved = S.runPure (answeringChapter 3) cast Stack.resolveTop
        after = S.runPure (answeringChapter 3) resolved Engine.priorityLoop
    -- CR 702.155a again, with the SBA in the way: chapters I and II are skipped,
    -- and chapter III must reach the stack and resolve before CR 704.5s takes the
    -- Saga -- Saga.awaitingChapter's exemption.
    Spec.assertEqWith s "CR 702.155a chapter I did not trigger" (S.handSize S.bob after) 0
    Spec.assertEqWith s "CR 702.155a chapter II did not trigger, so no Bird" (S.countOnBattlefieldByName birdToken S.alice after) 0
    Spec.assertEqWith s "CR 714.2b chapter III did, and it resolved" (S.counterOf CounterKind.PlusOnePlusOne pikerId after) 1
    case sagaOf resolved of
      Nothing -> Spec.assertFailure s "Love Song of Night and Day did not reach the battlefield"
      Just oid -> Spec.assertBool s (not (S.onBattlefield oid after)) "and only then did CR 704.5s take the Saga"

-- Answers CR 702.155b's chapter choice with `n` and nothing else, so the board
-- below differs from the default-answering one in exactly that.
answeringChapter :: Natural -> Prompt.Prompt r -> r
answeringChapter n p = case p of
  Prompt.ChooseReadAheadChapter {} -> n
  _ -> S.identityAnswer p

-- alice's three Plains, plus two library cards each: chapter I's draw must have
-- somewhere to draw FROM, or CR 104.3c ends the game for the drawing player and
-- the hand-size assertions pass because nobody was left rather than because
-- nothing was drawn.
stocked :: Printing.Printing -> GameState.GameState
stocked plains =
  let base = S.landsInPlay plains 3
      add gs pid = snd (S.addLibraryCard plains pid gs)
   in foldl add base [S.alice, S.alice, S.bob, S.bob]
