{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 706 ROLLING A DIE -- Pawl.Types.RollDie, Effect.RollDie's arm in
-- Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pair the
-- roll is externalised through. The transcript legs live in Pawl.ReplaySpec
-- with the other randomness prompts.
--
-- TWO FIXTURES, one per half of CR 706. Ancient Copper Dragon ("Flying /
-- Whenever this creature deals combat damage to a player, roll a d20. You create
-- a number of Treasure tokens equal to the result") is CR 706.4's, the result
-- read straight into a count; Djinni Windseer ("Flying / When this creature
-- enters, roll a d20. / 1-9 | Scry 1. / 10-19 | Scry 2. / 20 | Scry 3.") is CR
-- 706.3's results table, where the result selects an effect instead. Between
-- them that is the whole of CR 706 this file can reach: one die (#2085), no
-- modifier and no reroll (#2083), and no "Roll again" (#2124). CR 706.1's roll
-- does record its event now, but the trigger reading it lives in
-- Pawl.EventTriggerSpec beside the other condition cases.
--
-- THE ASSERTED QUANTITY on the DRAGON's boards is how many Treasure tokens alice
-- controls once combat damage has been dealt. It is the roll's result made
-- visible: nothing else on this board mints a token, bob controls nothing, and
-- the Treasure's own mana ability is never activated. Tokens enter under their
-- creator's control (CR 111.2) and alice is the only creator, so there is no
-- second trace to the same number.
--
-- FOUR BOARDS differing in ONE thing -- the number the interpreter answers with:
-- 13, 7, 20 and 21. 13 is the primary pin because it is none of the values a
-- wrong implementation could produce by accident: not 1 (Replay.defaultAnswer,
-- which S.identityAnswer falls through to), not 20 (the die's size), not 6 or 5
-- (the dragon's power and toughness) and not 0. The 7 leg falsifies a hard-coded
-- 13. The 20/21 pair straddles CR 706.1a's upper bound, which is the only place
-- an off-by-one on the face count is visible: 20 IS an outcome of a d20, and 21
-- is not.
--
-- TWO SEATS, not three: the roll's result is read by a Create scoped to "you",
-- so no clause here ranges over opponents, and a third seat would only add a
-- defending-player question. Bob controls nothing, because a blocker would keep
-- the combat damage off him and the trigger would never fire at all.
--
-- THE ASSERTED QUANTITY on the WINDSEER's boards is the identity of the top card
-- of alice's library after the trigger resolves, over a library of six cards
-- interned from six different printings. Every scry prompt is answered by
-- bottoming the whole look, so scry N moves exactly N cards off the top and the
-- top card names N: card 2 for scry 1, card 3 for scry 2, card 4 for scry 3, and
-- card 1 for a table that fired nothing at all. GameEvent.Scried carries a
-- PlayerId and no count, so library order is the only gameplay-level quantity
-- that can tell the striations apart.
--
-- SIX CARDS rather than four so that a table firing two striations at once is
-- distinguishable rather than clamped by a short library: dropping the 10-19
-- band's upper endpoint makes a roll of 20 scry 2 AND scry 3, which is five cards
-- bottomed and card 6 on top.
module Pawl.DiceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  rollDieSpec s registry
  resultsTableSpec s registry

treasure :: CardName.CardName
treasure = CardName.MkCardName (Text.pack "Treasure Token")

-- Pins BOTH questions this combat asks: alice attacks bob (CR 508.1), and the
-- d20 comes up `n`. The roll is answered by CONSTANT rather than by anything
-- derived from the prompt, so the engine cannot repair the answer after a
-- mutation, and never by 1, which is what Replay.defaultAnswer would supply
-- unasked.
rollAnswer :: Natural.Natural -> Prompt.Prompt r -> r
rollAnswer n p = case p of
  Prompt.RollDie _ -> n
  _ -> S.attackTo S.bob p

-- The same combat under an answerer that RECORDS what the roll prompt offered,
-- since the offer is not readable off the resulting board. S.runCombat's loop,
-- one monad up, because the trigger resolves in the combat damage step rather
-- than the step the fixture starts in.
offeredSides :: GameState.GameState -> [Natural.Natural]
offeredSides board =
  let logging :: Prompt.Prompt r -> State.State [Natural.Natural] r
      logging p = case p of
        Prompt.RollDie sides -> do
          State.modify' (sides :)
          pure (rollAnswer 13 p)
        _ -> pure (rollAnswer 13 p)
      go n gs =
        if n <= (0 :: Int) || Maybe.isJust (GameState.result gs) || not (S.inCombatPhase (GameState.phase gs))
          then pure gs
          else do
            (_, next) <- Engine.runGame logging gs Engine.runStep
            go (n - 1) next
   in reverse (State.execState (go (24 :: Int) board) [])

rollDieSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
rollDieSpec s registry = Spec.describe s "RollDie" $ do
  Spec.it s "CR 706.4 the result of the roll is the number of Treasure tokens" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
        after n = S.runCombat (rollAnswer n) board
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation: one Treasure per pip of the roll.
    Spec.assertEqWith
      s
      "CR 706.4: thirteen Treasures for a roll of thirteen"
      (S.countOnBattlefieldByName treasure S.alice (after 13))
      13
    -- The paired board, one thing different: the same combat with the die
    -- pinned to seven. Falsifies any implementation that hard-codes the pin
    -- above, or that reads the count from anywhere but the roll.
    Spec.assertEqWith
      s
      "and seven Treasures for a roll of seven"
      (S.countOnBattlefieldByName treasure S.alice (after 7))
      7
  Spec.it s "CR 706.1a a dN's outcomes are numbered from 1 to N, both ends included" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
        after n = S.runCombat (rollAnswer n) board
    -- The top face IS an outcome, so it is admitted: a range check written
    -- `< sides` instead of `<= sides` refuses it and falls back to the floor.
    Spec.assertEqWith
      s
      "CR 706.1a: 20 is an outcome of a d20, so twenty Treasures"
      (S.countOnBattlefieldByName treasure S.alice (after 20))
      20
    -- And the range is CLOSED above: an answer no d20 could show is refused and
    -- CR 706.1a's floor stands, the instruction being mandatory. An engine that
    -- trusted the answer mints 21.
    Spec.assertEqWith
      s
      "CR 706.1a: 21 is not an outcome of a d20, so the floor stands"
      (S.countOnBattlefieldByName treasure S.alice (after 21))
      1
  Spec.it s "CR 706.1 the engine offers the die and never rolls it" $ do
    dragon <- S.printingOf s registry "Ancient Copper Dragon"
    let (board, _, _) = S.combatBoardOf [dragon] []
    -- Supporting, and in its own case so it cannot stand in for the counts
    -- above: what the engine ASKED is the visible half of "offer and filter
    -- back", and it asked once, for a twenty-sided die.
    Spec.assertEqWith s "asked once, offering twenty sides" (offeredSides board) [20]

-- Six cards on alice's library from six DIFFERENT printings, top-first, with
-- Djinni Windseer entering under her and its CR 603.6a trigger pending. The
-- printings differ so that no two library positions can be confused for each
-- other; the returned ids are the library reading top-first.
--
-- addLibraryCard puts its card ON TOP, so the deck is dealt deepest-first.
tableBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m ([ObjectId.ObjectId], GameState.GameState)
tableBoard s registry = do
  djinni <- S.printingOf s registry "Djinni Windseer"
  deck <- traverse (S.printingOf s registry) ["Goblin Piker", "Bird Maiden", "Mountain", "Forest", "Island", "Plains"]
  let deal (acc, gs) printing = let (oid, gs') = S.addLibraryCard printing S.alice gs in (oid : acc, gs')
      (ids, stocked) = List.foldl' deal ([], Setup.emptyGame S.bothPlayers) (reverse deck)
      (_, entered) = S.entersWithTrigger djinni S.alice stocked
  pure (ids, entered)

-- Pins the d20 to `n` and BOTTOMS the whole look of every scry, so the number of
-- cards that leave the top is the scry's count and nothing else. Bottoming the
-- offered list is not a search for a legal answer -- it names whatever it was
-- shown -- so a mutation that changed which striation fired changes the library
-- rather than being repaired by the answerer.
tableAnswer :: Natural.Natural -> Prompt.Prompt r -> r
tableAnswer n p = case p of
  Prompt.RollDie _ -> n
  Prompt.ChooseScry _ _ looked -> (looked, [])
  _ -> S.identityAnswer p

-- The pending enters trigger, put on the stack and not yet resolved.
placeTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
placeTable n board = S.runPure (tableAnswer n) board Engine.placePendingTriggers

-- And resolved, under the same answerer, so the roll and the scry it selects are
-- the same run.
runTable :: Natural.Natural -> GameState.GameState -> GameState.GameState
runTable n board = S.runPure (tableAnswer n) (placeTable n board) Stack.resolveTop

-- Alice's library, top-first.
tableLibrary :: GameState.GameState -> [ObjectId.ObjectId]
tableLibrary = Game.zoneMembers Zone.Library S.alice

resultsTableSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
resultsTableSpec s registry = Spec.describe s "ResultsTable" $ do
  -- CR 706.3a's three striation forms on one card, read through the one thing
  -- that separates them: how many cards a scry took off the top. Card 1 on top
  -- is the vacuous board -- the table fired nothing -- and it is a DIFFERENT
  -- value from all three, so "the gate never held" cannot pass as any leg.
  Spec.it s "CR 706.3a the result selects one striation of the results table" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, fourth, _, _] -> do
        -- 10-19, the closed range in the middle: scry 2, so the third card is on
        -- top. First because the middle band is the one a half-open reading of
        -- CR 706.3a gets wrong in both directions.
        Spec.assertEqWith s "CR 706.3a: a roll of 10 is in 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 10 board))) (Just third)
        -- 1-9, the other closed range: scry 1, so the second card is on top.
        Spec.assertEqWith s "CR 706.3a: a roll of 9 is in 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 9 board))) (Just second)
        -- The single number: scry 3, so the fourth card is on top. A REGRESSION
        -- FENCE on this striation's own shape rather than a proof of it: 20 is a
        -- d20's top face, so a `20+` reading agrees with the printed `20` on
        -- every outcome, and swapping the card's Exactly for an AtLeast leaves
        -- this assertion green. It becomes discriminable only alongside a
        -- modifier that can push a result past the face count (#2083). What the
        -- assertion DOES prove is that 20 selects this striation and not the
        -- 10-19 band above it.
        Spec.assertEqWith s "CR 706.3a: a roll of 20 is the 20 striation, so scry 3" (Maybe.listToMaybe (tableLibrary (runTable 20 board))) (Just fourth)
      _ -> Spec.assertFailure s "expected six library cards"
  -- Both endpoints of a printed N1-N2 belong to it (CR 706.3a). 9 and 10 above
  -- are the adjacent pair that separates the two bands; these are the far ends,
  -- which an off-by-one at the other endpoint moves.
  Spec.it s "CR 706.3a a closed range includes both of its endpoints" $ do
    (ids, board) <- tableBoard s registry
    case ids of
      [_, second, third, _, _, _] -> do
        -- Moving this endpoint UP to 2 is visible here; deleting the test
        -- altogether is not, 1 being the die's floor. Same shape as the 20
        -- striation above, at the other end.
        Spec.assertEqWith s "CR 706.3a: 1 is the low end of 1-9, so scry 1" (Maybe.listToMaybe (tableLibrary (runTable 1 board))) (Just second)
        Spec.assertEqWith s "CR 706.3a: 19 is the high end of 10-19, so scry 2" (Maybe.listToMaybe (tableLibrary (runTable 19 board))) (Just third)
      _ -> Spec.assertFailure s "expected six library cards"
  -- The roll, the striations and the table are ONE ability, so entering puts one
  -- object on the stack and one resolution runs the whole table. Four striations
  -- transcribed as four abilities would put four there.
  Spec.it s "CR 706.3b the roll and its table are one ability" $ do
    (_, board) <- tableBoard s registry
    Spec.assertEqWith s "CR 706.3b: one ability on the stack, not one per striation" (length (GameState.stack (placeTable 9 board))) 1
    Spec.assertEqWith s "and it is gone after one resolution" (length (GameState.stack (runTable 9 board))) 0
  -- The fixture pin, in its own case so it cannot stand in for a striation
  -- above: the library really is the six cards in the order those assertions
  -- index, and CR 701.22a moves no card OUT of it, which is what makes "the top
  -- card" the whole reading.
  Spec.it s "CR 701.22a the fixture is six cards and the scry keeps them all" $ do
    (ids, board) <- tableBoard s registry
    Spec.assertEqWith s "six cards, top-first" (tableLibrary board) ids
    Spec.assertEqWith s "CR 701.22a: the library still holds all six" (length (tableLibrary (runTable 9 board))) 6
