{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 706 ROLLING A DIE -- Pawl.Types.RollDie, Effect.RollDie's arm in
-- Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pair the
-- roll is externalised through. The transcript legs live in Pawl.ReplaySpec
-- with the other randomness prompts.
--
-- Ancient Copper Dragon ("Flying / Whenever this creature deals combat damage to
-- a player, roll a d20. You create a number of Treasure tokens equal to the
-- result") is the fixture, and the whole of CR 706 this file can reach: one die,
-- no results table (#2082), no modifier and no reroll (#2083), and the result
-- read straight into a count (CR 706.4).
--
-- THE ASSERTED QUANTITY is how many Treasure tokens alice controls once combat
-- damage has been dealt. It is the roll's result made visible: nothing else on
-- this board mints a token, bob controls nothing, and the Treasure's own mana
-- ability is never activated. Tokens enter under their creator's control (CR
-- 111.2) and alice is the only creator, so there is no second trace to the same
-- number.
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
module Pawl.DiceSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Prompt as Prompt

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ rollDieSpec s registry

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
