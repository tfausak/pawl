{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 705 FLIPPING A COIN -- Pawl.Types.FlipCoin, Effect.FlipCoin's arm
-- in Pawl.Engine.Resolve, and the Pawl.Types.Prompt / Pawl.Types.Response pairs
-- the call and the flip are externalised through. The transcript legs live in
-- Pawl.ReplaySpec with the other randomness prompts.
--
-- NOT the flip's GameEvent, which Pawl.EventTriggerSpec's PlayerWinsCoinFlip
-- group proves with Tavern Scoundrel: what a trigger sees is a rule 603 question
-- rather than a rule 705 one, and Winter Sky watches nothing.
--
-- Its own module rather than a group in Pawl.DiceSpec: CR 705 and CR 706 are
-- different rules sharing no type, no prompt and no effect. A coin has no size,
-- no results table and no modifier; a die has no call and no winner.
--
-- ONE FIXTURE. Winter Sky ({R} Sorcery, "Flip a coin. If you win the flip,
-- Winter Sky deals 1 damage to each creature and each player. If you lose the
-- flip, each player draws a card.") is CR 705.2's win/lose reading with both
-- branches spelled in opcodes that already existed, so the flip is the only new
-- thing the board can be reading.
--
-- FOUR LEGS, the whole truth table of (face, call): (Heads, Heads) and (Tails,
-- Tails) match and so win; (Heads, Tails) and (Tails, Heads) do not and so lose.
-- An implementation that reads only the FACE is red on (Heads, Tails); one that
-- reads only the CALL is red on (Tails, Heads); one that hard-codes a win is red
-- on both losing legs. (Heads, Tails) is the PRIMARY leg because
-- Replay.defaultAnswer answers Heads to both prompts, so a run that asked
-- neither one produces a WIN -- every S.identityAnswer descendant falls through
-- to that default silently, and the losing legs are the ones such a run cannot
-- reach.
--
-- THE ASSERTED QUANTITIES, per leg: alice's life, bob's life, alice's creature
-- count, bob's creature count, alice's hand size, bob's hand size, and the depth
-- of the stack. Each column earns its place by separating a pair of readings
-- that another column cannot:
--
--   * Life and creature counts separate a WIN from everything else.
--   * HAND SIZE is the only column that separates a LOST flip from a gate that
--     never held at all -- an unbound slot and a misspelled slot name both leave
--     life and creatures exactly where a loss leaves them.
--   * Bob's column beside alice's separates "each player" and "each creature"
--     from a sweep miswritten as the controller's own.
--   * The stack's depth keeps a Winter Sky that never resolved from passing as a
--     lost flip.
--
-- THE BOARD. Two seats: "each player" appears in both branches, so one seat
-- cannot tell it from "you". Not three -- nothing here ranges over opponents.
-- Distinct life totals (20 and 17) and distinct creature counts (one and two) so
-- no numeric coincidence can make a wrong seat read right.
--
-- Alice and bob each control a Goblin Piker (2/1), which 1 damage kills through
-- CR 704.5g, so the damage becomes a board count rather than a marker nothing
-- reads. Bob ALSO controls a Bird Maiden (1/2), which survives: that is what
-- keeps "deals 1 damage to each creature" from reading the same as a wipe.
--
-- Two cards in each library, so CR 104.3c never decks a seat and replaces a hand
-- size with a loss; one card in alice's hand and none in bob's, so the two hand
-- columns stay distinct in both legs.
module Pawl.CoinSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CoinFace as CoinFace
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ flipCoinSpec s registry

-- Set a seat's life directly, so the two seats start on different numbers and
-- neither can be read for the other.
atLife :: PlayerId.PlayerId -> Integer -> GameState.GameState -> GameState.GameState
atLife pid n gs =
  gs {GameState.players = Map.adjust (\p -> p {Player.life = n}) pid (GameState.players gs)}

-- Pins BOTH of CR 705.2's questions by CONSTANT rather than by anything derived
-- from the prompt, so the engine cannot repair the answer after a mutation, and
-- never by whatever Replay.defaultAnswer would supply unasked -- which is Heads
-- for both, and so a WIN.
flipAnswer :: CoinFace.CoinFace -> CoinFace.CoinFace -> Prompt.Prompt r -> r
flipAnswer face called p = case p of
  Prompt.FlipCoin -> face
  Prompt.CallCoin {} -> called
  _ -> S.identityAnswer p

-- Winter Sky in alice's hand with one untapped Mountain to pay for it, over the
-- board described at the top. CAST rather than planted on the stack: CR 601.2b's
-- mode selection happens as the spell is cast, and a hand-built stack object
-- carries no chosen mode and so resolves to nothing at all.
coinBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, GameState.GameState)
coinBoard s registry = do
  sky <- S.printingOf s registry "Winter Sky"
  piker <- S.printingOf s registry "Goblin Piker"
  maiden <- S.printingOf s registry "Bird Maiden"
  mountain <- S.printingOf s registry "Mountain"
  let (_, gs1) = S.addCreature piker S.alice (Setup.emptyGame S.bothPlayers)
      (_, gs2) = S.addCreature piker S.bob gs1
      (_, gs3) = S.addCreature maiden S.bob gs2
      (_, gs4) = S.addLibraryCard mountain S.alice gs3
      (_, gs5) = S.addLibraryCard mountain S.alice gs4
      (_, gs6) = S.addLibraryCard mountain S.bob gs5
      (_, gs7) = S.addLibraryCard mountain S.bob gs6
      -- handOne REPLACES alice's hand, so the spare card goes in after it.
      (gs8, skyId) = S.handOne sky (S.landsFor mountain S.alice 1 gs7)
      (_, gs9) = S.addHandCard mountain S.alice gs8
  pure (skyId, atLife S.bob 17 gs9)

-- Cast Winter Sky and resolve it under a pinned (face, call), then settle CR
-- 704.5g so the damage reads as a board count.
after :: CoinFace.CoinFace -> CoinFace.CoinFace -> (ObjectId.ObjectId, GameState.GameState) -> GameState.GameState
after face called (skyId, board) =
  S.settleSba (S.runPure (flipAnswer face called) board (S.cast S.alice skyId >> Stack.resolveTop))

-- What resolution ASKED, in order: Nothing for CR 705.1's flip, which names no
-- seat, and Just the seat for CR 705.2's call. Not readable off the resulting
-- board, so it takes a State-logging answerer.
asked :: (ObjectId.ObjectId, GameState.GameState) -> [Maybe PlayerId.PlayerId]
asked (skyId, board) =
  let logging :: Prompt.Prompt r -> State.State [Maybe PlayerId.PlayerId] r
      logging p = case p of
        Prompt.FlipCoin -> do
          State.modify' (Nothing :)
          pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
        Prompt.CallCoin _ pid -> do
          State.modify' (Just pid :)
          pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
        _ -> pure (flipAnswer CoinFace.Heads CoinFace.Tails p)
      run = Engine.runGame logging board (S.cast S.alice skyId >> Stack.resolveTop)
   in reverse (State.execState run [])

-- Every gameplay-level column of one leg, in the order the header lists them.
reading :: GameState.GameState -> (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
reading gs =
  ( S.lifeOf S.alice gs,
    S.lifeOf S.bob gs,
    S.creaturesInPlay S.alice gs,
    S.creaturesInPlay S.bob gs,
    S.handSize S.alice gs,
    S.handSize S.bob gs,
    length (GameState.stack gs)
  )

-- A won flip: 1 damage to each player and each creature, so both Pikers die and
-- bob's Bird Maiden survives, and neither player draws.
won :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
won = (Just 19, Just 16, 0, 1, 1, 0, 0)

-- A lost flip: nothing is damaged and each player draws one, which is the only
-- reading in which the hand columns move.
lost :: (Maybe Integer, Maybe Integer, Int, Int, Int, Int, Int)
lost = (Just 20, Just 17, 1, 2, 2, 1, 0)

flipCoinSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
flipCoinSpec s registry = Spec.describe s "FlipCoin" $ do
  Spec.it s "CR 705.2 a call the coin does not match loses the flip" $ do
    board <- coinBoard s registry
    -- THE GAMEPLAY ASSERTION, first so nothing ahead of it can absorb a
    -- mutation, and on the one leg Replay.defaultAnswer cannot reach: the coin
    -- came up heads, the player called tails, so the call does not match and the
    -- flip is lost.
    Spec.assertEqWith
      s
      "CR 705.2: heads flipped against a call of tails is a lost flip"
      (reading (after CoinFace.Heads CoinFace.Tails board))
      lost
    -- The mirror, one thing different: the same mismatch the other way round.
    -- Falsifies an implementation that reads only the call.
    Spec.assertEqWith
      s
      "CR 705.2: tails flipped against a call of heads is a lost flip too"
      (reading (after CoinFace.Tails CoinFace.Heads board))
      lost
  Spec.it s "CR 705.2 a call the coin matches wins the flip" $ do
    board <- coinBoard s registry
    Spec.assertEqWith
      s
      "CR 705.2: heads flipped against a call of heads is a won flip"
      (reading (after CoinFace.Heads CoinFace.Heads board))
      won
    -- The other matching pair. Falsifies an implementation that reads only the
    -- face -- "heads wins" agrees with the line above and disagrees here.
    Spec.assertEqWith
      s
      "CR 705.2: tails flipped against a call of tails is a won flip too"
      (reading (after CoinFace.Tails CoinFace.Tails board))
      won
  Spec.it s "CR 705.2 only the flipping player calls, and calls before the coin comes up" $ do
    board <- coinBoard s registry
    -- Supporting, and in its own case so it cannot stand in for the four legs
    -- above: the call is asked once and of ALICE, CR 705.2's "only the player
    -- who flips the coin ... no other players are involved", and it is asked
    -- BEFORE the face, since calling with the face already known is a different
    -- game. Bob is never asked.
    Spec.assertEqWith s "the call, of alice, then the flip" (asked board) [Just S.alice, Nothing]
