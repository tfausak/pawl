{-# LANGUAGE GADTs #-}

-- Covers: CR 701.30 CLASH -- Effect.Clash's arm and the clash procedure in
-- Pawl.Engine.Resolve.Effect, plus Prompt.ChooseClash.
--
-- Pulling Teeth ({1}{B} Sorcery, "Clash with an opponent. If you win, target
-- player discards two cards. Otherwise, that player discards a card.") is the
-- fixture: the keyword action is its first clause, and the two discards are rule
-- 701.30d's outcome read back out of the slot.
--
-- THREE SEATS, because "an opponent" and "target player" are different players
-- here: alice casts, clashes with bob, and targets CAROL. A two-player board
-- would collapse the two and a discard would prove nothing about which one the
-- clash chose.
--
-- The harness has no vocabulary for Prompt.ChooseClash, so these cases run the
-- engine under a test-local answerer in State.State, which records who was asked
-- and what each was shown -- the APNAP order and rule 701.30c's public
-- reveal are assertions about the QUESTIONS, not about the board.
module Pawl.ClashSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Zone as Zone

-- What one ChooseClash asked: the player deciding, their own revealed card, and
-- every card the clash revealed. The last field is rule 701.30c's public reveal.
type Asked = (PlayerId.PlayerId, ObjectId.ObjectId, [(PlayerId.PlayerId, ObjectId.ObjectId)])

-- Which cards a CR 701.20a reveal has shown, off the event log, beside the
-- player it was shown by.
revealedIn :: GameState.GameState -> [(PlayerId.PlayerId, ObjectId.ObjectId)]
revealedIn gs =
  Maybe.mapMaybe
    ( \event -> case event of
        GameEvent.Revealed (Revealed.MkRevealed pid oid _ _) -> Just (pid, oid)
        _ -> Nothing
    )
    (S.eventsOf gs)

-- The top card of a player's library as a list, so a case can compare it without
-- a partial head over an empty library.
topOf :: PlayerId.PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
topOf pid gs = take 1 (Game.zoneMembers Zone.Library pid gs)

-- alice: two Swamps and Pulling Teeth in hand, and a library whose TOP is
-- `aliceTop` over one more card -- the card beneath is what makes top-and-bottom
-- two different places, so the decision is a real one. bob's library is the same
-- shape, and carol holds three cards, one more than the larger discard so that
-- "discards two" is a choice rather than her whole hand.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, GameState.GameState)
board swamp teeth aliceTop bobTop filler =
  let g0 = S.landsFor swamp S.alice 2 S.threePlayerGame
      (spell, g1) = S.addHandCard teeth S.alice g0
      (_, g2) = S.addLibraryCard filler S.alice g1
      (_, g3) = S.addLibraryCard aliceTop S.alice g2
      (_, g4) = S.addLibraryCard filler S.bob g3
      (_, g5) = S.addLibraryCard bobTop S.bob g4
      stock gs _ = snd (S.addHandCard filler S.carol gs)
   in (spell, Foldable.foldl' stock g5 [1 :: Int, 2, 3])

-- The answerer: alice bottoms the card she revealed, bob keeps his on top, and
-- every ChooseClash is recorded in the order it was asked. Pinned by seat rather
-- than searched, so a mutation cannot be repaired by an answerer that looks for
-- something legal.
clashing :: Prompt.Prompt r -> State.State [Asked] r
clashing p = case p of
  Prompt.ChooseClash _ pid _ own public -> do
    State.modify' (<> [(pid, own, NonEmpty.toList public)])
    pure (if pid == S.alice then LibraryPosition.Bottom else LibraryPosition.Top)
  Prompt.ChooseTargets _ _ _ sets -> pure (S.preferring ((==) (Just S.carol) . Recipient.playerOf) sets)
  _ -> pure (S.castAnswer p)

-- alice casts Pulling Teeth at carol and it resolves, keeping the questions the
-- clash asked beside the board it left.
resolved :: ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, [Asked])
resolved spell gs =
  let ((_, after), asked) = State.runState (Engine.runGame clashing gs (S.cast S.alice spell >> Stack.resolveTop)) []
   in (after, asked)

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Clash" $ do
  Spec.it s "CR 701.30d the controller's higher mana value wins the clash" $ do
    swamp <- S.printingOf s registry "Swamp"
    teeth <- S.printingOf s registry "Pulling Teeth"
    giant <- S.printingOf s registry "Hill Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    plains <- S.printingOf s registry "Plains"
    let (spell, before) = board swamp teeth giant bolt plains
        aliceTop = topOf S.alice before
        bobTop = topOf S.bob before
        (after, asked) = resolved spell before
    -- THE gameplay reading, and first: rule 701.30d gave alice the clash, so the
    -- "if you win" clause ran and carol discarded TWO.
    Spec.assertEqWith s "CR 701.30d the winner's clause discards two" (length (Game.zoneMembers Zone.Graveyard S.carol after)) 2
    -- CR 701.30c's reveal, which is PUBLIC and so is in the log rather than only
    -- in the prompt: both clashing players showed the card they revealed.
    Spec.assertEqWith s "CR 701.30c both clashing players revealed their top card" (fmap (fmap (: [])) (revealedIn after)) [(S.alice, aliceTop), (S.bob, bobTop)]
    -- CR 701.30c's decisions, in APNAP order and each shown BOTH revealed cards:
    -- alice is the active player, so she decides first, and neither question
    -- carries what the other player decided.
    Spec.assertEqWith
      s
      "CR 701.30c each clashing player decides, in APNAP order, seeing both revealed cards"
      (fmap (\(pid, own, public) -> (pid, [own], fmap (fmap (: [])) public)) asked)
      [ (S.alice, aliceTop, [(S.alice, aliceTop), (S.bob, bobTop)]),
        (S.bob, bobTop, [(S.alice, aliceTop), (S.bob, bobTop)])
      ]
    -- CR 701.30a's second sentence, both answers: alice's card went to the
    -- bottom of her library and bob's stayed where it was.
    Spec.assertEqWith s "CR 701.30a the card alice bottomed is under her library" (fmap (\oid -> [oid] == aliceTop) (Game.zoneMembers Zone.Library S.alice after)) [False, True]
    Spec.assertEqWith s "CR 701.30a and the card bob kept is still on top" (topOf S.bob after) bobTop
  -- The same board, differing in ONE thing: whose revealed card has the greater
  -- mana value. A board that lost for another reason would not tell the two
  -- clauses apart.
  Spec.it s "CR 701.30d a lower mana value loses it, and the other clause runs" $ do
    swamp <- S.printingOf s registry "Swamp"
    teeth <- S.printingOf s registry "Pulling Teeth"
    giant <- S.printingOf s registry "Hill Giant"
    bolt <- S.printingOf s registry "Lightning Bolt"
    plains <- S.printingOf s registry "Plains"
    let (spell, before) = board swamp teeth bolt giant plains
        (after, _) = resolved spell before
    Spec.assertEqWith s "CR 701.30d the loser's clause discards one" (length (Game.zoneMembers Zone.Graveyard S.carol after)) 1
