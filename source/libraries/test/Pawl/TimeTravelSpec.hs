{-# LANGUAGE GADTs #-}

-- Covers: CR 701.56 TIME TRAVEL -- Pawl.Engine.TimeTravel and Effect.TimeTravel's
-- arm in Pawl.Engine.Resolve.Effect.
--
-- Wibbly-wobbly, Timey-wimey ({1}{U} Sorcery, "Time travel. Draw a card.") is the
-- fixture: the keyword action is the whole of its first sentence, so every
-- assertion below is about rule 701.56a.
--
-- THE BOARD SHAPE that makes the cases discriminating. Rule 701.56a's candidate
-- is either a permanent YOU CONTROL with a time counter, or a SUSPENDED card YOU
-- OWN in exile with one -- and the board carries a counterexample to each half of
-- each: alice controls a Hill Giant with time counters (a candidate) and an
-- Ornithopter with none, bob controls a Goblin Piker with time counters, alice
-- owns a suspended Durkwood Baloth in exile (a candidate) and an Island in exile
-- with time counters but no suspend (CR 702.62b), and bob owns a suspended
-- Chronomantic Escape. Every count is distinct, so a reading that moved the wrong
-- object is named by the number it left behind.
module Pawl.TimeTravelSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TimeTravelChoice as TimeTravelChoice

-- The six objects rule 701.56a has to sort into two candidates and four
-- non-candidates, plus the spell that asks it to.
data Setup = MkSetup
  { spell :: ObjectId.ObjectId,
    -- | alice's, on the battlefield, with three time counters: a candidate.
    giant :: ObjectId.ObjectId,
    -- | alice's, on the battlefield, with none.
    thopter :: ObjectId.ObjectId,
    -- | bob's, on the battlefield, with five time counters.
    piker :: ObjectId.ObjectId,
    -- | alice's, suspended in exile, with seven time counters: a candidate.
    baloth :: ObjectId.ObjectId,
    -- | alice's, in exile with nine time counters and no suspend (an Island).
    landInExile :: ObjectId.ObjectId,
    -- | bob's, suspended in exile, with eleven time counters.
    escape :: ObjectId.ObjectId,
    state :: GameState.GameState
  }

board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> Setup
board island wibbly giantP thopterP pikerP balothP escapeP =
  let g0 = S.landsInPlay island 2
      -- Stocked, so the card's second sentence has something to draw and CR
      -- 104.3c does not decide the game before the assertions run.
      (_, g1) = S.addLibraryCard island S.alice g0
      (giantId, g2) = S.addPermanent giantP S.alice g1
      g3 = S.addCounter CounterKind.Time 3 giantId g2
      (thopterId, g4) = S.addPermanent thopterP S.alice g3
      (pikerId, g5) = S.addPermanent pikerP S.bob g4
      g6 = S.addCounter CounterKind.Time 5 pikerId g5
      (balothId, g7) = S.addExiledCard balothP S.alice g6
      g8 = S.addCounter CounterKind.Time 7 balothId g7
      (landId, g9) = S.addExiledCard island S.alice g8
      g10 = S.addCounter CounterKind.Time 9 landId g9
      (escapeId, g11) = S.addExiledCard escapeP S.bob g10
      g12 = S.addCounter CounterKind.Time 11 escapeId g11
      (spellId, g13) = S.addHandCard wibbly S.alice g12
   in MkSetup
        { spell = spellId,
          giant = giantId,
          thopter = thopterId,
          piker = pikerId,
          baloth = balothId,
          landInExile = landId,
          escape = escapeId,
          state = g13
        }

-- alice casts Wibbly-wobbly, Timey-wimey and it resolves, answering the time
-- travel by ObjectId: a counter onto the Hill Giant, one off the suspended
-- Durkwood Baloth, and nothing else chosen.
--
-- A RECORDING answerer, threaded through State, because the candidate POOL is
-- half of what rule 701.56a states: an answerer that merely picked its two
-- objects out of whatever it was offered would pass against an engine that
-- offered a narrower set. It answers by id rather than by searching the offered
-- list, so a mutation cannot be repaired into the right answer.
travelled :: Setup -> ([[ObjectId.ObjectId]], GameState.GameState)
travelled setup =
  let picks =
        Map.fromList
          [ (giant setup, TimeTravelChoice.Add),
            (baloth setup, TimeTravelChoice.Remove)
          ]
      answer :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
      answer p = case p of
        Prompt.ChooseTimeTravel _ _ _ offered -> do
          State.modify' (<> [offered])
          pure picks
        _ -> pure (S.castAnswer p)
      game = S.cast S.alice (spell setup) >> Stack.resolveTop
      ((_, after), offers) = State.runState (Engine.runGame answer (state setup) game) []
   in (offers, after)

-- How many time counters an object carries, the one question every assertion
-- below asks.
timeOn :: ObjectId.ObjectId -> GameState.GameState -> Natural
timeOn = S.counterOf CounterKind.Time

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "TimeTravel" $ do
  Spec.it s "CR 701.56a a counter goes onto the chosen permanent and comes off the chosen suspended card" $ do
    island <- S.printingOf s registry "Island"
    wibbly <- S.printingOf s registry "Wibbly-wobbly, Timey-wimey"
    giantP <- S.printingOf s registry "Hill Giant"
    thopterP <- S.printingOf s registry "Ornithopter"
    pikerP <- S.printingOf s registry "Goblin Piker"
    balothP <- S.printingOf s registry "Durkwood Baloth"
    escapeP <- S.printingOf s registry "Chronomantic Escape"
    let setup = board island wibbly giantP thopterP pikerP balothP escapeP
        (_, after) = travelled setup
    -- THE gameplay reading, and first: rule 701.56a's two directions, one of them
    -- on a permanent and the other on a suspended card in exile.
    Spec.assertEqWith s "CR 701.56a the chosen permanent went from three time counters to four" (timeOn (giant setup) after) 4
    Spec.assertEqWith s "CR 701.56a the chosen suspended card went from seven to six" (timeOn (baloth setup) after) 6
    -- The four non-candidates, on the SAME board: each is one clause of rule
    -- 701.56a's candidate set failing.
    Spec.assertEqWith s "the permanent with no time counter gained none" (timeOn (thopter setup) after) 0
    Spec.assertEqWith s "CR 701.56a's \"you control\": the opponent's permanent kept its five" (timeOn (piker setup) after) 5
    Spec.assertEqWith s "CR 702.62b an exiled card without suspend is not suspended, and kept its nine" (timeOn (landInExile setup) after) 9
    Spec.assertEqWith s "CR 701.56a's \"you own\": the opponent's suspended card kept its eleven" (timeOn (escape setup) after) 11
  Spec.it s "CR 701.56a the choice is offered over the whole candidate set, and over nothing else" $ do
    island <- S.printingOf s registry "Island"
    wibbly <- S.printingOf s registry "Wibbly-wobbly, Timey-wimey"
    giantP <- S.printingOf s registry "Hill Giant"
    thopterP <- S.printingOf s registry "Ornithopter"
    pikerP <- S.printingOf s registry "Goblin Piker"
    balothP <- S.printingOf s registry "Durkwood Baloth"
    escapeP <- S.printingOf s registry "Chronomantic Escape"
    let setup = board island wibbly giantP thopterP pikerP balothP escapeP
        (offers, _) = travelled setup
        expected = List.sort [giant setup, baloth setup]
    -- The POOL, not the picks: an engine that offered only what the answerer
    -- happened to name would satisfy the case above and fail this one.
    Spec.assertEqWith s "CR 701.56a one prompt, over both halves of the candidate set at once" offers [expected]
