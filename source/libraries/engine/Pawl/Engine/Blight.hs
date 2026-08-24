-- | CR 701.68, "blight N": putting N -1\/-1 counters on a creature the blighting
-- player controls, and the whole of the keyword action.
--
-- Pawl.Engine.Amass's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant forbids
-- the rules core casing on an EFFECT's identity, and nothing here does --
-- Pawl.Engine.Resolve's Effect.Blight arm and Pawl.Engine.Cost's
-- CostComponent.Blight arm both call in without saying which they are.
--
-- ONE module and not two procedures, because rule 701.68a is one rule however the
-- card demands it: CR 601.2f\/602.1b make it a cost, CR 118.12 makes it a cost
-- paid on resolution, and an effect asks for it outright. All three put the same
-- counters on a creature chosen the same way, so all three come here.
module Pawl.Engine.Blight where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt

-- | CR 701.68a's candidate set: every creature this player controls, UNNARROWED
-- -- the rule qualifies by control and by nothing else, which is the whole
-- difference from bolster's least-toughness pool. Read off the PROJECTION, so a
-- permanent that is a creature only by a continuous effect is a candidate and one
-- that has stopped being one is not.
--
-- ASCENDING, so both `blight`'s single-candidate shortcut and a transcript are
-- deterministic -- Pawl.Engine.Amass's posture, and Pawl.Engine.Ring.tempt's
-- before it.
candidates :: PlayerId -> GameState.GameState -> [ObjectId]
candidates pid gs = List.sort (filter (\oid -> Projection.isCreatureOf oid gs) (Projection.controls pid gs))

-- | CR 701.68b: can this player blight at all? "If a player is given the choice
-- to blight but is unable to put N -1\/-1 counters on a creature they control
-- (usually because they control no creatures), they can't choose to blight."
--
-- The CLASSIFICATION Pawl.Engine.Cost.canPayComponent reads, which is how a blight
-- COST is unpayable rather than a no-op: an activated ability whose cost includes
-- one is never offered (CR 601.2h's "unpayable costs can't be paid", reaching an
-- activation through CR 602.2b), and CR 118.12's resolution offer is never raised
-- (CR 118.3, and rule 118.12's own "does, doesn't, or can't").
--
-- Control and not existence -- an opponent's creatures are not candidates -- and
-- nothing here reads N. Rule 701.68b's "unable to put N counters" has only the one
-- cause the rule itself names: CR 122.6 puts any number of counters on any
-- creature, so a 1\/1 takes blight 5 as readily as blight 1.
canBlight :: PlayerId -> GameState.GameState -> Bool
canBlight pid gs = not (null (candidates pid gs))

-- | CR 701.68a: put N -1\/-1 counters on a creature this player controls.
-- Answers whether a creature was found -- False is rule 701.68b's board, which
-- CR 101.3 makes a no-op for a mandatory effect and Pawl.Engine.Cost turns into
-- an unpaid cost.
--
-- The ObjectId is the object the prompt names -- the spell or ability resolving,
-- or the one whose cost is being paid. The CounterCause is the caller's, and the
-- blighting player is read off it (CounterCause.putter): rule 701.68a's "you" is
-- the player putting the counters, so one argument cannot disagree with itself.
-- CR 601.2h's cost is CounterCause.ByPayment, CR 118.12's and an effect's
-- alike CounterCause.ByEffect -- one procedure, three provenances; see #1647.
--
-- CHOOSE, not target: rule 701.68a says "a creature you control" without saying
-- "target", so nothing was declared on the stack (CR 601.2c) and there is no CR
-- 608.2b legality to re-check.
--
-- The prompt is raised only for TWO OR MORE candidates. One creature is the whole
-- of the rule's candidate set, so performing the action decides nothing -- where
-- the rules leave nothing to ask, don't prompt.
--
-- FILTERED, NOT TRUSTED, the ChooseBolster posture: an answer naming something
-- never offered falls back to the first candidate. That holds for the cost callers
-- too, where the alternative would be Pawl.Engine.Cost's reject-not-repair --
-- rule 701.68a states no way to fail once a creature exists, so a payment lost to
-- a bad answer would be a refusal the rules do not offer.
--
-- N of zero still chooses, and CR 122.6 is why the choice is made anyway: rule
-- 701.68a's process is "put N -1\/-1 counters on a creature you control", so the
-- creature is chosen whatever N is, and CR 701.68c's "blighted creature" is that
-- creature. Nothing records it yet (gap #1492).
blight :: CounterCause.CounterCause -> ObjectId -> Natural -> Game Bool
blight cause resolving n = do
  let pid = CounterCause.putter cause
  gs <- State.get
  case candidates pid gs of
    -- CR 701.68b for a cost, CR 101.3 for an effect: a player controlling no
    -- creature blights nothing.
    [] -> pure False
    first : rest -> do
      blighted <- case rest of
        [] -> pure first
        second : more -> do
          let offered = first NonEmpty.:| (second : more)
          answer <- Game.choose (Prompt.ChooseBlight (Decide.deciderFor pid gs) pid resolving offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      -- CR 122.6: through the single funnel, so CR 614.1's counter replacements
      -- (Vorinclex, Monstrous Raider) get their opportunity -- and, where the
      -- cause is an effect, CR 614.16's (Doubling Season).
      Monad.when (n > 0) . Monad.void $
        Event.putCounters cause blighted CounterKind.MinusOneMinusOne n
      pure True
