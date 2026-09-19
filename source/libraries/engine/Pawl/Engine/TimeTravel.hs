-- | CR 701.56, time travel: choosing any number of the time-counter-bearing
-- objects a player owns or controls and moving each one's count by one, in
-- either direction, and the whole of the keyword action.
--
-- Pawl.Engine.Populate's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant
-- forbids the rules core casing on an EFFECT's identity, and nothing here does
-- -- Pawl.Engine.Resolve.Effect's Effect.TimeTravel arm calls in without saying
-- which effect it is.
--
-- Rule 701.56a has no "whenever a player time travels", so there is no GameEvent
-- here and no trigger condition to hang one on. Scryfall @o:"time travels"@,
-- 2026-09-19, returns no card; a printing worded that way is what would need
-- one.
module Pawl.Engine.TimeTravel where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.CounterCause as CounterCause
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TimeTravelChoice as TimeTravelChoice
import qualified Pawl.Types.Zone as Zone

-- Does this object carry at least one time counter right now? Rule 701.56a asks
-- it of both halves of the candidate set, so it is asked in one place.
--
-- The zero entry is filtered out rather than trusted absent: Event.removeCounters
-- is free to leave a kind mapped to zero, and a card whose last time counter has
-- gone is no longer suspended (CR 702.62b).
hasTimeCounter :: ObjectId -> GameState.GameState -> Bool
hasTimeCounter oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj -> Map.findWithDefault 0 CounterKind.Time (Object.counters obj) > 0

-- CR 701.56a's first half: "permanents you control with one or more time
-- counters on them".
--
-- CONTROL, off the projection (Projection.controls), so a permanent whose control
-- an effect has changed is a candidate for the player who controls it now and not
-- for its owner. The counters are the object's own; no rule projects those.
--
-- The permanent need not have vanishing or fading -- rule 701.56a says only "with
-- one or more time counters on them", so Rotating Fireplace, which enters with a
-- time counter and prints neither keyword, is as much a candidate as a Waning
-- Wurm.
permanentCandidates :: PlayerId -> GameState.GameState -> [ObjectId]
permanentCandidates pid gs = filter (\oid -> hasTimeCounter oid gs) (Projection.controls pid gs)

-- CR 701.56a's second half: "suspended cards you own in exile with one or more
-- time counters on them".
--
-- OWN and not control, which is rule 701.56a's own word for this half and the
-- whole difference from the first: Game.zoneMembers slices exile by owner (CR
-- 108.3), which is exactly the question here.
--
-- CR 702.62b defines suspended out of three facts -- in exile, has suspend, has a
-- time counter -- so all three are asked and none is stored; Pawl.Engine.Suspend
-- writes no stamp for that reason.
--
-- The keyword is read off Game.faceOf, the PRINTED card, which is
-- Pawl.Engine.Event.Trigger's reading for its exile scan one zone over: a card in
-- exile is no permanent, so there is no projection over it to ask instead.
suspendedCandidates :: PlayerId -> GameState.GameState -> [ObjectId]
suspendedCandidates pid gs =
  let hasSuspend oid = case Game.faceOf oid gs of
        Nothing -> False
        Just face -> Maybe.isJust (Keyword.suspend (Face.keywordSet face))
   in filter (\oid -> hasSuspend oid && hasTimeCounter oid gs) (Game.zoneMembers Zone.Exile pid gs)

-- CR 701.56a's whole candidate set, ASCENDING so a transcript and the prompt's
-- offered list are deterministic -- Pawl.Engine.Populate.candidates' posture.
candidates :: PlayerId -> GameState.GameState -> [ObjectId]
candidates pid gs = List.sort (permanentCandidates pid gs <> suspendedCandidates pid gs)

-- CR 701.56a: choose any number of those objects and, for each, put a time
-- counter on it or remove one from it.
--
-- The ObjectId is the object the prompt names -- the spell or ability resolving.
--
-- ONE PROMPT over BOTH halves at once, because rule 701.56a states one choice
-- over one set joined by "and\/or". Raised whenever the set is non-empty, which is
-- the difference from the choose-one prompts: "any number" leaves a lone
-- candidate three outcomes -- skipped, incremented, decremented -- so there is
-- still something to ask. An empty set asks nothing (CR 101.3).
--
-- CHOOSE, not target: rule 701.56a says "choose" without saying "target", so
-- nothing was declared on the stack (CR 601.2c) and there is no CR 608.2b
-- legality to re-check.
--
-- FILTERED, NOT TRUSTED, Pawl.Engine.Populate's posture: an answer naming
-- something never offered is dropped rather than repaired, since rule 701.56a
-- lets the player choose nothing at all and dropping is a choice the rule
-- already allows.
--
-- The candidates are swept BEFORE the prompt and before any counter moves (CR
-- 608.2h), so the pool cannot grow under the answer -- and each object is
-- resolved in the ascending order the sweep fixed, since rule 701.56a states no
-- order and the counters go on and come off through CR 122.6's one funnel
-- (Event.putCounters, Event.removeCounters), where CR 614's counter replacements
-- get their opportunity.
--
-- CounterCause.ByEffect, the one provenance this action has: no printing pays a
-- time travel as a cost, so every caller is a resolving effect.
timeTravel :: PlayerId -> ObjectId -> Game ()
timeTravel pid resolving = do
  gs <- State.get
  let offered = candidates pid gs
  Monad.unless (null offered) $ do
    answer <- Game.choose (Prompt.ChooseTimeTravel (Decide.deciderFor pid gs) pid resolving offered)
    Monad.forM_ offered $ \oid -> case Map.lookup oid answer of
      Nothing -> pure ()
      Just TimeTravelChoice.Add -> Monad.void (Event.putCounters (CounterCause.ByEffect pid) oid CounterKind.Time 1)
      Just TimeTravelChoice.Remove -> Event.removeCounters oid CounterKind.Time 1
