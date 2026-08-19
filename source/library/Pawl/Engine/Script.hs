{-# LANGUAGE GADTs #-}

module Pawl.Engine.Script where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Replay as Replay
import qualified Pawl.Types.Action as Action
import Pawl.Types.Prompt (Prompt)
import qualified Pawl.Types.Prompt as Prompt

-- Deterministic scripted answerers: whole games played without a person, for
-- callers that want a game to HAPPEN rather than a particular decision made.
-- The benchmark drives all three; the test suite answers its own prompts,
-- because a fixture's answer is what makes its assertion discriminating.
--
-- Each is a handful of arms over 'Replay.defaultAnswer', which is already the
-- total least-eventful answer to every prompt. Delegating rather than restating
-- it is what keeps a new 'Prompt' constructor from breaking these callers: the
-- library grows one arm and every script inherits it. Restating it is how
-- 'source/benchmark/Main.hs' came to carry three 48-arm copies, two of which
-- shipped a red CI when crew added a prompt (#916).

-- CR 514.1 cleanup discard, answered from the END of the hand.
--
-- Load-bearing, and not interchangeable with taking the front (#66). The hand is
-- oldest-first, and a player may play only one land per turn (CR 305.2), so
-- surplus lands pile up as the oldest cards held. Discarding from the front
-- therefore pitches precisely the lands a script needs, and the board never
-- develops even with 'casting' driving it: measured over one match,
-- front-discard yields 4 land plays and no combat, back-discard yields 72 land
-- plays and 66 declare-attackers prompts.
discardNewest :: [a] -> Natural -> [a]
discardNewest ids n = List.genericTake n (reverse ids)

-- 'Replay.defaultAnswer', but sacrificing nothing.
--
-- The base every scripted answerer wants, including the test suite's fixtures:
-- the minimal subset, so a default answer never throws a permanent away.
-- 'Replay.defaultAnswer' takes the maximal one, which is right for a replay
-- degrading gracefully and wrong for anything meant to develop a board.
declining :: Prompt r -> r
declining p = case p of
  Prompt.ChooseAnyNumberToSacrifice {} -> Set.empty
  _ -> Replay.defaultAnswer p

-- What the whole-game scripts below share: 'declining', plus the discard a long
-- unattended game needs to keep developing.
scripted :: Prompt r -> r
scripted p = case p of
  Prompt.ChooseDiscard _ _ ids n -> discardNewest ids n
  _ -> declining p

-- Cast if anything is castable, else play a land, else pass. Shared with the
-- test suite's Pawl.Support.castAnswer, whose base differs (it keeps
-- 'Replay.defaultAnswer''s front-of-hand discard).
castElsePlay :: [Action.Action] -> Action.Action
castElsePlay actions =
  let isCast a = case a of
        Action.Cast {} -> True
        _ -> False
      isPlay a = case a of
        Action.Play {} -> True
        _ -> False
   in case filter isCast actions of
        h : _ -> h
        [] -> case filter isPlay actions of
          h : _ -> h
          [] -> Action.Pass

-- Never takes an action: the goldfish game, where nothing is ever cast and the
-- turn structure alone is exercised. Overrides 'Replay.defaultAnswer', which
-- takes the FIRST legal action rather than passing.
passing :: Prompt r -> r
passing p = case p of
  Prompt.ChooseAction {} -> Action.Pass
  _ -> scripted p

-- Casts if anything is castable, else plays a land, else passes.
--
-- The land fallback is load-bearing: without it no land ever reaches the
-- battlefield, so no mana ever exists, so 'Action.legalActions' never offers a
-- Cast and the filter is always empty -- which is how three benchmarks came to
-- execute the same goldfish game (#66).
casting :: Prompt r -> r
casting p = case p of
  Prompt.ChooseAction _ _ actions -> castElsePlay actions
  _ -> scripted p

-- 'casting', plus attacking with everything and blocking everything onto the
-- first attacker: the script that exercises combat.
fighting :: Prompt r -> r
fighting p = case p of
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (fmap (\b -> (b, Set.singleton a)) mine)
  _ -> casting p
