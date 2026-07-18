{-# LANGUAGE GADTs #-}

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Numeric.Natural (Natural)
import qualified Pawl.Engine as Engine
import qualified Pawl.Setup as Setup
import qualified Pawl.Type.Action as Action
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.Result (Result)
import qualified Test.Tasty.Bench as Bench

isCreatureRecipient :: Recipient.Recipient -> Bool
isCreatureRecipient r = case r of
  Recipient.ToCreature _ -> True
  Recipient.ToDefender _ -> False

alwaysPass :: Prompt.Prompt r -> r
alwaysPass p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> Action.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty

-- Casts when legal, otherwise passes: the benchmark that actually exercises the
-- stack, mana payment, and resolution.
castAnswer :: Prompt.Prompt r -> r
castAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass

-- Casts, attacks, and blocks: the benchmark that exercises combat.
fightAnswer :: Prompt.Prompt r -> r
fightAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers _ _ ids -> ids
  Prompt.DeclareBlockers _ _ mine attackers -> case attackers of
    [] -> Map.empty
    a : _ -> Map.fromList (map (\b -> (b, a)) mine)
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseAction _ _ actions ->
    let isCast a = case a of
          Action.Cast _ -> True
          _ -> False
     in case filter isCast actions of
          h : _ -> h
          [] -> Action.Pass

-- Two players, seeded from the benchmark's argument.
playersFrom :: Natural -> NonEmpty.NonEmpty PlayerId
playersFrom n = PlayerId.MkPlayerId n NonEmpty.:| [PlayerId.MkPlayerId (n + 1)]

-- Takes the first player's id so the whole game genuinely depends on the
-- benchmark's argument; otherwise GHC floats the result out and times a cached
-- value rather than the game. NOINLINE keeps it from being folded back in.
goldfish :: Natural -> Result
goldfish n =
  let players = playersFrom n
   in fst (Engine.runMatchPure alwaysPass (Setup.mirror Setup.redDeck players))
{-# NOINLINE goldfish #-}

-- Parameterized for the same reason as 'goldfish'.
casting :: Natural -> Result
casting n =
  let players = playersFrom n
   in fst (Engine.runMatchPure castAnswer (Setup.mirror Setup.redDeck players))
{-# NOINLINE casting #-}

fighting :: Natural -> Result
fighting n =
  let players = playersFrom n
   in fst (Engine.runMatchPure fightAnswer (Setup.mirror Setup.redDeck players))
{-# NOINLINE fighting #-}

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bench "goldfish 2p" $ Bench.whnf goldfish 0,
      Bench.bench "casting 2p" $ Bench.whnf casting 0,
      Bench.bench "fighting 2p" $ Bench.whnf fighting 0
    ]
