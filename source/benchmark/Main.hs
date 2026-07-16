{-# LANGUAGE GADTs #-}

import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import qualified Pawl.Engine as Engine
import qualified Pawl.Setup as Setup
import qualified Pawl.Type.Action as Action
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Prompt as Prompt
import Pawl.Type.Result (Result)
import qualified Test.Tasty.Bench as Bench

alwaysPass :: Prompt.Prompt r -> r
alwaysPass p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> Action.Pass

-- Takes the first player's id so the whole game genuinely depends on the
-- benchmark's argument; otherwise GHC floats the result out and times a cached
-- value rather than the game. NOINLINE keeps it from being folded back in.
goldfish :: Natural -> Result
goldfish n =
  let players = PlayerId.MkPlayerId n NonEmpty.:| [PlayerId.MkPlayerId (n + 1)]
   in fst (Engine.runGamePure alwaysPass (Setup.emptyGame players) (Engine.playFrom players))
{-# NOINLINE goldfish #-}

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bench "goldfish 2p" $ Bench.whnf goldfish 0
    ]
