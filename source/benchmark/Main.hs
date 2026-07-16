import Pawl ()
import qualified Test.Tasty.Bench as Bench

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bench "id" $ Bench.whnf id ()
    ]
