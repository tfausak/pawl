import Pawl ()
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ HU.testCase "unit" $ pure (),
      QC.testProperty "prop" $ \x -> x QC.=== ()
    ]
