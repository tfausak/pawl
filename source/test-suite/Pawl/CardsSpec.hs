-- Covers data/cards/*.json and Pawl.Codec.slugify.
module Pawl.CardsSpec where

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Pawl.Cards as Cards
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json
import qualified Pawl.Type.Card as CardT
import qualified Pawl.Type.Printing as Printing
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

slugOf :: Printing.Printing -> Text.Text
slugOf = Codec.slugify . CardT.name . Printing.card

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.CardsSpec"
    [ HU.testCase "slugs are unique" $
        let slugs = map slugOf Cards.allPrintings
         in HU.assertEqual "unique" (List.sort slugs) (List.sort (List.nub slugs)),
      HU.testCase "each committed file re-parses to its compiled card (P3)" $
        mapM_ checkFile Cards.allPrintings
    ]

checkFile :: Printing.Printing -> HU.Assertion
checkFile p = do
  let path = "data/cards/" <> Text.unpack (slugOf p) <> ".json"
  contents <- TextIO.readFile path
  HU.assertEqual path (Right p) (Json.parse contents >>= Codec.jsonToPrinting)
  -- Byte-stability: the committed file is exactly the render plus the trailing
  -- newline the repo's file-hygiene rule mandates, so a regenerate is a no-op.
  HU.assertEqual (path <> " bytes") (Json.render (Codec.printingToJson p) <> Text.pack "\n") contents
