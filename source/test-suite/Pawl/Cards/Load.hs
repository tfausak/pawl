-- The temporary M3.5 TH shim: splice a card's compiled value from its committed
-- JSON at build time. It reads the file, registers it as a dependency (so an
-- edit forces a rebuild), decodes it, fails the build loudly on a Left (never a
-- partial value), and lifts the resulting Printing into the splice. EXPIRES when
-- the test suite converts to IO loading like the benchmark and this module and
-- its DeriveLift/TemplateHaskell extensions are deleted.
module Pawl.Cards.Load where

import qualified Data.Text.IO as TextIO
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import qualified Pawl.Codec as Codec
import qualified Pawl.Json as Json

loadPrinting :: String -> Q Exp
loadPrinting slug = do
  let path = "data/cards/" <> slug <> ".json"
  addDependentFile path
  contents <- runIO (TextIO.readFile path)
  case Json.parse contents >>= Codec.jsonToPrinting of
    Left err -> fail ("card " <> slug <> ": " <> show err)
    Right p -> lift p
