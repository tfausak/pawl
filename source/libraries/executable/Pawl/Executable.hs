module Pawl.Executable where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Benchmark
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.Test
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | Every entry point the package has, behind one dispatch. The `pawl`
-- executable, the test suite and the benchmark are all thin `Main.hs` wrappers
-- around this module and the two it delegates to, so importing this and
-- calling 'main' is exactly the installed binary.
--
-- Both delegates parse their own arguments (tasty and tasty-bench each read
-- 'Environment.getArgs' themselves), so the subcommand is stripped with
-- 'Environment.withArgs' rather than passed along.
main :: IO ()
main = do
  arguments <- Environment.getArgs
  case arguments of
    ["schema"] -> schema
    "bench" : rest -> Environment.withArgs rest Pawl.Benchmark.main
    "test" : rest -> Environment.withArgs rest Pawl.Test.main
    _ -> do
      name <- Environment.getProgName
      IO.hPutStrLn IO.stderr $ "usage: " <> name <> " (schema | bench | test)"
      Exit.exitFailure

-- | Emits the card format's JSON Schema, which is otherwise reachable only
-- from a REPL. Card rather than Printing because Pawl.Registry.parseCard is what reads
-- a committed file and it decodes a Card; the two write the same wire.
--
-- Emitted on demand rather than committed as a file: a committed copy would
-- need a test regenerating and comparing it, which is a second mechanism to
-- keep honest, and Pawl.CardsSpec already validates the corpus against this
-- exact value.
schema :: IO ()
schema =
  Builder.hPutBuilder IO.stdout $
    Value.encode (Define.run (Codec.schema Card.codec)) <> Builder.charUtf8 '\n'
