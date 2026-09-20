module Pawl.Executable where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified Pawl.Benchmark
import qualified Pawl.Codec.Card as Card
import qualified Pawl.DeckList as DeckList
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.Registry as Registry
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
    ["deck", path] -> deck path
    "bench" : rest -> Environment.withArgs rest Pawl.Benchmark.main
    "test" : rest -> Environment.withArgs rest Pawl.Test.main
    _ -> do
      name <- Environment.getProgName
      IO.hPutStrLn IO.stderr $ "usage: " <> name <> " (schema | deck FILE | bench | test)"
      Exit.exitFailure

-- | Reads a deck list and writes back the deck it means, which is what makes a
-- deck expressible as the text a player types rather than only as Haskell.
-- Round-tripping it is the point: the output is itself a deck list, so a name
-- that resolved is echoed in the spelling this pool files it under.
--
-- Decoded as UTF-8 explicitly rather than through TextIO.readFile, for
-- Pawl.Registry.parseCard's reason: that one decodes using the locale encoding,
-- so a card with a non-ASCII character in its name would fail under LC_ALL=C.
deck :: FilePath -> IO ()
deck path = do
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> do
      IO.hPutStrLn IO.stderr $ path <> ": not valid UTF-8: " <> show err
      Exit.exitFailure
    Right contents -> do
      root <- Registry.defaultRoot
      registry <- Registry.fileRegistry root
      result <- DeckList.parse registry contents
      case result of
        Left problems -> do
          mapM_ (TextIO.hPutStrLn IO.stderr . (\p -> Text.pack (path <> ": ") <> DeckList.explain p)) problems
          Exit.exitFailure
        Right parsed -> TextIO.putStr (DeckList.render parsed)

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
