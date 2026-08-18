import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonSchema.Define as Define
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

-- | Emits the card format's JSON Schema, which is otherwise reachable only
-- from a REPL. Card rather than Printing because Pawl.Registry.parseCard is what reads
-- a committed file and it decodes a Card; the two write the same wire.
--
-- Emitted on demand rather than committed as a file: a committed copy would
-- need a test regenerating and comparing it, which is a second mechanism to
-- keep honest, and Pawl.CardsSpec already validates the corpus against this
-- exact value.
main :: IO ()
main = do
  arguments <- Environment.getArgs
  case arguments of
    ["schema"] ->
      Builder.hPutBuilder IO.stdout $
        Value.encode (Define.run (Codec.schema Card.codec)) <> Builder.charUtf8 '\n'
    _ -> do
      name <- Environment.getProgName
      IO.hPutStrLn IO.stderr $ "usage: " <> name <> " schema"
      Exit.exitFailure
