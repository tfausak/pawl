module Pawl.Executable where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.IO as TextIO
import qualified Pawl.Benchmark
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Scenario as Codec.Scenario
import qualified Pawl.DeckList as DeckList
import qualified Pawl.Ingest as Ingest
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.Registry as Registry
import qualified Pawl.Scenario as Scenario
import qualified Pawl.Scenario.Load as Load
import qualified Pawl.Test
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified System.Directory as Directory
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
    ["schema", "scenario"] -> scenarioSchema
    ["deck", path] -> deck path
    ["ingest", path] -> ingest path
    "scenario" : paths@(_ : _) -> scenario paths
    "bench" : rest -> Environment.withArgs rest Pawl.Benchmark.main
    "test" : rest -> Environment.withArgs rest Pawl.Test.main
    _ -> do
      name <- Environment.getProgName
      IO.hPutStrLn IO.stderr $ "usage: " <> name <> " (schema [scenario] | deck FILE | ingest FILE | scenario FILE... | bench | test)"
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

-- | Reads script/ingest/candidates.jq's reduction of MTGJSON's AtomicCards and
-- writes a card file for every keyword-only card it holds, Oracle text included
-- (#9). A card already in the pool is never overwritten -- the hand-written
-- file wins -- and is named on stderr if it says something other than MTGJSON
-- does. Then every pool card gains its faces' Oracle text, the one field this
-- writes into a hand-written file. Files are written compact;
-- `script/format-json.sh fix` puts them in canonical form.
--
-- Prints what it did, and why the cards it did not write were left out, by the
-- kind of word that disqualified them.
ingest :: FilePath -> IO ()
ingest path = do
  bytes <- ByteString.readFile path
  let parsed = do
        contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
        fields <- Common.parse contents >>= Common.asObject
        records <- Common.field "cards" fields >>= Common.asArray
        known <- Common.field "texts" fields >>= Ingest.texts
        pure (records, known)
  case parsed of
    Left problem -> do
      TextIO.hPutStrLn IO.stderr (Text.pack (path <> ": ") <> problem)
      Exit.exitFailure
    Right (records, known) -> do
      root <- Registry.defaultRoot
      outcomes <- mapM (ingestOne root) records
      loaded <- Registry.loadRoot root
      stamped <- fmap length . Monad.filterM (stampOne known) $ Maybe.mapMaybe (\entry@(file, _) -> fmap ((,) file) (Registry.referenceCard entry)) loaded
      let reasons = Map.fromListWith (+) [(Text.takeWhile (/= ':') reason, 1 :: Int) | Left reason <- outcomes]
      putStrLn $ "written: " <> show (length (filter (== Right True) outcomes))
      putStrLn $ "already in the pool: " <> show (length (filter (== Right False) outcomes))
      putStrLn $ "pool files given Oracle text: " <> show stamped
      putStrLn $ "left out: " <> show (sum reasons)
      mapM_ (\(kind, n) -> TextIO.putStrLn (Text.pack "  " <> kind <> Text.pack (": " <> show n))) (Map.toDescList reasons)

-- Right True for a card written, Right False for one the pool already holds.
ingestOne :: FilePath -> Value.Value -> IO (Either Text.Text Bool)
ingestOne root record = case Ingest.candidate record of
  Left reason -> pure (Left reason)
  Right card -> do
    let file = Registry.cardPath root (Registry.filedAs card)
    exists <- Directory.doesFileExist file
    if exists
      then do
        existing <- fmap Registry.parseCard (ByteString.readFile file)
        Monad.unless (fmap (Ingest.stamp (texts card)) existing == Right card) $
          IO.hPutStrLn IO.stderr (file <> ": disagrees with MTGJSON; the pool's file is kept")
      else writeCard file card
    pure (Right (not exists))
  where
    texts card = Map.fromList [(CardName.unwrap (Face.name face), text) | face <- NonEmpty.toList (Card.Type.faces card), Just text <- [Face.oracleText face]]

-- True when the card's file changed.
stampOne :: Map.Map Text.Text Text.Text -> (FilePath, Card.Type.Card) -> IO Bool
stampOne known (file, card) =
  let stamped = Ingest.stamp known card
   in if stamped == card then pure False else True <$ writeCard file stamped

writeCard :: FilePath -> Card.Type.Card -> IO ()
writeCard file = ByteString.writeFile file . Encoding.encodeUtf8 . Common.render . Codec.encode Card.codec

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

-- | The scenario format's JSON Schema, for an editor or a contributor writing
-- one by hand. Emitted on demand for 'schema''s reason.
scenarioSchema :: IO ()
scenarioSchema =
  Builder.hPutBuilder IO.stdout $
    Value.encode (Define.run (Codec.schema Codec.Scenario.codec)) <> Builder.charUtf8 '\n'

-- | Runs each scenario file against the bundled cards and says how it went,
-- one line each: the first way to drive the engine that is not the test suite.
-- Exits 1 if any did not decode or did not run clean.
scenario :: [FilePath] -> IO ()
scenario paths = do
  root <- Registry.defaultRoot
  registry <- Registry.fileRegistry root
  outcomes <- mapM (runOne registry) paths
  Monad.unless (and outcomes) Exit.exitFailure

runOne :: Registry.Registry IO -> FilePath -> IO Bool
runOne registry path = do
  let report message ok = do
        TextIO.hPutStrLn (if ok then IO.stdout else IO.stderr) (Text.pack (path <> ": ") <> message)
        pure ok
  (_, decoded) <- Load.loadFile path
  case decoded of
    Left problem -> report (Text.pack "does not decode: " <> problem) False
    Right parsed -> do
      result <- Scenario.run registry parsed
      case result of
        Left failure -> report (Scenario.render failure) False
        Right _ -> report (Text.pack "ok") True
