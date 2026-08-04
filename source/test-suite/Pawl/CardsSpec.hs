-- Covers data/cards/*.json and Pawl.Slug.slugify.
module Pawl.CardsSpec where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Printing as Printing
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Printing as Printing

-- Where a card's file lives, by the FILING convention Pawl.Registry.parseCard
-- enforces: every face name joined, slugified.
--
-- Not Card.combined's name, which is a different question and only coincidentally
-- the same string. CR 709.4a's joined name IS what the combined view of a split
-- card shows, but CR 715.4 gives an adventurer card its normal face's name alone
-- -- so reading the file's location off the combined view would look for
-- embereth-shieldbreaker.json while the registry files that card under
-- embereth-shieldbreaker-battle-display.json. The joined string is a filing
-- convention rather than a name the card has (#649), and this is the side of
-- that distinction a PATH is on; a lookup by either face's own name still lands
-- through Registry.byFaceName.
slugOf :: Printing.Printing -> Slug.Slug
slugOf =
  Slug.fromText
    . CardName.unwrap
    . CardName.join
    . fmap Face.name
    . Card.Type.faces
    . Printing.card

spec :: Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.Cards" $ do
  Spec.it s "each committed file re-parses to its compiled card (P3)" $ do
    root <- Registry.defaultRoot
    ps <- S.allPrintings s
    mapM_ (checkFile s root) ps

checkFile :: Spec.Spec IO n -> FilePath -> Printing.Printing -> IO ()
checkFile s root p = do
  let slug = slugOf p
  let path = Registry.cardPath root slug
  -- Read as bytes and decoded as UTF-8 explicitly, for the reason
  -- Pawl.Registry.parseCard is: Data.Text.IO.readFile decodes using the locale
  -- encoding, which is ASCII under LC_ALL=C, so this would otherwise die on
  -- khabal-ghoul.json's "á". Not parseCard itself -- this case wants the decoded
  -- JSON value to compare against, not a Card.
  bytes <- ByteString.readFile path
  case Encoding.decodeUtf8' bytes of
    Left err -> Spec.assertFailure s (path <> ": not valid UTF-8: " <> show err)
    Right contents ->
      case Common.parse contents of
        -- Unreachable: S.allPrintings would have failed in IO first.
        Left err -> Spec.assertFailure s (path <> ": " <> Text.unpack err)
        Right value ->
          -- The loader reads everything the file says and invents nothing:
          -- re-encoding the loaded printing reproduces the file's meaning. Compared
          -- up to key order and whitespace, because JSON objects are unordered and
          -- formatting is not part of the contract. The corpus is committed
          -- pretty-printed (`jq -S .`) while Common.render emits compact output, so
          -- this can never quietly regress into a byte comparison: every file would
          -- fail at once.
          Spec.assertEqWith s path (Common.sortKeys (Printing.toJson p)) (Common.sortKeys value)
