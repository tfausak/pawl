-- The bundled card corpus, read as data rather than through a registry.
--
-- Every consumer of this module is a lint over data/cards: that each committed
-- file re-parses, that each file's name agrees with the card's own name, that
-- every mode declares the slots it reads. Those are claims about what pawl
-- SHIPS, not questions anyone asks a registry -- and treating them as registry
-- operations is what forced Pawl.Registry to enumerate directories at all.
--
-- So this lives in the test suite. Nothing outside it lints the corpus, and a
-- library module no consumer calls is dead weight.
--
-- Enumeration is what this module ADDS. Where a card file lives and what its
-- bytes mean are the same facts a lookup needs, so they come from Pawl.Registry
-- (cardPath, parseCard) rather than being restated here. Importing a registry
-- module is not the same as going through a registry -- no Registry value is
-- constructed below, and none should be.
module Pawl.Corpus where

import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified System.Directory as Directory

-- Every card file in `root`, by slug, ascending.
--
-- A file name is slugified, not validated, so a stem that is not already a slug
-- yields a slug naming a DIFFERENT path. Pawl.CardSpec pins every committed file
-- name to its own slug so that never arises in the corpus. Non-.json entries are
-- ignored outright -- a README is not a broken card.
slugsIn :: FilePath -> IO [Slug.Slug]
slugsIn root =
  let json = ".json"
      stem name = take (length name - length json) name
      toSlug = Slug.fromText . Text.pack . stem
   in do
        entries <- Directory.listDirectory root
        pure $ fmap toSlug (List.sort (filter (List.isSuffixOf json) entries))

-- Read and parse every card file in `root`, keeping each slug beside its result.
--
-- Per-card Either rather than an exception, so a sweep can name every bad file
-- in one run instead of dying on the first. That is the whole reason a lint
-- wants a different shape from a lookup: a registry answering one question
-- should fail; a linter answering 180 should report.
loadAll :: FilePath -> IO [(Slug.Slug, Either CardError.CardError Card.Card)]
loadAll root = do
  found <- slugsIn root
  mapM (\slug -> fmap ((,) slug) (loadOne root slug)) found

-- The lint's half of the read/parse split described on Pawl.Registry.parseCard.
-- No tryIOError, unlike Pawl.Registry.loadFile: every slug here came from
-- slugsIn listing the directory a moment ago, so a file that is not there is a
-- broken invariant rather than a card the corpus declines to contain -- and an
-- exception is the right way for that to surface.
--
-- The name is derived from the slug because a sweep has no name to have been
-- asked for. It only ever reaches the error message.
loadOne :: FilePath -> Slug.Slug -> IO (Either CardError.CardError Card.Card)
loadOne root slug = do
  let path = Registry.cardPath root slug
      name = CardName.MkCardName (Slug.unwrap slug)
  bytes <- ByteString.readFile path
  pure (either (Left . CardError.Invalid name . (<>) (path <> ": ") . Text.unpack) Right (Registry.parseFiledCard slug bytes))
