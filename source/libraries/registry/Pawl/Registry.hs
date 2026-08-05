-- Answering one question: given a card's name, what card is that?
--
-- The registry is that function and nothing else. It is parameterized over the
-- caller's monad, so a caller who already has the cards -- a fixture map, a
-- pool compiled in, a generated one -- can supply their own without pretending
-- to do IO. Failure is a returned value rather than an exception for the same
-- reason: a pure registry cannot throw. How a registry answers is not part of
-- the type.
--
-- Enumerating the pool is deliberately NOT in the INTERFACE: every caller that
-- wanted it was linting the corpus pawl ships, which is a claim about the data
-- rather than a question for a registry, and that lives in the test suite. What
-- the test suite does borrow is cardPath, parseCard, loadRoot and filedAs --
-- facts about the on-disk format rather than about looking a card up.
module Pawl.Registry where

import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Paths_pawl as Paths
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Exceptions.InvalidCorpus as InvalidCorpus
import qualified Pawl.Exceptions.MissingRoot as MissingRoot
import qualified Pawl.Slug as Slug
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardError as CardError
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face
import qualified System.Directory as Directory

newtype Registry m = MkRegistry
  { fetchCard :: CardName.CardName -> m (Either CardError.CardError Card.Card)
  }

-- A card by name ("Goblin Piker") or by slug ("goblin-piker") -- a file-backed
-- registry slugifies, and slugify is idempotent, so both are the same lookup.
-- Takes a String because pawl does not enable OverloadedStrings: the newtype is
-- what the interface speaks, this is what callers write.
named :: Registry m -> String -> m (Either CardError.CardError Card.Card)
named registry = fetchCard registry . CardName.MkCardName . Text.pack

-- The card corpus's default root: data/cards declared as cabal data-files,
-- resolved through the cabal-generated Paths_pawl so an installed pawl finds
-- its cards. A default, not a hidden global -- 'fileRegistry' still takes an
-- explicit FilePath.
defaultRoot :: IO FilePath
defaultRoot = Paths.getDataFileName "cards"

-- A registry over a directory of one-card-per-file JSON, read once, here.
--
-- The root is checked before anything else because a mistyped --cards-dir
-- otherwise surfaces as N identical failures, one per card, instead of one
-- clear failure at startup (#167). Every other way the pool can be broken is
-- reported the same way and at the same moment, by `index`.
--
-- Eager rather than memoized per name: the map makes "each file is parsed at
-- most once" true by construction, where the MVar it replaces made it true by
-- holding a lock across a read (#265). The cost is that a caller wanting one
-- card parses the whole pool; the pool is hand-authored and card-driven, and
-- the test suite already reads it whole many times over per run --
-- Pawl.Support.allPrintings alone is unmemoized and is called repeatedly from
-- CardSpec, CardsSpec and CodecIntegrationSpec.
fileRegistry :: FilePath -> IO (Registry IO)
fileRegistry root = do
  exists <- Directory.doesDirectoryExist root
  if not exists
    then Exception.throwIO MissingRoot.MkMissingRoot {MissingRoot.path = root}
    else do
      loaded <- loadRoot root
      case index loaded of
        Left problems -> Exception.throwIO InvalidCorpus.MkInvalidCorpus {InvalidCorpus.root = root, InvalidCorpus.problems = problems}
        Right cards ->
          pure (MkRegistry (\name -> pure (maybe (Left (CardError.Missing name)) Right (Map.lookup (slugFor name) cards))))

-- The slug a name is looked up by. `named` accepts a name or a slug for the
-- same reason: slugify is idempotent, so the two are one lookup.
slugFor :: CardName.CardName -> Slug.Slug
slugFor = Slug.fromText . CardName.unwrap

-- Every name every card has, pointing at that card -- or every reason the pool
-- cannot be indexed.
--
-- CR 709.4a: "Each split card has two names." Both are keys here, which is what
-- makes looking up either half a hit rather than a scan. CR 715.4 gives an
-- adventurer card only its normal characteristics off the stack, so its
-- alternative name is not a name it HAS the way a split card's two are -- but
-- CR 715.5 lets a player CHOOSE that alternative name wherever an effect asks
-- for a card name, which a name-keyed lookup has to answer to all the same.
-- CR 720.5 does the same for an omen card's alternative name.
--
-- A name claimed twice is fatal wherever it comes from -- two cards, or one card
-- repeating its own face name. Pawl.CardSpec's "a card's face names are pairwise
-- distinct" lint is the INTRA-card half of that and holds only over the corpus
-- pawl ships; this holds over whatever root a caller points at.
index :: [(FilePath, Either Text.Text Card.Card)] -> Either [String] (Map.Map Slug.Slug Card.Card)
index loaded =
  let named' = [(path, card) | (path, Right card) <- loaded]
      keyed = [(slugFor (Face.name face), (path, card)) | (path, card) <- named', face <- NonEmpty.toList (Card.faces card)]
      unparsed = [path <> ": " <> Text.unpack reason | (path, Left reason) <- loaded]
      claims = Map.fromListWith (<>) [(slug, [path]) | (slug, (path, _)) <- keyed]
      -- A single path claiming its own slug more than once is one card
      -- repeating a face name, not two cards colliding -- List.nub tells the
      -- two apart so the message names what actually happened instead of
      -- rendering the same path twice, which reads like a bug in the report
      -- rather than a description of the pool.
      ambiguous =
        [ case List.nub (List.sort paths) of
            [one] -> Text.unpack (Slug.unwrap slug) <> " is claimed by " <> one <> ", which repeats it across " <> show (length paths) <> " of its own faces"
            distinct -> Text.unpack (Slug.unwrap slug) <> " is claimed by " <> List.intercalate ", " distinct
        | (slug, paths) <- Map.toAscList claims,
          length paths > 1
        ]
   in case unparsed <> ambiguous of
        [] -> Right (Map.fromList [(slug, card) | (slug, (_, card)) <- keyed])
        problems -> Left problems

-- Where the file for a given slug lives, one file per card. What the slug IS
-- varies by caller: for a single-faced card it is the card's own name, but
-- `filedAs` below derives it from a split or adventurer card's JOINED face
-- names instead, so "the card's own name" does not hold in general.
cardPath :: FilePath -> Slug.Slug -> FilePath
cardPath root slug = root <> "/" <> Text.unpack (Slug.unwrap slug) <> ".json"

-- What a card file's bytes mean. Pure, and separate from reading them, because
-- that is exactly what a lookup and the corpus-wide lints agree about; they
-- disagree about how to obtain the bytes and about what a failure to obtain
-- them means. The caller pairs the reason with the path.
--
-- Decoded as UTF-8 explicitly rather than via Data.Text.IO.readFile, which
-- decodes using the locale encoding -- ASCII under LC_ALL=C -- so a card with a
-- non-ASCII character would fail with "invalid byte sequence" instead of naming
-- the offending file. The caller pairs the reason with the path.
--
-- Says NOTHING about where the file was found: that is `filedAs` below, and
-- keeping the two apart is what lets a root be read without a filename deciding
-- what any of it answers for.
parseCard :: ByteString.ByteString -> Either Text.Text Card.Card
parseCard bytes = do
  contents <- either (\err -> Left (Text.pack ("not valid UTF-8: " <> show err))) Right (Encoding.decodeUtf8' bytes)
  Common.parse contents >>= Card.fromJson

-- The slug a card's file is named for: its face names joined, slugified.
--
-- A FILING convention and not a name the card has -- CR 709.4a gives a split
-- card two names and no combined one (#649) -- so this decides where a file
-- LIVES and never what a lookup may ask for. Also not Pawl.Engine.Card.combined's
-- name, which is a different question and only coincidentally the same string:
-- CR 715.4 gives an adventurer card its normal face's name alone, so the
-- combined view would name embereth-shieldbreaker while the file is
-- embereth-shieldbreaker-battle-display.
filedAs :: Card.Card -> Slug.Slug
filedAs = Slug.fromText . CardName.unwrap . CardName.join . fmap Face.name . Card.faces

-- Every card file in a root, ascending by path, each paired with what its bytes
-- mean. One IO pass over the whole directory: this is what a registry is built
-- from and what the corpus-wide lints sweep, so neither has to restate how a
-- pool is enumerated.
--
-- Per-file Either rather than an exception, so one pass can name every bad file
-- instead of dying on the first.
--
-- Sorted so that what a caller reports first is a fact about the pool rather
-- than about the order a directory happens to enumerate in. Non-.json entries
-- are ignored outright -- a README is not a broken card.
loadRoot :: FilePath -> IO [(FilePath, Either Text.Text Card.Card)]
loadRoot root = do
  entries <- fmap List.sort (Directory.listDirectory root)
  let paths = fmap (\entry -> root <> "/" <> entry) (filter (List.isSuffixOf ".json") entries)
  mapM (\path -> fmap (\bytes -> (path, parseCard bytes)) (ByteString.readFile path)) paths
