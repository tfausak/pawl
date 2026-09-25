-- Answering one question: given the text a player would actually type, what
-- deck is that?
--
-- A deck list is a count and a card name per line, with CR 100.4's sideboard
-- written as a section of its own. Every name is resolved through a
-- Pawl.Registry, which is the only thing that knows what cards exist; this
-- module decides what a LINE means and never what a name means.
--
-- Here rather than in the test suite or the executable because both of those
-- already build their decks in Haskell and a deck list is what they cannot
-- say. Here rather than in `types` because resolving a name needs a registry.
module Pawl.DeckList where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Registry as Registry
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Printing as Printing
import qualified Text.Read as Read

-- | Why a deck list's line did not become a deck entry, and which line it was.
-- Every arm carries the one-based line number, since a report that cannot say
-- WHERE is not worth reading.
--
-- A returned value rather than an exception, for Pawl.Registry's reason: a
-- parse over a pure registry cannot throw. Every bad line is reported, not just
-- the first.
data Problem
  = -- | A line that is neither blank, a section header, nor a count and a name.
    Unreadable Natural.Natural Text.Text
  | -- | A name -- or, for a joined name, one of its halves -- that no card has.
    NoSuchCard Natural.Natural CardName.CardName
  | -- | CR 709.4a: a joined name whose halves name two different cards, so the
    -- line names no one card.
    HalvesDisagree Natural.Natural CardName.CardName
  deriving (Eq, Ord, Show)

-- | A problem as a line a person can read.
explain :: Problem -> Text.Text
explain problem =
  let at n = Text.pack ("line " <> show n <> ": ")
   in case problem of
        Unreadable n content -> at n <> Text.pack "not a count and a card name: " <> content
        NoSuchCard n name -> at n <> Text.pack "no card is named " <> CardName.unwrap name
        HalvesDisagree n name -> at n <> Text.pack "the halves of " <> CardName.unwrap name <> Text.pack " are different cards"

-- | Which of a deck list's two sections the lines that follow belong to. CR
-- 100.4 makes the sideboard a group of cards beside the deck rather than part
-- of it, which is why Pawl.Types.Deck keeps the two apart.
data Section
  = Main
  | Side
  deriving (Eq, Ord, Show)

-- | The deck a deck list describes, or every line that stopped it.
--
-- The two sections a deck list has are the two fields this fills. CR 903.3's
-- commander, CR 902.3's vanguard card and CR 309.2's dungeon cards are brought
-- alongside a deck rather than written on its list, so a caller in one of those
-- formats designates them itself.
parse :: (Monad m) => Registry.Registry m -> Text.Text -> m (Either [Problem] Deck.Deck)
parse registry text = do
  results <- mapM (lineOf registry) (List.zip [1 ..] (Text.lines text))
  let step (section, entries, problems) result = case result of
        Left problem -> (section, entries, problem : problems)
        Right Nothing -> (section, entries, problems)
        Right (Just (Left next)) -> (next, entries, problems)
        Right (Just (Right (count, card))) -> (section, (section, card, count) : entries, problems)
      (_, found, bad) = List.foldl' step (Main, [], []) results
      gather want = Map.fromListWith (+) [(Printing.ofCard card, count) | (section, card, count) <- found, section == want, count /= 0]
      deck = Deck.fromCards (gather Main)
   in pure $ case List.reverse bad of
        [] -> Right deck {Deck.sideboard = gather Side}
        problems -> Left problems

-- What one line means: nothing (blank), the section the lines after it belong
-- to, or a count and the card it names.
lineOf :: (Monad m) => Registry.Registry m -> (Natural.Natural, Text.Text) -> m (Either Problem (Maybe (Either Section (Natural.Natural, Card.Card))))
lineOf registry (number, raw) =
  let trimmed = Text.strip raw
   in if Text.null trimmed
        then pure (Right Nothing)
        else case sectionNamed trimmed of
          Just section -> pure (Right (Just (Left section)))
          Nothing -> case entryOf trimmed of
            Nothing -> pure (Left (Unreadable number trimmed))
            Just (count, name) -> fmap (fmap (\card -> Just (Right (count, card)))) (resolve registry number name)

-- The section a header line names, if it is one. Only the two headers a deck
-- list actually carries: everything else on a line of its own is a card name
-- with an implicit count of one, so this set stays closed rather than
-- swallowing lines it does not understand.
sectionNamed :: Text.Text -> Maybe Section
sectionNamed text = case Text.unpack (Text.toLower (Text.filter (/= ':') text)) of
  "deck" -> Just Main
  "maindeck" -> Just Main
  "main deck" -> Just Main
  "sideboard" -> Just Side
  _ -> Nothing

-- A line's count and the name it precedes. A bare name is one copy, which is
-- how every export writes a singleton deck's list.
--
-- Everything after the count is the name, so an export that appends a set code
-- and a collector number ("4 Lightning Bolt (M10) 146") names no card here and
-- is reported as such rather than guessed at.
entryOf :: Text.Text -> Maybe (Natural.Natural, CardName.CardName)
entryOf text =
  let (digits, rest) = Text.span Char.isDigit text
      named = CardName.MkCardName . Text.strip
   in if Text.null digits
        then Just (1, named text)
        else do
          count <- Read.readMaybe (Text.unpack digits)
          -- "4Bolt" is not four Bolts: a count is a word of its own, and the
          -- alternative reading would silently accept a name beginning with a
          -- digit as a count plus the rest of itself.
          stripped <- if Text.null rest || Char.isSpace (Text.head rest) then Just (Text.strip rest) else Nothing
          if Text.null stripped then Nothing else Just (count, named stripped)

-- The card a deck list's name means.
--
-- CR 709.4a gives a split card two names and no combined one, but every deck
-- list writes both joined ("Wax // Wane"). The joined string is not a name a
-- card has, so the registry rightly refuses it (#649); splitting it here is
-- what lets a deck list say what players write without the lookup learning
-- about "//" again. Both halves are resolved and must agree, so a line naming
-- two cards is an error rather than a silent choice of the first.
--
-- Splitting on "//" and trimming reconciles the two spellings in the tree at
-- once: the printed, spaced "Wax // Wane" and Pawl.Types.CardName.join's
-- unspaced "Wax//Wane", which is what docs/rules.txt's own examples use.
resolve :: (Monad m) => Registry.Registry m -> Natural.Natural -> CardName.CardName -> m (Either Problem Card.Card)
resolve registry number name = do
  found <- mapM (Registry.fetchCard registry) (halvesOf name)
  pure $ case sequence found of
    Nothing -> Left (NoSuchCard number name)
    Just (card NonEmpty.:| rest)
      | all (== card) rest -> Right card
      | otherwise -> Left (HalvesDisagree number name)

-- The names a written name joins, which for every name but a split card's is
-- the one name itself.
halvesOf :: CardName.CardName -> NonEmpty.NonEmpty CardName.CardName
halvesOf name =
  let parts = filter (not . Text.null) (fmap Text.strip (Text.splitOn (Text.pack "//") (CardName.unwrap name)))
   in case NonEmpty.nonEmpty parts of
        Nothing -> NonEmpty.singleton name
        Just halves -> fmap CardName.MkCardName halves

-- | A deck as a deck list, which `parse` reads back.
--
-- Sorted by the name written, so the text is a fact about the deck rather than
-- about the order a Map enumerates its structural keys in.
render :: Deck.Deck -> Text.Text
render deck =
  let entries m = fmap line (List.sortOn snd [(count, written (Printing.card printing)) | (printing, count) <- Map.toList m])
      line (count, name) = Text.pack (show count) <> Text.singleton ' ' <> name
      side = entries (Deck.sideboard deck)
      body = entries (Deck.cards deck) <> if null side then [] else Text.empty : Text.pack "Sideboard" : side
   in Text.unlines body

-- A card's name as a deck list writes it: its face names joined by a SPACED
-- "//", which is how every printing and every export spells a split card's name
-- (#679). Pawl.Types.CardName.join writes the same names unspaced, which is
-- docs/rules.txt's spelling; `halvesOf` accepts either.
written :: Card.Card -> Text.Text
written = Text.intercalate (Text.pack " // ") . NonEmpty.toList . fmap (CardName.unwrap . Face.name) . Card.faces
