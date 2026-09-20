-- Covers Pawl.DeckList. Every case builds its own corpus in a temporary
-- directory, for Pawl.RegistrySpec's reason: what a deck list has to resolve is
-- a name, and a two-faced card plus a one-faced one is the whole vocabulary
-- these cases need.
module Pawl.DeckListSpec where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.DeckList as DeckList
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Deck as Deck
import qualified Pawl.Types.Printing as Printing

-- A registry over Goblin Piker and Wax // Wane, which between them are a name
-- with no halves and CR 709.4a's name with two.
withPool :: String -> (Registry.Registry IO -> IO a) -> IO a
withPool label action = do
  piker <- S.pikerJson
  waxWane <- S.waxWaneJson
  S.withCorpusDir label [("goblin-piker.json", piker), ("wax-wane.json", waxWane)] (Registry.fileRegistry Monad.>=> action)

-- A section's contents as sorted (name, count) pairs. Named through
-- Pawl.Engine.Card.combined rather than Pawl.DeckList.written, so what an
-- assertion reads is not the same function the parser and renderer agree by.
entriesOf :: Map.Map Printing.Printing Natural.Natural -> [(Text.Text, Natural.Natural)]
entriesOf cards = List.sort [(CardName.unwrap (S.nameOf (Printing.card printing)), count) | (printing, count) <- Map.toList cards]

-- The deck a list means, failing the case rather than escaping when a line did
-- not resolve.
deckOf :: Spec.Spec IO n -> String -> Registry.Registry IO -> String -> IO Deck.Deck
deckOf s label registry text = do
  result <- DeckList.parse registry (Text.pack text)
  case result of
    Right deck -> pure deck
    Left problems -> Spec.assertFailure s (label <> ": " <> show (fmap DeckList.explain problems))

-- The problems a list raises, failing the case when it unexpectedly parses.
problemsOf :: Spec.Spec IO n -> String -> Registry.Registry IO -> String -> IO [DeckList.Problem]
problemsOf s label registry text = do
  result <- DeckList.parse registry (Text.pack text)
  case result of
    Left problems -> pure problems
    Right _ -> Spec.assertFailure s (label <> ": expected the list to be rejected")

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.DeckList" $ do
  Spec.it s "a count and a name become that many copies of one card"
    . withPool "counted"
    $ \registry -> do
      deck <- deckOf s "counted" registry "4 Goblin Piker\n"
      Spec.assertEqWith s "four Pikers" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]

  -- Blank lines separate sections in every export, and the same name on two
  -- lines is one entry: Pawl.Types.Deck.cards is a multiset, so a list writing
  -- "2 Piker" twice means four rather than the later line winning.
  Spec.it s "blank lines are skipped and repeated names add up"
    . withPool "repeated"
    $ \registry -> do
      deck <- deckOf s "repeated" registry "2 Goblin Piker\n\n2 Goblin Piker\n"
      Spec.assertEqWith s "two and two" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]

  Spec.it s "a bare name is one copy"
    . withPool "bare"
    $ \registry -> do
      deck <- deckOf s "bare" registry "Goblin Piker\n"
      Spec.assertEqWith s "one Piker" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 1)]

  -- CR 709.4a gives a split card two names and no combined one, and
  -- Pawl.RegistrySpec's "a split card is found by either of its names" pins
  -- that the joined string is not one a lookup may ask for. A deck list writes
  -- exactly that joined string, so splitting it is the parser's job (#679).
  -- The two spellings are the printed, spaced one and Pawl.Types.CardName.join's
  -- unspaced one, which is what docs/rules.txt's examples use; both are lists a
  -- person could hand pawl.
  Spec.it s "CR 709.4a a split card's joined name resolves, spaced or not"
    . withPool "joined"
    $ \registry -> do
      -- Either half on its own is already a name the card has (#649), so this
      -- is the deck the two joined spellings have to mean as well. Read
      -- through the parser rather than built by hand so the comparison is
      -- between two deck lists rather than between a list and a fixture.
      half <- deckOf s "half" registry "3 Wane\n"
      Spec.assertEqWith s "one card, three times" (entriesOf (Deck.cards half)) [(Text.pack "Wax//Wane", 3)]
      spaced <- DeckList.parse registry (Text.pack "3 Wax // Wane\n")
      Spec.assertEqWith s "the printed spelling is that same deck" spaced (Right half)
      unspaced <- DeckList.parse registry (Text.pack "3 Wax//Wane\n")
      Spec.assertEqWith s "and so is the unspaced spelling" unspaced (Right half)

  -- A joined name whose halves are two cards names no one card. Without this
  -- the parser could take the first half and silently drop the rest, which
  -- would make "Wax // Goblin Piker" a legal way to write three Waxes.
  Spec.it s "CR 709.4a a joined name whose halves disagree is rejected"
    . withPool "disagree"
    $ \registry -> do
      problems <- problemsOf s "disagree" registry "3 Wax // Goblin Piker\n"
      Spec.assertEqWith s "the one line" problems [DeckList.HalvesDisagree 1 (CardName.MkCardName (Text.pack "Wax // Goblin Piker"))]

  -- CR 100.4: a sideboard is a group of cards beside the deck rather than part
  -- of it, which is why Pawl.Types.Deck keeps the two apart. Distinct cards on
  -- the two sides, so a parser that put everything in one field could not pass.
  Spec.it s "CR 100.4 lines after a sideboard header are the sideboard"
    . withPool "sideboard"
    $ \registry -> do
      deck <- deckOf s "sideboard" registry "4 Goblin Piker\n\nSideboard\n2 Wax // Wane\n"
      Spec.assertEqWith s "the deck" (entriesOf (Deck.cards deck)) [(Text.pack "Goblin Piker", 4)]
      Spec.assertEqWith s "and the sideboard" (entriesOf (Deck.sideboard deck)) [(Text.pack "Wax//Wane", 2)]

  -- Every bad line is reported rather than the first, for Pawl.Registry.index's
  -- reason: a report naming one offender at a time is read once per offender.
  Spec.it s "an unknown name is reported with its line, and every bad line is named"
    . withPool "unknown"
    $ \registry -> do
      problems <- problemsOf s "unknown" registry "4 Goblin Piker\n2 Shivan Dragon\n4Bolt\n"
      Spec.assertEqWith
        s
        "the second line and the third, not the first"
        problems
        [ DeckList.NoSuchCard 2 (CardName.MkCardName (Text.pack "Shivan Dragon")),
          DeckList.Unreadable 3 (Text.pack "4Bolt")
        ]

  -- The rendered list is itself a deck list, which is what makes the output of
  -- `pawl deck` something a person can hand back. A split card renders in the
  -- printed, spaced spelling, so the round trip is what pins that the parser
  -- accepts it (#679).
  Spec.it s "a rendered deck parses back to the same deck"
    . withPool "round-trip"
    $ \registry -> do
      deck <- deckOf s "round-trip" registry "4 Goblin Piker\n\nSideboard\n2 Wax // Wane\n"
      let text = DeckList.render deck
      Spec.assertBool s (Text.isInfixOf (Text.pack "2 Wax // Wane") text) ("the printed spelling: " <> show text)
      again <- deckOf s "round-trip-again" registry (Text.unpack text)
      Spec.assertEqWith s "the same deck" again deck
