-- Covers Pawl.Corpus: reading the whole pool as data, which is what the
-- corpus-wide lints in Pawl.CardSpec and Pawl.CardsSpec are built on. Every case
-- builds its own throwaway directory, since the committed data/cards is
-- read-only here and by construction contains no bad file to find.
module Pawl.CorpusSpec where

import qualified Data.Text as Text
import qualified Pawl.Corpus as Corpus
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Registry as Registry
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Face as Face

spec :: (Monad n) => Spec.Spec IO n -> n ()
spec s = Spec.describe s "Pawl.Corpus" $ do
  -- A CLI, a scenario loader, a deckbuilder and a linter all want "every card
  -- in this pool". The listing IS the pool: a hand-kept list is exactly what
  -- forgets the file nobody loads.
  Spec.it s "slugsIn enumerates the pool in ascending order, ignoring non-.json entries" $ do
    piker <- S.pikerJson
    S.withCorpusDir
      "enumerate"
      [ ("goblin-piker.json", piker),
        ("bird-maiden.json", piker),
        ("README.md", Text.pack "not a card")
      ]
      $ \root -> do
        found <- Corpus.slugsIn root
        Spec.assertEqWith s "sorted, .json only" (fmap Slug.unwrap found) [Text.pack "bird-maiden", Text.pack "goblin-piker"]

  Spec.it s "loadAll loads every card the pool enumerates" $ do
    piker <- S.pikerJson
    S.withCorpusDir "load-all" [("goblin-piker.json", piker)] $ \root -> do
      loaded <- Corpus.loadAll root
      Spec.assertEqWith s "one card, by name" [Face.name (Card.combined c) | (_, Right c) <- loaded] [CardName.MkCardName $ Text.pack "Goblin Piker"]

  -- What a registry is built from and what the corpus-wide lints sweep, so
  -- neither restates how a pool is enumerated. Ascending, .json only, and a
  -- file that will not parse is a reported Left rather than a thrown exception
  -- -- one pass names every bad file instead of dying on the first.
  Spec.it s "loadRoot pairs every card file with what its bytes mean" $ do
    piker <- S.pikerJson
    S.withCorpusDir
      "load-root"
      [ ("goblin-piker.json", piker),
        ("bird-maiden.json", Text.pack "{oh no"),
        ("README.md", Text.pack "not a card")
      ]
      $ \root -> do
        loaded <- Registry.loadRoot root
        Spec.assertEqWith s "ascending, .json only" (fmap fst loaded) [root <> "/bird-maiden.json", root <> "/goblin-piker.json"]
        Spec.assertEqWith s "the good one parsed" [Face.name (Card.combined c) | (_, Right c) <- loaded] [CardName.MkCardName $ Text.pack "Goblin Piker"]
        Spec.assertEqWith s "and the bad one is reported, not thrown" (length [reason | (_, Left reason) <- loaded]) 1
