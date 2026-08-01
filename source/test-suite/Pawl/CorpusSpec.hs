-- Covers Pawl.Corpus: reading the whole pool as data, which is what the
-- corpus-wide lints in Pawl.CardSpec and Pawl.CardsSpec are built on. Every case
-- builds its own throwaway directory, since the committed data/cards is
-- read-only here and by construction contains no bad file to find.
module Pawl.CorpusSpec where

import qualified Data.Text as Text
import qualified Pawl.Corpus as Corpus
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Card as Card

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
      Spec.assertEqWith s "one card, by name" [Card.name c | (_, Right c) <- loaded] [Text.pack "Goblin Piker"]
