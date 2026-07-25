-- Covers Pawl.Slug and Pawl.Type.Slug.
module Pawl.SlugSpec where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Pawl.Slug as Slug
import qualified Pawl.Type.Slug as Slug.Type
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

-- slugify's Text output, for assertions that only care about the text and not
-- the Slug wrapper.
slugifyText :: String -> Maybe Text.Text
slugifyText = fmap Slug.Type.slugToText . Slug.slugify . Text.pack

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.SlugSpec"
    [ Tasty.testGroup
        "slugify"
        [ HU.testCase "a plain name" $
            HU.assertEqual "goblin-piker" (Just (Text.pack "goblin-piker")) (slugifyText "Goblin Piker"),
          HU.testCase "an apostrophe is dropped, not separated" $
            HU.assertEqual "serpents-gift" (Just (Text.pack "serpents-gift")) (slugifyText "Serpent's Gift"),
          HU.testCase "an accented letter folds to ASCII" $
            HU.assertEqual "khabal-ghoul" (Just (Text.pack "khabal-ghoul")) (slugifyText "Khabál Ghoul"),
          HU.testCase "a comma is one separator, not two" $
            HU.assertEqual "inner-calm" (Just (Text.pack "inner-calm-outer-strength")) (slugifyText "Inner Calm, Outer Strength"),
          HU.testCase "a split card's slashes collapse" $
            HU.assertEqual "fire-ice" (Just (Text.pack "fire-ice")) (slugifyText "Fire // Ice"),
          HU.testCase "trailing punctuation is trimmed" $
            HU.assertEqual "no trailing hyphen" (Just (Text.pack "sword-of-dungeons-dragons")) (slugifyText "Sword of Dungeons & Dragons®"),
          HU.testCase "the eszett folds to ss without a table entry" $
            HU.assertEqual "strasse" (Just (Text.pack "strasse")) (slugifyText "Straße"),
          HU.testCase "digits survive; the comma between them separates" $
            HU.assertEqual "borrowing-100-000-arrows" (Just (Text.pack "borrowing-100-000-arrows")) (slugifyText "Borrowing 100,000 Arrows"),
          HU.testCase "a run of underscores collapses to one, not nothing" $
            HU.assertEqual "_" (Just (Text.pack "_")) (slugifyText "_____"),
          HU.testCase "a name with nothing to keep slugifies to Nothing" $
            HU.assertEqual "nothing" Nothing (slugifyText "!!!"),
          HU.testCase "an underscore run inside a name keeps its blank" $
            HU.assertEqual
              "knight-in-_-armor"
              (Just (Text.pack "knight-in-_-armor"))
              (slugifyText "Knight in _____ Armor"),
          HU.testCase "a leading underscore run keeps its blank" $
            HU.assertEqual "_-goblin" (Just (Text.pack "_-goblin")) (slugifyText "_____ Goblin"),
          HU.testCase "a doubled underscore between alphanumerics keeps its blank" $
            HU.assertEqual "foo_bar" (Just (Text.pack "foo_bar")) (slugifyText "Foo__Bar"),
          QC.testProperty "idempotent: a slug slugifies to itself" $
            \s -> case Slug.slugify (Text.pack s) of
              Nothing -> QC.property True
              Just slug -> Slug.slugify (Slug.Type.slugToText slug) QC.=== Just slug,
          QC.testProperty "the output is ASCII [a-z0-9-_] throughout" $
            \s ->
              let ok c = Char.isAsciiLower c || Char.isDigit c || c == '-' || c == '_'
               in case Slug.slugify (Text.pack s) of
                    Nothing -> QC.property True
                    Just slug -> QC.property (Text.all ok (Slug.Type.slugToText slug)),
          QC.testProperty "no leading, trailing, or doubled hyphen, and no doubled underscore" $
            \s -> case Slug.slugify (Text.pack s) of
              Nothing -> QC.property True
              Just slug ->
                let t = Slug.Type.slugToText slug
                 in QC.property
                      ( not (Text.isPrefixOf (Text.pack "-") t)
                          && not (Text.isSuffixOf (Text.pack "-") t)
                          && not (Text.isInfixOf (Text.pack "--") t)
                          && not (Text.isInfixOf (Text.pack "__") t)
                      ),
          -- The payoff: textToSlug is slugify's own validation, applied to
          -- slugify's own output, so drifting either one apart fails this.
          QC.testProperty "everything slugify produces is accepted by textToSlug" $
            \s ->
              let slug = Slug.slugify (Text.pack s)
               in QC.property
                    (maybe True (\sg -> Slug.Type.textToSlug (Slug.Type.slugToText sg) == Just sg) slug)
        ],
      Tasty.testGroup
        "textToSlug"
        [ HU.testCase "accepts what slugify produces" $
            HU.assertEqual "goblin-piker" (Just (Text.pack "goblin-piker")) (fmap Slug.Type.slugToText (Slug.Type.textToSlug (Text.pack "goblin-piker"))),
          HU.testCase "rejects a raw, unslugified name" $
            HU.assertEqual "Goblin Piker" Nothing (Slug.Type.textToSlug (Text.pack "Goblin Piker")),
          HU.testCase "rejects a leading hyphen" $
            HU.assertEqual "-x" Nothing (Slug.Type.textToSlug (Text.pack "-x")),
          HU.testCase "rejects a trailing hyphen" $
            HU.assertEqual "x-" Nothing (Slug.Type.textToSlug (Text.pack "x-")),
          HU.testCase "rejects a doubled hyphen" $
            HU.assertEqual "a--b" Nothing (Slug.Type.textToSlug (Text.pack "a--b")),
          HU.testCase "rejects a doubled underscore" $
            HU.assertEqual "a__b" Nothing (Slug.Type.textToSlug (Text.pack "a__b")),
          HU.testCase "rejects the empty text" $
            HU.assertEqual "empty" Nothing (Slug.Type.textToSlug Text.empty),
          HU.testCase "accepts a lone underscore (Unhinged's _____)" $
            HU.assertEqual "_" (Just (Text.pack "_")) (fmap Slug.Type.slugToText (Slug.Type.textToSlug (Text.pack "_")))
        ]
    ]
