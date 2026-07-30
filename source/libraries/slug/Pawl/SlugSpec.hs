module Pawl.SlugSpec where

import qualified Data.Text as Text
import qualified Pawl.Slug as Slug
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Slug" $ do
  Spec.describe s "fromText" $ do
    Spec.it s "works with the empty string" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "") . Slug.UnsafeSlug $ Text.pack ""

    Spec.it s "works with a letter" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "a") . Slug.UnsafeSlug $ Text.pack "a"

    Spec.it s "works with a number" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "1") . Slug.UnsafeSlug $ Text.pack "1"

    Spec.it s "works with multiple letters" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "bc") . Slug.UnsafeSlug $ Text.pack "bc"

    Spec.it s "lower cases" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "D") . Slug.UnsafeSlug $ Text.pack "d"

    Spec.it s "strips leading blanks" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack " e") . Slug.UnsafeSlug $ Text.pack "e"

    Spec.it s "strips trailing blanks" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "f ") . Slug.UnsafeSlug $ Text.pack "f"

    Spec.it s "transliterates" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "\xe0") . Slug.UnsafeSlug $ Text.pack "a"

    Spec.it s "hyphenates" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "g h") . Slug.UnsafeSlug $ Text.pack "g-h"

    Spec.it s "collapses runs of underscores" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "__") . Slug.UnsafeSlug $ Text.pack "_"

    Spec.it s "strips apostrophes" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "Serpent's Gift") . Slug.UnsafeSlug $ Text.pack "serpents-gift"

    Spec.it s "strips commas" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "Inner Calm, Outer Strength") . Slug.UnsafeSlug $ Text.pack "inner-calm-outer-strength"

    Spec.it s "strips slashes" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "Fire // Ice") . Slug.UnsafeSlug $ Text.pack "fire-ice"

    Spec.it s "strips ampersands" $ do
      Spec.assertEq s (Slug.fromText $ Text.pack "Sword of Dungeons & Dragons") . Slug.UnsafeSlug $ Text.pack "sword-of-dungeons-dragons"
