module Pawl.Types.CardName where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

-- | A card's printed name ("Goblin Piker"), as a registry is asked for it.
--
-- Not a Pawl.Slug: a name is what a card calls itself and what a caller types,
-- and slugifying it is a registry's business rather than a caller's. A
-- file-backed registry slugifies to find a path; a map-backed one need not
-- slugify at all.
newtype CardName = MkCardName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- CR 709.4a as far as a single name can carry it (#650): docs/rules.txt's own
-- Examples write a split card's name joined by "//", unspaced -- "Fire//Ice"
-- (lines 3882, 5747) and "Assault//Battery" (line 5746). CR 709.4a itself
-- gives no example.
--
-- Unspaced rather than the printed "Wax // Wane" is a decision already made,
-- not an open question: Pawl.Slug.fromText maps '/' to a space and splits on
-- words, so "Wax//Wane" and "Wax // Wane" both slugify to "wax-wane" --
-- Task 5's filename check cannot diverge either way, whichever this writes.
--
-- Here rather than in Pawl.Engine.Card because `registry` sits BEFORE `engine`
-- in the sublibrary table and cannot reach it, while both sit after `types`.
-- Pawl.Engine.Card.combined and Pawl.Registry.parseCard must agree on this
-- string exactly -- the filename a card is filed under is its slug -- and two
-- copies of an intercalation are how that quietly stops being true.
join :: NonEmpty.NonEmpty CardName -> CardName
join names = MkCardName (Text.intercalate (Text.pack "//") (fmap unwrap (NonEmpty.toList names)))
