module Pawl.Types.CounterName where

import qualified Data.Set as Set
import qualified Data.Text as Text

-- | The NAME a card prints for a counter kind no rule in the CR reads --
-- Zhao, the Moon Slayer's conqueror counter, Gemstone Caverns' luck counter.
-- CR 122.1's last sentence is the whole citation: "Counters with the same name
-- or description are interchangeable", so the name IS the kind's identity and
-- exact equality on this text is exact equality of kind.
--
-- AbilityName's and SlotName's shape, with one difference: 'UnsafeMkCounterName'
-- rather than @MkCounterName@, because 'make' maintains an invariant the bare
-- constructor sidesteps. Ord is load-bearing for the same reason
-- 'Pawl.Types.CounterKind.CounterKind''s is: this ends up inside a Map key.
--
-- NOT normalized -- no case folding, no slugging. CR 122.1 says nothing about
-- either, so a policy the rules do not state would be pawl inventing one;
-- equality is exact 'Text.Text' equality.
newtype CounterName = UnsafeMkCounterName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | The spellings a card may NOT name, because 'Pawl.Types.CounterKind' already
-- has a constructor for each: a card writing @Named "flying"@ while another
-- writes @Keyword Flying@ would make two Map keys on @Object.counters@ for one
-- rules object, and every count read would split between them. CR 122.1 says
-- those two counters are interchangeable, so representing them apart is a
-- rules-observable divergence rather than an inconvenience.
--
-- HAND-KEPT, and nothing forces it: a twelfth 'Pawl.Types.CounterKind'
-- constructor compiles fine with no entry here, since answering "not reserved"
-- is a legal answer. The same site genre as #1715's codec arms. Deriving it from
-- an exhaustive case over 'Pawl.Types.CounterKind' is not open, because that
-- module imports this one.
--
-- CR 122.1b's fifteen keyword spellings sit alongside the kind spellings, for
-- the 'Pawl.Types.CounterKind.Keyword' arm. The rule's trailing "as well as any
-- variants of those keywords" is not a closed list and is not fenced here; a
-- card naming a variant reaches the same collision, which is the residue this
-- set does not cover.
reserved :: Set.Set Text.Text
reserved =
  Set.fromList $
    fmap
      Text.pack
      [ "+1/+1",
        "-1/-1",
        "defense",
        "fade",
        "level",
        "lore",
        "loyalty",
        "shield",
        "time",
        -- CR 122.1b's list, in the rule's own order.
        "flying",
        "first strike",
        "double strike",
        "deathtouch",
        "decayed",
        "exalted",
        "haste",
        "hexproof",
        "indestructible",
        "lifelink",
        "menace",
        "reach",
        "shadow",
        "trample",
        "vigilance"
      ]

-- | The only door in. Rejects a 'reserved' spelling, and rejects the empty name,
-- which no card prints and which would read as "the counter with no name".
make :: Text.Text -> Either Text.Text CounterName
make text
  | Text.null text = Left (Text.pack "CounterName: a counter's name must not be empty")
  | Set.member text reserved =
      Left
        ( Text.pack "CounterName: CR 122.1 makes counters with the same name interchangeable, and Pawl.Types.CounterKind already names "
            <> text
        )
  | otherwise = Right (UnsafeMkCounterName text)
