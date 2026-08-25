module Pawl.Types.CounterName where

import qualified Data.Text as Text

-- | The NAME a card prints for a counter kind no rule in the CR reads --
-- Zhao, the Moon Slayer's conqueror counter, Gemstone Caverns' luck counter.
-- CR 122.1's last sentence is the whole citation: "Counters with the same name
-- or description are interchangeable", so the name IS the kind's identity and
-- exact equality on this text is exact equality of kind.
--
-- AbilityName's and SlotName's shape, with one difference:
-- 'UnsafeMkCounterName' rather than @MkCounterName@, because the bare
-- constructor sidesteps an invariant. The invariant is CR 122.1's
-- interchangeability sentence, and the door that maintains it is
-- Pawl.Codec.CounterName.make -- it lives there rather than here because it
-- must consult every Pawl.Types.CounterKind constructor's spelling, and that
-- module imports this one. Ord is load-bearing for the same reason
-- 'Pawl.Types.CounterKind.CounterKind''s is: this ends up inside a Map key.
--
-- NOT normalized -- no case folding, no slugging. CR 122.1 says nothing about
-- either, so a policy the rules do not state would be pawl inventing one;
-- equality is exact 'Text.Text' equality.
newtype CounterName = UnsafeMkCounterName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
