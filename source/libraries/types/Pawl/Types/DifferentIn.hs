module Pawl.Types.DifferentIn where

-- | A characteristic no two cards a search finds may share (CR 701.23a's
-- "matches the given description", said of the found group as a whole).
data DifferentIn
  = -- | Gifts Ungiven's "with different names" (CR 201.2b).
    Names
  | -- | Threats Undetected's "with different powers" (CR 208.1).
    Powers
  | -- | The Karst, Enchanted's "don't share a mana value" (CR 202.3).
    ManaValues
  | -- | The Karst, Enchanted's "don't share ... toughness" (CR 208.1).
    Toughnesses
  | -- | The Karst, Enchanted's "don't share ... card type" (CR 205.2a).
    CardTypes
  deriving (Bounded, Enum, Eq, Ord, Show)
