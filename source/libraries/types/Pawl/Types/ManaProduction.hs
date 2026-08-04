module Pawl.Types.ManaProduction where

import qualified Pawl.Types.ManaType as ManaType

-- | CR 106.3: how an AddMana effect decides WHICH mana it puts into the pool.
-- Either one fixed type -- Llanowar Elves' "Add {G}" -- or one mana of a colour
-- the producing player chooses (Birds of Paradise), which CR 105.4 restricts to
-- the five colours, never colourless.
--
-- Data hanging off the one AddMana opcode rather than a second opcode: "add one
-- mana" is a single instruction, and what varies is a payload saying how its type
-- is determined. Nothing in the rules core cases on this -- it asks
-- Mana.producedTypes for the options and prompts among them, which is the only
-- obligation a future constructor ("of any type", CR 607.2's "of the chosen
-- colour") would carry.
data ManaProduction
  = OfType ManaType.ManaType
  | AnyColor
  deriving (Eq, Ord, Show)
