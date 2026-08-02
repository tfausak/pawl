module Pawl.Types.ManaProduction where

import qualified Pawl.Types.ManaType as ManaType

-- | CR 106.3: how an AddMana effect decides WHICH mana it puts into the pool.
-- Either one fixed type -- Llanowar Elves' "Add {G}" -- or one mana of a colour
-- the producing player chooses (Birds of Paradise), which CR 105.4 restricts to
-- the five colours, never colourless.
--
-- Data hanging off the one AddMana opcode rather than a second opcode, for the
-- reason Effect.Replace gives for covering both a Fog and a regeneration shield:
-- "add one mana" is a single instruction, and what varies is a payload saying
-- how its type is determined. Nothing in the rules core cases on this -- it asks
-- Mana.producedTypes for the options and prompts among them.
--
-- Grows: "of any type" (colourless included, so a sibling rather than a tweak to
-- AnyColor), "of the chosen colour" (CR 607.2's linked abilities -- Paradise
-- Plume), and a restricted any-colour ("any colour among permanents you
-- control"). Each is a new constructor whose only obligation is to answer
-- producedTypes.
data ManaProduction
  = OfType ManaType.ManaType
  | AnyColor
  deriving (Eq, Ord, Show)
