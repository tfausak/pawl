module Pawl.Type.ManaSymbol where

import Numeric.Natural (Natural)
import Pawl.Type.ManaType (ManaType)

-- CR 107.4. Grows: Phyrexian, snow, monocolored hybrid ({2/B}).
data ManaSymbol
  = Generic Natural
  | OfType ManaType
  | -- CR 107.4e: "Each one represents a cost that can be paid in one of two ways,
    -- as represented by the two halves of the symbol. A hybrid symbol such as
    -- {W/U} can be paid with either white or blue mana."
    --
    -- COLOUR/COLOUR only. CR 107.4e's other half -- the monocolored hybrid
    -- {2/B}, payable with one black or TWO mana of any type -- is deliberately
    -- not this constructor: every other symbol here is satisfied by exactly one
    -- mana, and {2/B} is the one that is not, so folding it in would break the
    -- one-supply-per-demand shape Pawl.Mana's payment rests on (#106). Phyrexian
    -- (CR 107.4f) is a second exception for a different reason: it can be paid
    -- with life rather than mana at all.
    --
    -- A pair rather than a Set, so the printed order survives a round trip. The
    -- two are ALTERNATIVES, so order carries no meaning beyond presentation, and
    -- `Hybrid t t` is degenerate rather than illegal -- it simply means `OfType
    -- t`, and no card prints one.
    Hybrid ManaType ManaType
  | -- CR 107.3 / 601.2b: the {X} symbol in a cost. Contributes the chosen value
    -- of X once chosen (0 before, for the CR 601.2b castability floor and for a
    -- mana value off the stack, CR 202.3b).
    Variable
  deriving (Eq, Ord, Show)
