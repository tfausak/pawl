module Pawl.Types.ChooseBetween where

import qualified Numeric.Natural as Natural

-- | CR 700.2's range: an instruction naming how FEW and how MANY modes may be
-- chosen, both inclusive. "Choose one or both --" over two modes (Vandalize) is
-- 1 to 2.

-- Both fields are a Natural and the pair is ordered, so they are named rather
-- than positional: a card file that swapped them would state a range no
-- selection can satisfy, and the two would still decode.
--
-- @least <= most@ is an invariant nothing here maintains; Pawl.Codec.ChooseBetween
-- rejects a range that breaks it, this being where a range enters the engine.
data ChooseBetween = MkChooseBetween
  { least :: Natural.Natural,
    most :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
