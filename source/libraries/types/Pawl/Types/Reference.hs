module Pawl.Types.Reference where

import Numeric.Natural (Natural)
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Label as Label

-- | How a scenario names a seat or an object without an id, which a setup
-- change would renumber and another engine could not read.
data Reference
  = -- | The seat or object the board labelled so.
    Labelled Label.Label
  | -- | The live object with this card name, 1-based in creation order across
    -- every zone. CR 400.7 makes a moved card a new object, counted afresh.
    Printed CardName.CardName Natural
  deriving (Eq, Ord, Show)
