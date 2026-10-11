module Pawl.Types.Measures where

import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.Operand as Operand

-- | The payload of Pawl.Types.Filter's Measures atom: the candidate's measure
-- relates thus to the operand.
data Measures = MkMeasures
  { measure :: Measure.Measure,
    comparison :: Comparison.Comparison,
    operand :: Operand.Operand
  }
  deriving (Eq, Ord, Show)
