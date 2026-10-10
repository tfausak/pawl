module Pawl.Types.Operand where

import qualified Pawl.Types.BoundMeasure as BoundMeasure
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.SlotName as SlotName

-- | What a Pawl.Types.Measures comparison compares the candidate's measure
-- against. Every arm but Literal is answered off Pawl.Engine.Filter.Context, so
-- where each may be written is a position question
-- (Pawl.FilterPositionLintSpec).
data Operand
  = -- | A printed number.
    Literal Integer
  | -- | CR 113.7: the source's measure (CR 702.134a's mentor).
    OfSource Measure.Measure
  | -- | CR 208.1: the candidate's own other measure.
    Own Measure.Measure
  | -- | CR 608.2c: a measure of the one object an earlier clause bound at a slot.
    OfBound BoundMeasure.BoundMeasure
  | -- | CR 608.2c: a number an earlier clause bound at a slot.
    AmountInSlot SlotName.SlotName
  | -- | CR 601.2b: the amount the enclosing target slot or conjure reference
    -- names (Pawl.Types.TargetSlot's and Pawl.Types.FromReference's @amount@).
    EnclosingAmount
  deriving (Eq, Ord, Show)
