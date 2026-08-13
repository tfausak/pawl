module Pawl.Types.Compares where

import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Quantity as Quantity

-- | One comparison: the measured Quantity relates thus to the threshold. The
-- payload of Pawl.Types.Condition's Compares arm, given a record of its own so
-- that the arm carries a named type rather than three positional fields
-- (#1305) -- docs/style-guide.md's @data T = C1 T1@ shape.
--
-- BOTH SIDES are a full Quantity, and both are evaluated symmetrically --
-- Pawl.Engine.Condition.holds passes each through Pawl.Engine.Quantity.evaluate
-- against the same object and view, so a Quantity.Power on either side reads the
-- same object. Only the Comparison is oriented, AtLeast meaning the measured side
-- is at least the threshold. That width is what lets a condition read the object
-- it is evaluated against rather than a set swept out of a zone, which CR 603.4's
-- intervening "if" on a leaves-the-battlefield ability needs: the source is gone,
-- so CR 608.2h answers through Projection.viewWithLastKnownAnywhere -- which owes
-- the same fallback to an object the clause names through a SLOT, rule 702.100a's
-- entrant being one.
--
-- The field names are the wire format's keys, which is what keeps a card file
-- that swapped the two Quantities a decode failure rather than a silently
-- different condition.
data Compares = MkCompares
  { measured :: Quantity.Quantity,
    comparison :: Comparison.Comparison,
    threshold :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
