module Pawl.Types.Earthbend where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's Earthbend arm: CR 701.66a's "earthbend
-- N", plus the reference naming the land the rule words as "target land you
-- control".
--
-- The ObjectRef is Effect's Detain and Goad arms' field, not a bare slot, so the
-- card declares the target slot and its CR 115.1 pool the way every other
-- targeted opcode's card does; rule 701.66a names one target, and nothing here
-- narrows the reference further.
--
-- N is a Quantity rather than a Natural, Pawl.Types.Amass's reason: every
-- printing in the pool writes a literal, but the position is an expression over
-- game state everywhere else a keyword action counts.
data Earthbend = MkEarthbend
  { quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
