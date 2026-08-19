module Pawl.Types.PutCounters where

import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's PutCounters arm (#1305).
--
-- An ObjectRef rather than a bare slot, so that Renegade Krasis' "each other
-- creature you control with a +1/+1 counter on it" can be written: CR 115.10a
-- makes such a set a description and never a target, which is exactly the
-- distinction that type draws.
--
-- Each named permanent gets its OWN call to Event.putCounters, because CR 614.16
-- replaces one placement at a time: a Hardened Scales seeing three creatures gets
-- three opportunities, not one.
data PutCounters = MkPutCounters
  { kind :: CounterKind.CounterKind Keyword.Keyword,
    quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
