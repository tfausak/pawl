module Pawl.Types.WhichCounters where

import qualified Pawl.Types.CounterKind as CounterKind

-- | Which counters an instruction to remove them reaches (CR 122.1): one kind,
-- or any kind, where each counter taken is the payer's choice of kind as well
-- as of permanent.
--
-- PARAMETRIC in the keyword for the CounterKind it carries, exactly as
-- Pawl.Types.CounterKind is.
data WhichCounters keyword
  = -- | Novijen Sages' "+1\/+1 counters", The Filigree Sylex's "oil counters".
    OfKind (CounterKind.CounterKind keyword)
  | -- | Tayam, Luminous Enigma's bare "counters".
    OfAnyKind
  deriving (Eq, Ord, Show)
