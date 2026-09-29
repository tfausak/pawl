module Pawl.Types.ExchangeBlocks where

import qualified Pawl.Types.SlotName as SlotName

-- | The two blocking creatures Effect.ExchangeBlocks trades blocks between
-- (Sorrow's Path), one target slot each. Two slots rather than one of count
-- two, since the second's "controlled by the same opponent" is
-- Filter.SameControllerAsBound over the first (CR 110.2).
data ExchangeBlocks = MkExchangeBlocks
  { first :: SlotName.SlotName,
    second :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
