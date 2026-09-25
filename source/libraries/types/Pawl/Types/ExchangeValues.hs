module Pawl.Types.ExchangeValues where

import qualified Pawl.Types.ExchangedValue as ExchangedValue

-- | The payload of Pawl.Types.Effect's ExchangeValues arm: the two numerical
-- values CR 701.12g exchanges, each becoming the other's previous value.
data ExchangeValues = MkExchangeValues
  { one :: ExchangedValue.ExchangedValue,
    other :: ExchangedValue.ExchangedValue
  }
  deriving (Eq, Ord, Show)
