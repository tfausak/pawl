module Pawl.Types.ExchangeOwnership where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 701.12a / 108.3: the one object each ref names trade owners -- Tempest
-- Efreet's "Exchange ownership of the revealed card and Tempest Efreet".
data ExchangeOwnership = MkExchangeOwnership
  { -- | One side; a ref naming no object or several exchanges nothing.
    one :: ObjectRef.ObjectRef,
    -- | The other side.
    other :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
