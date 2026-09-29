module Pawl.Types.TappedIs where

import qualified Pawl.Types.Reference as Reference
import qualified Pawl.Types.TapState as TapState

-- | CR 110.5: whether a permanent is tapped.
data TappedIs = MkTappedIs
  { object :: Reference.Reference,
    tapped :: TapState.TapState
  }
  deriving (Eq, Ord, Show)
