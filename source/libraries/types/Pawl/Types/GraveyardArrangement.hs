module Pawl.Types.GraveyardArrangement where

import qualified Numeric.Natural as Natural

-- | CR 404.3: an owner's answer to Prompt.ArrangeGraveyardArrivals.
data GraveyardArrangement
  = -- | Any order will do: the batch keeps the order it moved in.
    AnyOrder
  | -- | A permutation of the offered indices, reading from the top down.
    InOrder [Natural.Natural]
  deriving (Eq, Ord, Show)
