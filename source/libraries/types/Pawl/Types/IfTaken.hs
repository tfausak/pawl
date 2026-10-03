module Pawl.Types.IfTaken where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.ClauseIndex as ClauseIndex

-- | CR 608.2c: which earlier clauses of its mode a clause hangs off, and which
-- way -- see Pawl.Types.Clause.ifTaken. Keyed on whether each named clause's
-- instructions RAN (Pawl.Engine.Resolve.recordTaken).
data IfTaken
  = -- | "If you do": any of them ran -- Tweeze, Worms of the Earth (CR 608.2c).
    AnyTaken (NonEmpty.NonEmpty ClauseIndex.ClauseIndex)
  | -- | "If no one does": none of them ran -- Browbeat, Development (CR 608.2c, 118.12a).
    NoneTaken (NonEmpty.NonEmpty ClauseIndex.ClauseIndex)
  deriving (Eq, Ord, Show)
