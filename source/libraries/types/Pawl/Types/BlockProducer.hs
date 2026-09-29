module Pawl.Types.BlockProducer where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectId as ObjectId

-- | Which of CR 509's three roads made a creature a blocking creature: CR
-- 509.3a and CR 509.3b tell all three apart, and CR 509.3e's attacker-side
-- forms need the third told from the first.
data BlockProducer
  = -- | CR 509.1a: declared as a blocker.
    Declared
  | -- | CR 509.4: put onto the battlefield blocking.
    PutOntoBattlefield
  | -- | CR 509.3b: an effect made a creature already on the battlefield block,
    -- with the attacker's blockers just after it (CR 509.3e's other comparand).
    ByEffect (Set.Set ObjectId.ObjectId)
  deriving (Eq, Ord, Show)
