module Pawl.Types.RequireBlock where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's RequireBlock arm (#1305): CR 509.1c's
-- requirement that the blockers named block the attackers named, for this
-- duration.
--
-- BOTH sides are an ObjectRef and the two are NOT interchangeable, so they are
-- named rather than positional: a card file that swapped them would otherwise
-- decode into a requirement pointing the wrong way.
data RequireBlock = MkRequireBlock
  { duration :: Duration.Duration,
    blocker :: ObjectRef.ObjectRef,
    attacker :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
