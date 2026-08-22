module Pawl.Types.CantBeRegenerated where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's CantBeRegenerated arm (#1680): CR
-- 701.19c's prohibition, standing over the named permanents for this duration.
--
-- Hurr Jackal's is `CantBeRegenerated UntilEndOfTurn (InSlot target)`.
--
-- Pawl.Types.RequireBlock's shape minus one ref, and for its reason: the rule's
-- subject is an OBJECT, so a ref rather than an AffectedPlayers scope. Unlike
-- RequireBlock there is only one axis, so the fields cannot be swapped and the
-- naming is ordinary convenience.
--
-- The prohibition is a DURATION, where Pawl.Types.Regenerability is a property
-- of one destruction: CR 611.2 makes "this turn" an ordinary stored continuous
-- effect, which is what the two carriers are for.
data CantBeRegenerated = MkCantBeRegenerated
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
