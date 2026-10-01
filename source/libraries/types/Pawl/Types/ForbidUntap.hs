module Pawl.Types.ForbidUntap where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ForbidUntap arm: CR 502.3's untap with
-- CR 101.2's "can't", standing over the named permanents for this duration.
--
-- Wall of Stolen Identity's linked trigger is
-- @ForbidUntap (ForAsLongAs ...) (InSlot thatCopiedObject)@.
--
-- Pawl.Types.ForbidActivation's shape exactly, and for its reason: the
-- sentence's subject is an OBJECT and there is only one axis.
--
-- Not a Pawl.Types.UntapRestriction: that type is printed card text gathered
-- live off a SOURCE on the battlefield, where this outlives its source (CR
-- 611.2a) and names the permanents it covers once, at resolution. Nor
-- Pawl.Types.DoesNotUntapNext, which counts untap steps rather than lasting for
-- a stated duration.
data ForbidUntap = MkForbidUntap
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
