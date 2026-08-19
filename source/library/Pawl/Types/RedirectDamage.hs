module Pawl.Types.RedirectDamage where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 614.9's redirection: for a duration, damage of some kind headed for one
-- recipient is dealt to another instead.

-- 'from' and 'to' are both an ObjectRef and are the two ends of the same
-- rewrite, so they are named rather than positional: a card file that swapped
-- them would redirect damage in the wrong direction and still decode.
data RedirectDamage = MkRedirectDamage
  { duration :: Duration.Duration,
    -- | PRINTED, not assumed -- Turn the Tables says "all COMBAT damage".
    -- Nothing is a redirect naming no kind, and is elided rather than written
    -- null.
    kind :: Maybe DamageKind.DamageKind,
    from :: ObjectRef.ObjectRef,
    to :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
