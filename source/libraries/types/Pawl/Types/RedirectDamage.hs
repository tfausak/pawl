module Pawl.Types.RedirectDamage where

import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
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
    to :: ObjectRef.ObjectRef,
    -- | CR 609.7a's "by a source of your choice" (Oracle's Attendants), as the
    -- PROPERTIES the chosen source must have, exactly as
    -- Pawl.Types.PreventAllDamage carries it on the unbounded shield. Nothing is
    -- a redirection naming no source at all (Turn the Tables), which watches
    -- every source; `Just` makes the effect's controller choose ONE source when
    -- the effect is created, and Pawl.Engine.Resolve bakes that id into
    -- Pawl.Types.DamagePattern.whichSource.
    --
    -- The Filter is BOTH halves of CR 609.7b: it narrows the candidates offered,
    -- and it is written into DamagePattern.whatSource so the recheck happens at
    -- the damage event rather than at the choice. Oracle's Attendants says only
    -- "a source", so its Filter is the trivial `And []` -- present, not absent,
    -- because "any source of your choice" is still a choice.
    chosenSource :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
