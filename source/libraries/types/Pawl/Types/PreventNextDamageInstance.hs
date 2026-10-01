module Pawl.Types.PreventNextDamageInstance where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 615.8's prevention shield: over whom, from which chosen source, for how
-- long, and CR 615.5's additional effect riding it. One INSTANCE of damage goes,
-- whatever its size, and the shield is then used up.

-- Parametric in the effect for Pawl.Types.PreventNextDamage's reason.
data PreventNextDamageInstance effect = MkPreventNextDamageInstance
  { duration :: Duration.Duration,
    -- | The recipients this RESOLUTION names -- Deflecting Palm's "to you",
    -- Honorable Passage's "to any target" -- one CR 615.8 shield each.
    --
    -- Required, where the two shields beside this one make it optional: CR 615.8
    -- describes an effect that watches ONE source, and every printing of it names
    -- the protected recipient outright. A card describing its recipients by
    -- characteristic instead would want Pawl.Types.PreventNextDamage's
    -- @whatRecipient@ and @whoRecipient@ pair here too.
    ref :: ObjectRef.ObjectRef,
    -- | CR 609.7a's "a source of your choice", as the PROPERTIES the chosen
    -- source must have. Required for the reason @ref@ is: CR 615.8's shield is
    -- about "the next time a SPECIFIC source would deal damage", so there is no
    -- shape of this rule that watches every source. Deflecting Palm says only "a
    -- source", so its Filter is the trivial `And []` -- "any source of your
    -- choice" is still a choice.
    --
    -- BOTH halves of CR 615.9, exactly as the sibling shields carry it: it
    -- narrows the candidates offered, and it is written into
    -- Pawl.Types.DamagePattern.whatSource so CR 609.7b's recheck happens at the
    -- damage event rather than at the choice.
    chosenSource :: Filter.Filter Keyword.Keyword,
    -- | CR 615.5's additional effect -- Reverse Damage's "you gain life equal to
    -- the damage prevented this way". Empty for a shield with no such clause,
    -- so the key is elided rather than written as an empty array.
    riders :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
