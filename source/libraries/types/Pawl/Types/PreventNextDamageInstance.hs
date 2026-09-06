module Pawl.Types.PreventNextDamageInstance where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 615.8's prevention shield: over whom, from which chosen source, and for
-- how long. One INSTANCE of damage goes, whatever its size, and the shield is
-- then used up.

-- Not parametric in the effect, where Pawl.Types.PreventNextDamage and
-- Pawl.Types.PreventAllDamage both are: those two carry CR 615.5's rider and so
-- hold effects, and no printing of this rule's shape carries one -- Deflecting
-- Palm, Honorable Passage and New Way Forward all say "if damage is prevented
-- this way" instead, which is CR 615.13's triggered ability and lives in the
-- card's delayed abilities. A printing that wrote "for each 1 damage prevented
-- this way" would want the field, and would want the parameter with it.
data PreventNextDamageInstance = MkPreventNextDamageInstance
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
    chosenSource :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
