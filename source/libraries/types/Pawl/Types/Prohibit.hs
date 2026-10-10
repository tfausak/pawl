module Pawl.Types.Prohibit where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Prohibition as Prohibition

-- | The payload of Pawl.Types.Effect's Prohibit arm: CR 101.2's "can't",
-- standing over the permanents 'ref' names for 'duration' (CR 611.2a).
--
-- Hurr Jackal's is @Prohibit Regenerate UntilEndOfTurn (InSlot target)@;
-- Deadlock Trap, Zirda, the Dawnwaker and Wall of Stolen Identity write the
-- other three.
--
-- An ObjectRef: each sentence's subject is an OBJECT and there is one axis.
-- Pawl.Types.ForbidAttack is not this shape -- CR 611.2c makes "creatures can't
-- attack you" a class, and it carries what the attack is aimed at.
--
-- Not a printed carrier (Pawl.Types.ActivationProhibition,
-- Pawl.Types.CombatRestriction, Pawl.Types.UntapRestriction): those are
-- gathered live off a SOURCE on the battlefield, where this outlives its source
-- (CR 611.2a) and names the permanents it covers once, at resolution.
--
-- No CR 605.1a kind for Activate: both printings of that sentence ("its
-- activated abilities can't be activated this turn", Deadlock Trap; Dovin
-- Baan's "until your next turn") name every activated ability. No CR 509.1b
-- "unless" gate for Block: no printing of this sentence states one.
data Prohibit = MkProhibit
  { what :: Prohibition.Prohibition,
    duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
