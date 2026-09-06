module Pawl.Types.ForbidActivation where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's ForbidActivation arm (#3278): CR 602.2's
-- prohibition with CR 101.2, standing over the named permanents for this
-- duration.
--
-- Deadlock Trap's is
-- @ForbidActivation UntilEndOfTurn (InSlot target)@.
--
-- Pawl.Types.ForbidBlock's shape exactly, and for its reason: the sentence's
-- subject is an OBJECT and there is only one axis, so an ObjectRef rather than
-- Pawl.Types.ForbidAttack's Pawl.Types.RestrictedCreatures.
--
-- Not a Pawl.Types.ActivationProhibition: that type is printed card text
-- gathered live off a SOURCE on the battlefield, where this outlives its source
-- (CR 611.2a) and names the permanents it covers once, at resolution.
--
-- No CR 605.1a kind, where the printed carrier holds one: both printings of this
-- sentence ("its activated abilities can't be activated this turn", Deadlock
-- Trap; "until your next turn ... its activated abilities can't be activated",
-- Dovin Baan) name every activated ability, so a kind would be a field no card
-- writes.
data ForbidActivation = MkForbidActivation
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
