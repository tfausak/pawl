module Pawl.Types.Counterability where

-- | Whether a spell can be countered (CR 701.6a: "to counter a spell or ability
-- means to cancel it, removing it from the stack").
--
-- CR 113.6g is what makes this a property of the OBJECT and not of the
-- countering effect: "An object's ability that states it can't be countered or
-- can't be copied functions on the stack." So Rending Volley carries this, and
-- Cancel does not -- the opposite arrangement from Pawl.Types.Regenerability,
-- which rides the Destroy effect because CR 701.19c's "it can't be regenerated"
-- is printed on the destroying spell.
--
-- CR 101.2 is why a gate is the right shape rather than a negotiation: "When a
-- rule or effect allows or directs something to happen, and another effect
-- states that it can't happen, the 'can't' effect takes precedence." Cancel
-- still resolves, and still legally targeted the spell -- CR 113.6g gives no
-- targeting immunity, so this is not shroud -- it simply fails to counter it.
--
-- Not a Bool, for the reason Regenerability, TapState and Sickness are not
-- Bools: `Counterability.CantBeCountered` names the rule at the site that reads
-- it, where a bare True would name nothing.
--
-- Modelled on the CARD rather than as a StaticAbility. Under the rules it IS a
-- static ability -- CR 604.2's continuous effects last "as long as the object
-- with the ability remains in the appropriate zone, as described in rule 113.6",
-- which is exactly CR 113.6g's stack. But Pawl.Types.StaticAbility implements only
-- the battlefield-scoped part of that rule: an Affected set plus Modifications,
-- gathered from the BATTLEFIELD by the projection and folded through the CR 613
-- layers. A prohibition that functions on the stack has no layer, no affected set
-- and modifies no characteristic, so it fits none of that machinery. Same reason
-- Card.castingPermissions is a card field: CR 113.6 abilities that function
-- outside the battlefield are where the projection stops.
data Counterability
  = Counterable
  | CantBeCountered
  deriving (Eq, Ord, Show)
