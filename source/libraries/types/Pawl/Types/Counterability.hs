module Pawl.Types.Counterability where

-- | Whether a spell can be countered (CR 701.6a).
--
-- CR 113.6g makes this a property of the OBJECT and not of the countering effect,
-- so Rending Volley carries it and Cancel does not -- the opposite arrangement
-- from Pawl.Types.Regenerability, which rides the Destroy effect because
-- CR 701.19c's "it can't be regenerated" is printed on the destroying spell.
--
-- CR 101.2 is why a gate is the right shape rather than a negotiation: Cancel
-- still resolves and still legally targeted the spell (CR 113.6g grants no
-- targeting immunity, so this is not shroud); it simply fails to counter it.
--
-- Not a Bool, for the reason Regenerability, TapState and Sickness are not:
-- CantBeCountered names the rule at the site that reads it.
--
-- Modelled on the CARD rather than as a StaticAbility. Under the rules it IS a
-- static ability (CR 604.1), and CR 604.2 keeps its continuous effect active as
-- long as the object stays in the zone CR 113.6 names -- CR 113.6g's stack.
-- But Pawl.Types.StaticAbility implements only the battlefield-scoped part of
-- CR 113.6 -- an Affected set plus Modifications folded through the CR 613
-- layers. A prohibition functioning on the stack has no layer, no affected set
-- and modifies no characteristic. Same reason Card.castingPermissions is a card
-- field.
data Counterability
  = Counterable
  | CantBeCountered
  deriving (Eq, Ord, Show)
