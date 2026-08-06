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
-- and modifies no characteristic. Same reason Face.castingPermissions is a card
-- field.
--
-- SELF-referential, which is the whole of what separates this from
-- Pawl.Types.PlayerEffect.CantBeCountered. CR 113.6g is about "an object's
-- ability that states IT can't be countered", so the ability and the object it
-- protects are the same object and the card is where it can live. Spider-Punk's
-- "spells and abilities can't be countered" is an ability of a BATTLEFIELD
-- PERMANENT about OTHER objects, which CR 113.6 leaves functioning from the
-- battlefield in the ordinary way and CR 611.1's third clause makes a
-- rules-modifying continuous effect. Neither carrier could hold the other's
-- card: Pawl.Engine.PlayerEffect.applying walks the battlefield, where a spell
-- on the stack is not, and this field is read off a card, which an ability on
-- the stack has none of. Pawl.Engine.Event.counter asks both.
--
-- Prowling Serpopard prints one sentence of each and so declares both, which is
-- the clearest demonstration that they are two carriers rather than one written
-- two ways: "This spell can't be countered" is this field, and "Creature spells
-- you control can't be countered" is the player-axis constructor. The proof is
-- in Pawl.PlayerEffectSpec's ProwlingSerpopard group, where a Serpopard SPELL
-- survives a Cancel with no Serpopard on the battlefield at all.
data Counterability
  = Counterable
  | CantBeCountered
  deriving (Eq, Ord, Show)
