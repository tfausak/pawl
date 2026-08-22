module Pawl.Types.DamageDirection where

-- | Which SIDE of a damage event an unbounded prevention shield (CR 615.1 /
-- 615.3) watches: the permanent or player the damage would be dealt TO, or the
-- object that would deal it. Dovin, Hand of Control's "-1: prevent all damage
-- that would be dealt to and dealt BY target permanent an opponent controls"
-- prints both of one another, so its clause carries one
-- Pawl.Types.PreventAllDamage per direction over the same slot -- the shape
-- pawl's shields already have, one row per relation, rather than a third
-- constructor meaning "both".
--
-- Cashed by Pawl.Engine.Resolve.installDamageRow into
-- Pawl.Types.DamagePattern's two baked halves: 'DealtTo' writes the recipient
-- into @whichRecipient@, 'DealtBy' writes the object into @whichSource@ and
-- names no recipient, so the by-direction covers a player as much as a
-- permanent.
--
-- Not a Bool, for Pawl.Types.CostScale's reason: the constructor names the
-- direction where @True@ would only say that something is different.
--
-- Nothing here is CR 609.7a's CHOSEN source (Pawl.Types.PreventAllDamage's
-- @chosenSource@, beside this field): a TARGETED source is declared on the stack
-- (CR 601.2c), so there is no property for CR 615.9 to recheck at the damage
-- event.
data DamageDirection
  = DealtTo
  | DealtBy
  deriving (Bounded, Enum, Eq, Ord, Show)
