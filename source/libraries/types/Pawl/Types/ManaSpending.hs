module Pawl.Types.ManaSpending where

-- | CR 118.14: how one player may spend mana toward ONE cost -- ordinarily as
-- the mana is, and under an effect that says so, as though it were mana of any
-- type.
--
-- WHAT IT IS NOT is the whole of CR 609.4b: "this affects only how the player
-- may pay a cost. It doesn't change that cost, and it doesn't change what mana
-- was actually spent to pay that cost." So this never reaches a ManaCost, and it
-- never reaches a Pawl.Types.ManaUnit either -- Pawl.Engine.Mana applies it to
-- the DEMANDS a cost resolves into, leaving the cost and the pool alone. A model
-- that rewrote either would answer CR 202.3's mana value, CR 500.5's leftover
-- pool or a "spend only black mana" restriction wrongly.
--
-- A TYPE and not a Bool, because the axis has a second point the rules already
-- write: "spend mana as though it were mana of any color" (CR 609.4b's own
-- wording) permits five types where AnyType permits CR 106.1b's six. No card in
-- this pool prints it, so it is a constructor this type does not have yet rather
-- than a field.
--
-- Not a per-PLAYER setting: rule 118.14's last sentence scopes the permission to
-- the spells cast under the effect that granted it ("this applies only to mana
-- that player spends to cast spells that way"), which is why it rides
-- Pawl.Types.ExilePlayPermission -- one card, one player -- rather than sitting
-- on the player.
data ManaSpending
  = -- | The default, and every cost in the game that no effect has spoken about:
    -- a demand for red mana is served by red mana.
    AsProduced
  | -- | CR 118.14's "mana of any type can be spent to pay that cost" -- the
    -- player "may spend mana as though it were colorless mana or mana of any
    -- color", which is CR 106.1b's six types.
    --
    -- TYPES only. CR 107.4h's {S} demands mana produced by a snow source, which
    -- is a fact about where the mana came from rather than about what it is, and
    -- rule 118.14 says nothing about provenance -- so a snow demand still wants a
    -- snow supply under this permission (Pawl.Engine.Mana.relax).
    AnyType
  deriving (Bounded, Enum, Eq, Ord, Show)
