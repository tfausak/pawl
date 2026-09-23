module Pawl.Types.PermissionLimit where

-- | CR 601.3: how often a permission an effect grants may be used. Johann,
-- Apprentice Sorcerer's "Once each turn, you may cast an instant or sorcery
-- spell from the top of your library" is OnceEachTurn; Future Sight's
-- unrestricted sentence is Unlimited.
--
-- Carried by the permission itself (Pawl.Types.CastFromZone.limit), because CR
-- 601.3 supplies no default budget: nothing in the rules counts how often a
-- permission has been used, so an effect that prints a limit is the only thing
-- that can say there is one.
--
-- Also the budget on a CR 706.2 die-roll modifier (Pawl.Types.ModifiedRoll's
-- `limit`, Night Shift of the Living Dead's "Do this only once each turn"),
-- spent in Pawl.Types.GameState.rollModifiersUsedThisTurn.
--
-- The CASTING-side twin of Pawl.Types.ActivationRestriction's OnlyOnceEachTurn,
-- and deliberately a separate type rather than that one reused: CR 602.5b's
-- rider is one clause of a list an activated ability prints about itself, while
-- this is a field of a permission, and the two vocabularies share nothing else.
--
-- A sum type rather than a Bool or a Maybe Natural: no boolean blindness, and a
-- budget of two would want a card to state it -- Scryfall
-- @o:"twice each turn" o:cast@, 2026-09-20, one hit, and it budgets loyalty
-- abilities rather than a cast (Urza, Planeswalker).
--
-- Not implemented: a budget scoped to a subset of turns. Johann's "once each
-- turn" and Serra Paragon's "once during each of your turns" differ in whose
-- turns they admit, and no arm here carries a Pawl.Types.TurnScope (#3589).
data PermissionLimit
  = -- | No printed budget: the permission applies whenever it otherwise would
    -- (Future Sight, Garruk's Horde, Yawgmoth's Will).
    Unlimited
  | -- | "Once each turn": one use per turn, reset at the turn handoff
    -- (Pawl.Engine.Engine.beginTurnOf), which is the whole of "each turn". The
    -- memory is Pawl.Types.GameState.castPermissionsUsedThisTurn, keyed by the
    -- object granting the permission for CR 602.5b's reason its activation-side
    -- twin is: the budget is the ability's and survives a change of control.
    OnceEachTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
