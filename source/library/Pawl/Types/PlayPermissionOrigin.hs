module Pawl.Types.PlayPermissionOrigin where

-- | Which RULE granted an ExilePlayPermission. CR 715.3d's last clause is the
-- only thing that asks: "it can't be cast as an Adventure this way, although
-- other effects that allow a player to cast it may allow a player to cast it as
-- an Adventure" -- so the Adventure exclusion is scoped to the permission rule
-- 715.3d itself grants, and a permission from anywhere else carries no such
-- exclusion.
--
-- A CLASSIFICATION of permissions and not of effects: Adventure names rule
-- 715.3d, Granted names "some Effect.GrantPlayFromExile said so", and no
-- consumer may ask WHICH effect that was. That keeps Pawl.Engine.Cast reading
-- the rulebook rather than the card pool.
--
-- Not a Bool, for Regenerability's reason: at the call site
-- `origin == Adventure` says which rule is in play, where a `permitsAdventure`
-- flag would record the consequence and lose the fact.
data PlayPermissionOrigin
  = -- | CR 715.3d's own permission, written as the Adventure spell resolves.
    Adventure
  | -- | A permission a card states, via Effect.GrantPlayFromExile (CR 601.3).
    Granted
  deriving (Bounded, Enum, Eq, Ord, Show)
