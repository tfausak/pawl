module Pawl.Types.PermissionPool where

-- | CR 601.3 / 607.2a: which cards of the zone a CR 601.3 permission opens it
-- reaches, before its Filter narrows them.
data PermissionPool
  = -- | Every card in the zone (Yawgmoth's Will, Garruk's Horde).
    EveryCard
  | -- | CR 607.2a: only the cards exiled with the permission's source (Dawnhand
    -- Dissident's "cards you own exiled with this creature").
    CardsExiledWithSource
  deriving (Bounded, Enum, Eq, Ord, Show)
