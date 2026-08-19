module Pawl.Types.Zone where

data Zone
  = Library
  | Hand
  | Graveyard
  | Battlefield
  | Stack
  | Exile
  | -- | CR 400.1 / 408.1: the command zone -- a game area for objects with an
    -- overarching effect that are not permanents and cannot be destroyed. Shared
    -- across players (not per-player), like Battlefield and Exile. Emblems (CR
    -- 114) are its first resident.
    Command
  deriving (Bounded, Enum, Eq, Ord, Show)
