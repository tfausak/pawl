module Pawl.Type.Zone where

data Zone
  = Library
  | Hand
  | Graveyard
  | Battlefield
  | Stack
  | Exile
  deriving (Eq, Ord, Show)
