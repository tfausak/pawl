{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Zone where

import Language.Haskell.TH.Syntax (Lift)

data Zone
  = Library
  | Hand
  | Graveyard
  | Battlefield
  | Stack
  | Exile
  deriving (Eq, Lift, Ord, Show)
