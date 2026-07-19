{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Supertype where

import Language.Haskell.TH.Syntax (Lift)

-- Grows: Snow, World, …
data Supertype
  = Basic
  | Legendary
  deriving (Eq, Lift, Ord, Show)
