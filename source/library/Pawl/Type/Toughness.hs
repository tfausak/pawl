{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Toughness where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.Quantity (Quantity)

newtype Toughness = MkToughness Quantity
  deriving (Eq, Lift, Ord, Show)
