{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Power where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.Quantity (Quantity)

newtype Power = MkPower Quantity
  deriving (Eq, Lift, Ord, Show)
