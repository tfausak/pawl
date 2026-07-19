{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Color where

import Language.Haskell.TH.Syntax (Lift)

-- CR 105.1: there are five colors. Closed and finite -- this enumeration is
-- complete and will never grow.
data Color
  = White
  | Blue
  | Black
  | Red
  | Green
  deriving (Eq, Lift, Ord, Show)
