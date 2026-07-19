{-# LANGUAGE DeriveLift #-}

module Pawl.Type.Printing where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.Card (Card)

newtype Printing = MkPrinting
  { card :: Card
  }
  deriving (Eq, Lift, Ord, Show)
