module Pawl.Type.Printing where

import Pawl.Type.Card (Card)

newtype Printing = MkPrinting
  { card :: Card
  }
  deriving (Eq, Ord, Show)
