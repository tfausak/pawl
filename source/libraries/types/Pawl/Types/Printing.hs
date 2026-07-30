module Pawl.Types.Printing where

import Pawl.Types.Card (Card)

newtype Printing = MkPrinting
  { card :: Card
  }
  deriving (Eq, Ord, Show)
