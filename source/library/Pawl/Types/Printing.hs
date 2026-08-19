module Pawl.Types.Printing where

import qualified Pawl.Types.Card as Card

newtype Printing = MkPrinting
  { card :: Card.Card
  }
  deriving (Eq, Ord, Show)
