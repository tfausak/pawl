module Pawl.Type.Card where

import Data.Text (Text)
import Pawl.Type.TypeLine (TypeLine)

data Card = MkCard
  { name :: Text,
    typeLine :: TypeLine
  }
  deriving (Eq, Ord, Show)
