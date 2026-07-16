module Pawl where

import qualified Data.List as List

newtype T a b = C (List.List b) deriving (Show)
