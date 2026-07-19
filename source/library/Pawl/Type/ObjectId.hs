{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ObjectId where

import Language.Haskell.TH.Syntax (Lift)
import Numeric.Natural (Natural)

newtype ObjectId = MkObjectId Natural
  deriving (Eq, Lift, Ord, Show)
