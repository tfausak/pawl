{-# LANGUAGE DeriveLift #-}

module Pawl.Type.TypeLine where

import Data.Set (Set)
import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.CardType (CardType)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.Supertype (Supertype)

data TypeLine = MkTypeLine
  { supertypes :: Set Supertype,
    types :: Set CardType,
    subtypes :: Set Subtype
  }
  deriving (Eq, Lift, Ord, Show)
