{-# LANGUAGE DeriveLift #-}

module Pawl.Type.ManaCost where

import Language.Haskell.TH.Syntax (Lift)
import Pawl.Type.ManaSymbol (ManaSymbol)

-- A list, never fixed arity: fixed arity is the recurring root cause behind the
-- shapes that later need rewriting (see the design doc, section 2.11).
newtype ManaCost = MkManaCost [ManaSymbol]
  deriving (Eq, Lift, Ord, Show)
