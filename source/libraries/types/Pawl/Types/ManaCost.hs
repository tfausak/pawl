module Pawl.Types.ManaCost where

import qualified Pawl.Types.ManaSymbol as ManaSymbol

-- | A list, never fixed arity: fixed arity is the recurring root cause behind the
-- shapes that later need rewriting (see the design doc, section 2.11).
newtype ManaCost = MkManaCost
  { unwrap :: [ManaSymbol.ManaSymbol]
  }
  deriving (Eq, Ord, Show)
