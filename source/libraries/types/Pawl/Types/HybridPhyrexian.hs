module Pawl.Types.HybridPhyrexian where

import qualified Pawl.Types.Color as Color

-- | CR 107.4f's hybrid Phyrexian symbol: the two colours whose mana may pay it,
-- the third way -- 2 life -- being the rule's and not a field.

-- BOTH fields are a Color and not a ManaType, the reason
-- Pawl.Types.ManaSymbol's Phyrexian gives one constructor over: CR 107.4f names
-- ten symbols and every one of them is a pair of COLOURS, so a colourless half
-- stays unsayable rather than a case every reader has to rule out.
--
-- Named rather than positional, and ordered, for Pawl.Types.Hybrid's reason:
-- @{G/U/P}@ and @{U/G/P}@ say the same thing -- CR 107.4f names ten symbols, one
-- per unordered pair -- but they are not the same printed text, so the order
-- survives a round trip. The two are
-- ALTERNATIVES, so the order carries no meaning beyond presentation, and
-- @MkHybridPhyrexian c c@ is degenerate rather than illegal -- it simply means
-- @Phyrexian c@, and no card prints one.
data HybridPhyrexian = MkHybridPhyrexian
  { left :: Color.Color,
    right :: Color.Color
  }
  deriving (Eq, Ord, Show)
