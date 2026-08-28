module Pawl.Types.LifeLossR where

import qualified Pawl.Types.LifeLossPattern as LifeLossPattern
import qualified Pawl.Types.LifeLossRewrite as LifeLossRewrite

-- | The payload of Pawl.Types.ReplacementEffect's LifeLossR arm: which life
-- losses are intercepted, and how much of the loss survives.
data LifeLossR = MkLifeLossR
  { matching :: LifeLossPattern.LifeLossPattern,
    rewrite :: LifeLossRewrite.LifeLossRewrite
  }
  deriving (Eq, Ord, Show)
