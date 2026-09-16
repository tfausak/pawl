module Pawl.Types.CoinFlipR where

import qualified Pawl.Types.CoinFlipRewrite as CoinFlipRewrite
import qualified Pawl.Types.ControllerRelation as ControllerRelation

-- | The payload of Pawl.Types.ReplacementEffect's CoinFlipR arm: whose coin
-- flips are intercepted (CR 705.1 / 614.1a), and what happens instead.
--
-- A bare ControllerRelation beside the rewrite rather than a pattern RECORD,
-- Pawl.Types.LifeGainR's shape: CR 705.2's last sentence makes the flipper the
-- only seat a flip concerns, and nothing else about rule 705.1's flip -- it has
-- no source, no amount and no destination -- is there to narrow by. The record
-- appears when a card needs one.
data CoinFlipR = MkCoinFlipR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: CoinFlipRewrite.CoinFlipRewrite
  }
  deriving (Eq, Ord, Show)
