module Pawl.Types.ProliferateR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.ProliferateRewrite as ProliferateRewrite

-- | The payload of Pawl.Types.ReplacementEffect's ProliferateR arm: whose
-- proliferates are intercepted (CR 701.34a / 614.1a), and what happens instead.
--
-- A bare ControllerRelation beside the rewrite, Pawl.Types.CoinFlipR's shape and
-- for its reason: rule 701.34a's instruction concerns the proliferating player
-- and has no source, amount or destination to narrow by.
data ProliferateR = MkProliferateR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: ProliferateRewrite.ProliferateRewrite
  }
  deriving (Eq, Ord, Show)
