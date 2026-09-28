module Pawl.Types.ScryR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.ScryRewrite as ScryRewrite

-- | The payload of Pawl.Types.ReplacementEffect's ScryR arm: whose scries are
-- intercepted (CR 701.22a / 614.1a), and what happens instead.
--
-- A bare ControllerRelation beside the rewrite, Pawl.Types.ProliferateR's shape
-- and for its reason: the instruction concerns the scrying player and nothing
-- else a row here would narrow by.
data ScryR = MkScryR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: ScryRewrite.ScryRewrite
  }
  deriving (Eq, Ord, Show)
