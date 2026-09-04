module Pawl.Types.LifeGainR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite

-- | The payload of Pawl.Types.ReplacementEffect's LifeGainR arm: whose life
-- gains are intercepted (CR 119.10 / 614.1a), and what happens instead.
--
-- A bare ControllerRelation rather than a pattern RECORD, Pawl.Types.DrawR's
-- shape rather than Pawl.Types.LifeLossPattern's: the loss side needs a cause
-- field because Worship's clause says "damage that would reduce your life
-- total", and no printed gain clause narrows that way -- Boon Reflection, Rhox
-- Faithmender and Alhammarret's Archive all say "if you would gain life" flat.
-- The record appears when a card needs one.
data LifeGainR = MkLifeGainR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: LifeGainRewrite.LifeGainRewrite
  }
  deriving (Eq, Ord, Show)
