module Pawl.Types.DrawR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DrawRewrite as DrawRewrite

-- | The payload of Pawl.Types.ReplacementEffect's DrawR arm: whose card draws are
-- intercepted (CR 121.6 / 614.11), and what happens instead.
--
-- A bare ControllerRelation rather than a pattern RECORD, unlike
-- Pawl.Types.LifeLossPattern: rule 121.1's draw carries no cause, no source and
-- no amount for a second field to narrow by, CR 121.2 having broken every
-- instruction into individual draws before a replacement sees one. The record
-- appears when a card needs one.
data DrawR = MkDrawR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: DrawRewrite.DrawRewrite
  }
  deriving (Eq, Ord, Show)
