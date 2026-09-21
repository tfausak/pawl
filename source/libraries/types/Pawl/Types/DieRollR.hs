module Pawl.Types.DieRollR where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DieRollRewrite as DieRollRewrite

-- | The payload of Pawl.Types.ReplacementEffect's DieRollR arm: whose die rolls
-- are intercepted (CR 706.1 / 614.1a), and what happens instead.
--
-- A bare ControllerRelation beside the rewrite rather than a pattern RECORD,
-- Pawl.Types.CoinFlipR's shape and for its reason: the roller is the only seat
-- rule 706.1's instruction concerns, and the rest of the roll -- the die's size
-- and the count -- belongs to the instruction rather than to anything a row here
-- would narrow by. Notably NOT the die's size: every printing of the sentence
-- says "one or more dice" and names no dN, so a row that asked about sides would
-- be this engine's restriction rather than a rule's. The record appears when a
-- card needs one.
data DieRollR = MkDieRollR
  { whose :: ControllerRelation.ControllerRelation,
    rewrite :: DieRollRewrite.DieRollRewrite
  }
  deriving (Eq, Ord, Show)
