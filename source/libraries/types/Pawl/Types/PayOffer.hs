module Pawl.Types.PayOffer where

import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Recipient as Recipient

-- | Which CR 118.12 offer a resolving object is making, so the payer can tell
-- one offer from another.
data PayOffer
  = -- | A clause's own gate, by CR 608.2e's mode and clause ordinals.
    AtClause ModeIndex.ModeIndex ClauseIndex.ClauseIndex
  | -- | A CR 608.2f loop's gate, offered once per member (Cleansing's "for each
    -- land ... unless any player pays 1 life").
    ForMember Recipient.Recipient
  deriving (Eq, Ord, Show)
