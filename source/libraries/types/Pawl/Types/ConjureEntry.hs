module Pawl.Types.ConjureEntry where

import qualified Pawl.Types.TapState as TapState

-- | What a conjure onto the battlefield says about the card AS IT ENTERS --
-- Foundry Groundbreaker\'s "onto the battlefield tapped" and Kari Zev, Crew of
-- Two\'s "onto the battlefield tapped and attacking".
--
-- The two riders of 'Pawl.Types.EntryRiders.EntryRiders' a printed conjure
-- states, and no others: its counters, face-downness, transformation,
-- attachment, blocking and CR 110.2a controller are sentences no conjure
-- prints.
data ConjureEntry = MkConjureEntry
  { -- | CR 110.5b's status; untapped unless the sentence says otherwise.
    tapped :: TapState.TapState,
    -- | CR 508.4: it enters attacking, and its controller chooses what it
    -- attacks. A Bool rather than an 'Pawl.Types.EntryAttack.EntryAttack':
    -- every printing states the bare "attacking", which is that type\'s
    -- @Chosen@.
    attacking :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Lam, Storm Crane Elder\'s bare "onto the battlefield", and the value the
-- codec writes as the bare tag.
defaultValue :: ConjureEntry
defaultValue = MkConjureEntry {tapped = TapState.Untapped, attacking = False}
