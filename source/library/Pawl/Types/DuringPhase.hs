module Pawl.Types.DuringPhase where

import qualified Pawl.Types.PhaseSelector as PhaseSelector
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 602.5's timing rider: WHICH phase or step the activation is confined to,
-- and WHOSE turns count.

-- There is deliberately no scope-less form: the axis is not a default a card may
-- omit, so a payload naming only a window is a decode failure rather than a
-- silent EachTurn.
data DuringPhase = MkDuringPhase
  { window :: PhaseSelector.PhaseSelector,
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
