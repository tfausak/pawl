module Pawl.Types.LookAt where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's LookAt arm: WHICH cards CR 701.20e's look
-- shows, and the slot the resolution remembers them in.
--
-- Two fields rather than three, and the missing one is WHO looks. Rule 701.20e
-- shows the cards "only to the specified player", which pawl has nowhere to
-- record: there is no per-player view of the game state at all
-- (docs/design.md's PlayerView), so a looker field would be data nothing could
-- read (#1412). Every printing in the pool has the resolving controller look at
-- their own library, which is what the ObjectRef already says.
data LookAt = MkLookAt
  { ref :: ObjectRef.ObjectRef,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
