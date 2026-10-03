module Pawl.Types.LoopTrail where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StateDigest as StateDigest

-- | CR 732.3: the priority choices one priority loop has seen, and where each
-- game state recurred among them. Loop-local, so no codec.
data LoopTrail = MkLoopTrail
  { -- | Each choice taken at a prompt that offered one, oldest first, with
    -- the state it was taken in.
    choices :: Seq.Seq (StateDigest.StateDigest, PlayerId.PlayerId, Action.Action),
    -- | Each digested state's positions in `choices`, newest first.
    seen :: Map.Map StateDigest.StateDigest [Int],
    -- | The latest choice and the state it was made in, held until the next
    -- prompt shows whether it changed anything: CR 733.1 reverses an illegal
    -- action, which then never happened.
    pending :: Maybe (StateDigest.StateDigest, PlayerId.PlayerId, Action.Action)
  }
  deriving (Eq, Show)

-- | A loop that has seen nothing yet.
empty :: LoopTrail
empty = MkLoopTrail {choices = Seq.empty, seen = Map.empty, pending = Nothing}
