module Pawl.Types.SpellWasCopied where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 707.10: a copy of a spell was put onto the stack -- the player who
-- copied it, who owns and controls the copy, and the copy itself.
data SpellWasCopied = MkSpellWasCopied
  { player :: PlayerId.PlayerId,
    copy :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
