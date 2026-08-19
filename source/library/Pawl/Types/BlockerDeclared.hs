module Pawl.Types.BlockerDeclared where

import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 509.1i: a creature was declared blocking, and which attacker it blocked.

-- BOTH fields are an ObjectId and they are NOT interchangeable, so they are named
-- rather than positional: a swap would say the attacker blocked the blocker.
data BlockerDeclared = MkBlockerDeclared
  { blocker :: ObjectId.ObjectId,
    attacker :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
