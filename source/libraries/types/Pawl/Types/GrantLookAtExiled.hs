module Pawl.Types.GrantLookAtExiled where

import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 406.3's look permission as an instruction gives it, and CR 702.75a's
-- rider on top of it. The two travel in one opcode because rule 702.75a's
-- hideaway gives BOTH at once: the player it instructs to look at the cards and
-- exile one face down is rule 406.3's continuing looker, and the granted ability
-- names the exiling permanent's controller besides.
data GrantLookAtExiled = MkGrantLookAtExiled
  { -- | The exiled cards the permission is written onto.
    cards :: ObjectRef.ObjectRef,
    -- | CR 702.75a: whether the permission ALSO follows control of the permanent
    -- that exiled the card, rather than naming CR 109.5's "you" alone.
    followsExiler :: Bool
  }
  deriving (Eq, Ord, Show)
