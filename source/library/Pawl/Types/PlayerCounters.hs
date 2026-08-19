module Pawl.Types.PlayerCounters where

import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 122: "these players, this kind, this many" -- the payload shared by
-- Pawl.Types.Effect's GainPlayerCounters and RemovePlayerCounters arms (#1305).
--
-- Those two are separate CONSTRUCTORS for the reason LoseLife and GainLife are:
-- a signed amount would fuse two events one day told apart by "whenever you get
-- a counter" text, and a Quantity that went negative would have to answer what a
-- negative COUNT of counters means. They share this payload because the subject
-- coincides exactly, not because the effects do.
--
-- WHEN ONE SHARER NEEDS A FIELD THE OTHER DOES NOT, SPIN OUT A SEPARATE TYPE.
-- Never bolt an optional field on here for one arm's sake -- a record carrying a
-- Maybe for exactly one of its users has become an untagged union, since the
-- field's absence would be how a reader tells the arms apart.
data PlayerCounters = MkPlayerCounters
  { player :: PlayerRef.PlayerRef,
    kind :: PlayerCounterKind.PlayerCounterKind,
    quantity :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
