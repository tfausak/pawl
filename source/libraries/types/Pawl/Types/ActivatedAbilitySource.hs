module Pawl.Types.ActivatedAbilitySource where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 602: an activated ability on the stack -- the source permanent's id plus
-- the ability. The ability travels with the object so it resolves even if the
-- source leaves (CR 113.7a).
--
-- The id alone is enough once the source is gone: CR 400.7 mints a fresh id on
-- every zone change, so this one then names nothing, which is exactly the
-- trigger for CR 608.2h's last known information, filed under the same id in
-- GameState.lastKnown. Nothing about the source is copied in here; a snapshot
-- would have to be kept in step with a source that can still change while the
-- ability waits on the stack.
--
-- A record rather than two positional fields, so which of the two the id names
-- cannot be got wrong at a construction site and so the arm has the one payload
-- a codec needs. Pawl.Types.Source is what this is an arm of.
data ActivatedAbilitySource = MkActivatedAbilitySource
  { source :: ObjectId.ObjectId,
    ability :: ActivatedAbility.ActivatedAbility Card.Card
  }
  deriving (Eq, Ord, Show)
