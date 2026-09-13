module Pawl.Types.VoteObjects where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | CR 701.38b's OBJECTS, the payload of Pawl.Types.VoteChoices' Objects arm:
-- the listed choices are the permanents @filter@ matches, and the objects tied
-- for most votes are bound at @slot@ for a later effect of the same resolution
-- to name (Council's Judgment's "a nonland permanent you don't control", then
-- "exile each permanent from among them").
--
-- The filter is swept ONCE, off the board as the vote begins, which is also
-- what makes every voter's list the same list -- rule 701.38a votes "for one
-- choice from a list", singular.
data VoteObjects = MkVoteObjects
  { filter :: Filter.Filter Keyword.Keyword,
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
