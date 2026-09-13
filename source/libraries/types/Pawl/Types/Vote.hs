module Pawl.Types.Vote where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Vote arm (CR 701.38a): each player,
-- starting with @starter@ and proceeding in turn order, chooses one of the
-- objects @filter@ matches on the battlefield, and the objects tied for most
-- votes are bound at @slot@ for a later effect of the same resolution to name.
--
-- Rule 701.38b's choices may be objects, words, or other variables. Objects are
-- the arm here -- Council's Judgment's "a nonland permanent you don't control"
-- -- so the listed choices are a Filter, swept ONCE before the first vote, which
-- is also what makes every voter's list the same list.
--
-- Not implemented: rule 701.38b's WORDS with no rules meaning, each connected to
-- a different effect, which is every "will of the council" printing that votes
-- between two named consequences (#3679). Not implemented: rule 701.38d's
-- several votes for one player (#3680).
data Vote = MkVote
  { -- | CR 701.38a's "specified player", the seat the vote starts with.
    starter :: PlayerRef.PlayerRef,
    -- | CR 701.38b: the listed choices, as a sweep of the battlefield.
    filter :: Filter.Filter Keyword.Keyword,
    -- | Where CR 701.38a's outcome is bound: the objects tied for most votes,
    -- for a later effect of the same resolution to name.
    slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
