module Pawl.Types.VoteChoices where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.VoteObjects as VoteObjects

-- | CR 701.38b's listed choices: "objects, words with no rules meaning that are
-- each connected to a different effect, or other variables relevant to the
-- resolution of the spell or ability". The kinds differ in what a vote DECIDES,
-- which is why they are a sum rather than two opcodes: rule 701.38a's tally is
-- one procedure over both, and rule 701.38d's extra votes have to reach both.
--
-- Not implemented: rule 701.38b's third kind, "other variables relevant to the
-- resolution" -- Council Guardian votes for a colour and Mob Verdict for a
-- player (#3717).
data VoteChoices
  = -- | CR 701.38b's objects (Council's Judgment); see Pawl.Types.VoteObjects.
    Objects VoteObjects.VoteObjects
  | -- | CR 701.38b's "words with no rules meaning that are each connected to a
    -- different effect" (Plea for Power's time and knowledge). A word HAS no
    -- rules meaning, so the only thing pawl needs of one is the connection to an
    -- effect, and a SlotName is exactly that: each word's tally is bound as an
    -- amount under the slot of that name, and the clause connected to it reads
    -- the tally as Pawl.Types.Quantity's InSlot.
    --
    -- That spelling is what makes CR 701.38a's tie-break ordinary arithmetic
    -- rather than an outcome of its own: Plea for Power's "if knowledge gets
    -- more votes or the vote is tied" is one Condition comparing the two tallies,
    -- and no card has to name a tie.
    --
    -- A NonEmpty of slots and not a Set: the wire order is the printed order,
    -- which is the order Prompt.ChooseVoteWord offers them in. Rule 701.38b's
    -- "each connected to a DIFFERENT effect" makes a repeated word meaningless,
    -- and Pawl.CardSpec is what holds a card to distinct ones.
    Words (NonEmpty.NonEmpty SlotName.SlotName)
  deriving (Eq, Ord, Show)
