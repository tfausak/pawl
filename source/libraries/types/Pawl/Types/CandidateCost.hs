module Pawl.Types.CandidateCost where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 601.2b: one of the costs a spell may be cast for, together with WHAT
-- OFFERED IT.
--
-- The cost alone is not enough, because four rule 702 abilities condition
-- something AFTER the cast on which candidate was chosen: CR 702.34a's "if the
-- flashback cost was paid" and CR 702.133a's "if this spell was cast using its
-- jump-start ability" both exile the card as it leaves the stack, and CR
-- 702.103b makes a spell cast bestowed an Aura enchantment. A bare
-- @Cost@ answers "what was paid" and never "which cost that was", and the two
-- come apart the moment a second permission offers the same card a second cost
-- from the same zone.
--
-- The tag is a Pawl.Types.Keyword and not a Pawl.Types.KeywordFamily because
-- jump-start and aftermath are NULLARY keywords, which that type deliberately
-- has no constructors for. Nothing means the candidate is the card's own
-- printed cost, one of its printed alternatives, or a cost an effect applied
-- (CR 118.9) -- in each case a cost no keyword ability offered.
--
-- The keyword is the one READ IN THE ZONE the cast was proposed from (CR
-- 613.1), since that is where Pawl.Engine.Cost.candidateCostsFor builds the
-- list: a granted flashback tags its candidate exactly as a printed one does.
data CandidateCost = MkCandidateCost
  { keyword :: Maybe Keyword.Keyword,
    cost :: Cost.Cost Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
