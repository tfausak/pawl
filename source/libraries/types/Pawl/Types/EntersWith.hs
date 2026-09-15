module Pawl.Types.EntersWith where

import qualified Data.Set as Set
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.WithCounters as WithCounters

-- | CR 614.1c's as-enters clause where it names KEYWORDS, and the counters the
-- same printed sentence places -- Voidpouncer's "it enters with two +1/+1
-- counters and a trample counter on it and with haste".
--
-- ONE payload for both halves because CR 616.1 counts replacement EFFECTS and a
-- printed sentence is one: two rows are two candidates, which offers the entering
-- permanent's controller an order the card does not have. Pawl.Types.WithCounters
-- makes the same argument one level down, across the counter kinds of one
-- sentence.
--
-- The keywords are required and the counters are not, which is what keeps the two
-- spellings disjoint: a sentence that places counters and grants nothing is
-- EntryRewrite.WithCounters, and this is that sentence's keyword-bearing shape.
-- Pawl.Codec.EntersWith rejects an empty keyword set, so the overlap is unsayable
-- on the wire as well.
data EntersWith = MkEntersWith
  { counters :: Maybe WithCounters.WithCounters,
    keywords :: Set.Set Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
