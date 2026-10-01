module Pawl.Types.EntersWith where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.WithCounters as WithCounters

-- | CR 614.1c's as-enters clause where it names KEYWORDS or QUOTED ABILITIES,
-- and the counters the same printed sentence places -- Voidpouncer's "it enters
-- with two +1/+1 counters and a trample counter on it and with haste",
-- Degavolver's "it enters with two +1/+1 counters on it and with 'Pay 3 life:
-- Regenerate this creature.'".
--
-- ONE payload for both halves because CR 616.1 counts replacement EFFECTS and a
-- printed sentence is one: two rows are two candidates, which offers the entering
-- permanent's controller an order the card does not have. Pawl.Types.WithCounters
-- makes the same argument one level down, across the counter kinds of one
-- sentence.
--
-- A grant is required and the counters are not, which is what keeps the two
-- spellings disjoint: a sentence that places counters and grants nothing is
-- EntryRewrite.WithCounters, and this is that sentence's grant-bearing shape.
-- Pawl.Codec.EntersWith rejects a payload with neither keywords nor abilities,
-- so the overlap is unsayable on the wire as well.
--
-- Parametric in the ABILITY for Pawl.Types.CopyException's reason: a quoted
-- ability is a Pawl.Types.GrantedAbility, which this module cannot name.
data EntersWith ability = MkEntersWith
  { counters :: Maybe WithCounters.WithCounters,
    keywords :: Set.Set Keyword.Keyword,
    abilities :: Seq.Seq ability
  }
  deriving (Eq, Ord, Show)
