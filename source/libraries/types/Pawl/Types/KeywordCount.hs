module Pawl.Types.KeywordCount where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.KeywordTally as KeywordTally
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally

-- | The N of CR 702.181a's "mobilize N" and CR 702.189a's "firebending N": a
-- printed number, or one the card restates as a value its ability reads as it
-- resolves (CR 608.2h).
--
-- PARAMETRIC in the keyword for Pawl.Types.Devour's reason: the Filter can name
-- a Keyword and Keyword names this. Only @KeywordCount Keyword.Keyword@ is ever
-- written.
data KeywordCount keyword
  = -- | A printed number (Dalkovan Packbeasts' mobilize 3).
    Fixed Natural.Natural
  | -- | CR 208.1: the power of the object carrying the keyword (Firebending
    -- Student's "firebending X, where X is this creature's power").
    Power
  | -- | How many objects in the scope the filter keeps (Avenger of the Fallen's
    -- "the number of creature cards in your graveyard").
    Tally (KeywordTally.KeywordTally keyword)
  | -- | CR 122.1: how many counters of a kind a player has (Zuko, Firebending
    -- Master's "the number of experience counters you have").
    PlayerCounters PlayerCounterTally.PlayerCounterTally
  deriving (Eq, Ord, Show)
