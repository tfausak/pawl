module Pawl.Types.FromOutsideTheGame where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 400.11c: which of the cards a player owns outside the game an
-- instruction may bring in, and whether that instruction shows the card first.

-- A record rather than a bare Filter because the two axes are independent. The
-- wish cycle prints both together -- Burning Wish's "reveal a sorcery card you
-- own from outside the game and put it into your hand" -- while Death Wish
-- prints the move alone, and CR 701.20a's showing is a keyword action separate
-- from the move it accompanies.
data FromOutsideTheGame = MkFromOutsideTheGame
  { -- | Which cards out there the instruction admits (CR 400.11c), matched
    -- against the PRINTED FACE since CR 604.3 leaves nothing else out there to
    -- read.
    --
    -- Not a Maybe: an instruction naming no quality -- Death Wish's "a card you
    -- own from outside the game" -- is @And []@, which admits everything, so a
    -- Maybe would give one instruction two spellings.
    filter :: Filter.Filter Keyword.Keyword,
    -- | Whether CR 701.20a's reveal rides along, as the wish cycle prints it and
    -- Death Wish does not.
    reveal :: Bool
  }
  deriving (Eq, Ord, Show)
