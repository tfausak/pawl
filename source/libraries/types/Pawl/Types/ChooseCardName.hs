module Pawl.Types.ChooseCardName where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Effect's ChooseCardName arm: WHO chooses CR
-- 201.4's name, and CR 201.4a's restriction on what may be chosen.
--
-- A PlayerRef and not CR 109.5's "you", because the chooser is a target slot on
-- Petra Sphinx's "target player chooses a card name" and every player at once on
-- Conundrum Sphinx's. Pawl.Types.Search carries two of these for the same
-- reason: whose library is searched is a question the card asks, not a constant.
--
-- Not implemented: the names the several choosers of ONE instruction picked,
-- kept apart per player. They union into the one Object.chosenNames set on the
-- source, so a reference to "the name THEY chose" cannot pick its chooser's out
-- (#3316). A card naming ONE chooser is exact, the resolve arm assigning that
-- set rather than adding to whatever an earlier resolution left.
data ChooseCardName = MkChooseCardName
  { player :: PlayerRef.PlayerRef,
    restriction :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
