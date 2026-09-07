module Pawl.Types.ActivateManaAbilities where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Effect's ActivateManaAbilities arm: each player the
-- reference names activates a mana ability of every permanent they control that
-- matches the filter. The instruction is the card's (CR 608.2c); each activation
-- it causes then runs by CR 605.3.
--
-- WHO activates, and therefore who chooses, is settled by CR 602.2: only an
-- object's controller can activate its activated ability. That is why the
-- reference names the player whose permanents are swept rather than a decider.
--
-- A PlayerRef and not a bare SlotName, Pawl.Types.ManaAddition's reason: only a
-- reference can spell CR 109.5's "you" beside a slot the announcement bound, and
-- Drain Power's "target player" is the slot spelling.
--
-- The FILTER is the permanents, not the abilities: "a mana ability of each LAND
-- they control" narrows which permanents are activated and says nothing about
-- which ability of each -- that choice is the activating player's, CR 602.2a
-- having them announce which ability they are activating.
--
-- Control is not in the filter either, since the filter is read in the SPELL's
-- context, where CR 109.5's "you" is its controller rather than the player doing
-- the activating; the arm scopes the candidates to what that player controls,
-- Pawl.Types.PlayerSacrifices' arrangement.
data ActivateManaAbilities = MkActivateManaAbilities
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
