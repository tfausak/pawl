module Pawl.Types.PastActivation where

import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 602.2: one activated ability a player began to activate this turn, kept
-- as the facts Pawl.Types.CostModifier's criteria ask of an activation,
-- so "the FIRST activated ability you activate" (Professor Hojo) can be asked
-- of the turn's history whatever has happened to the objects since.
--
-- Every activation, mana abilities included (CR 605.1a): Tezzeret, Betrayer of
-- Flesh's ruling counts a mana ability as a first activation.
data PastActivation = MkPastActivation
  { -- | CR 602.2: the player who activated it.
    activator :: PlayerId.PlayerId,
    -- | The ability's source as the activation began.
    source :: ObjectSnapshot.ObjectSnapshot,
    -- | The rule 702 keyword the ability is under
    -- (Pawl.Types.ActivatedAbility.keyword), for ActivationCriteria.grantedBy.
    keyword :: Maybe Keyword.Keyword,
    -- | CR 605.1a's side of the ability, for ActivationCriteria.whichKind.
    kind :: AbilityKind.AbilityKind,
    -- | CR 601.2c's object targets, as they stood when announced. Not
    -- implemented: their zone, so a criterion asking CR 109.2's "creature" of
    -- them cannot tell a permanent from a spell (#4926).
    targets :: [ObjectSnapshot.ObjectSnapshot]
  }
  deriving (Eq, Ord, Show)
