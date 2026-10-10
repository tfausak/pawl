module Pawl.Types.AlternativeActivationCost where

import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.TurnScope as TurnScope

-- | The payload of Pawl.Types.PlayerEffect's AlternativeActivationCost arm:
-- Kíli the Resourceful's "you may pay {0} rather than pay the equip cost of the
-- first equip ability you activate each turn". CR 118.9 through CR 602.2b: the
-- player may announce this cost at CR 601.2b in place of the ability's whole
-- activation cost, and CR 118.9d then applies increases and reductions to it.
data AlternativeActivationCost = MkAlternativeActivationCost
  { -- | The rule 702 keyword whose ability it replaces the cost of -- "the
    -- EQUIP cost", compared through Pawl.Types.ActivatedAbility.keyword.
    grantedBy :: KeywordDesignator.KeywordDesignator,
    -- | "Of the FIRST equip ability you activate each turn", read exactly as
    -- Pawl.Types.CostModifier.onlyFirst is. Nothing is every matching
    -- activation.
    onlyFirst :: Maybe TurnScope.TurnScope,
    -- | What is paid instead: the {0}.
    cost :: ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
