module Pawl.Types.ActivationCriteria where

import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordDesignator as KeywordDesignator
import qualified Pawl.Types.LoyaltyKind as LoyaltyKind

-- | What narrows a Pawl.Types.CostModifier to SOME of the activated abilities
-- of a matching source. Each is a fact about the ABILITY being activated, which
-- the source Filter, asked of the object, cannot answer -- and none has a spell
-- counterpart, a spell being one object with one cost.
--
-- Nothing in every field is every activated ability of a matching source.
data ActivationCriteria = MkActivationCriteria
  { -- | Only an ability under this rule-702 keyword (Pawl.Types.ActivatedAbility.keyword):
    -- Fluctuator's "cycling abilities", Bureau Headmaster's "equip abilities".
    -- Spell-free because a spell's keywords are the spell's, which the Filter
    -- reads.
    grantedBy :: Maybe (KeywordDesignator.KeywordDesignator Keyword.Keyword),
    -- | CR 605.1a: Zirda, the Dawnwaker's "that aren't mana abilities",
    -- Suppression Field's "unless they're mana abilities". Spell-free because
    -- rule 605.1a classifies only abilities.
    whichKind :: Maybe AbilityKind.AbilityKind,
    -- | CR 606.2: Carth the Lion's "planeswalkers' loyalty abilities". Spell-free
    -- because rule 606.2 classifies only activated abilities.
    whichLoyalty :: Maybe LoyaltyKind.LoyaltyKind
  }
  deriving (Eq, Ord, Show)
