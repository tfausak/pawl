module Pawl.Types.WhenSpent where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 106.6's third shape as card data: a delayed triggered ability (CR 603.7a)
-- that triggers when the mana is spent to cast a spell -- Pyromancer's Goggles'
-- "When that mana is spent to cast a red instant or sorcery spell, copy that
-- spell". Rides Pawl.Types.ManaAddition beside the restriction and the rider.
--
-- The ability by NAME, Pawl.Types.ArmDelayedTrigger's reason: Pawl.Types.Effect
-- is first-order, so the payload lives in Pawl.Types.Face.delayedAbilities and
-- this names it. Its effects read "that spell" as Pawl.Engine.Binding.castSpell.
data WhenSpent = MkWhenSpent
  { -- | Which spells spending the mana to cast fires it, matched against the spell
    -- as Pawl.Types.ManaRider's condition is.
    casts :: Filter.Filter Keyword.Keyword,
    ability :: AbilityName.AbilityName
  }
  deriving (Eq, Ord, Show)
