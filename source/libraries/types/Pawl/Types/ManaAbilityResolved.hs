module Pawl.Types.ManaAbilityResolved where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 605.3b's moment: an activated mana ability resolved, which permanent's it
-- was, and how much mana the activation produced. The payload of
-- Pawl.Types.GameEvent's arm of the same name.
--
-- The PERMANENT and not an ability object, CR 605.3b leaving one uncreated: the
-- source stands in for it here the way it does in
-- Pawl.Engine.Resolve.Effect.performManaAbility.
--
-- The AMOUNT is here for Pawl.Types.TappedForMana's reason -- nothing on the
-- board can answer for it afterwards, the mana being a Pawl.Types.ManaUnit in
-- some pool that carries no reference to its source and may already have been
-- spent. A count and not that event's set of types, because the rule this arm
-- serves is the one that reads "the amount of mana this creature produced"
-- (Tyvar the Bellicose); the types are CR 106.12a's question and
-- Pawl.Types.TappedForMana answers it.
--
-- The WHOLE yield, not the activating player's share: CR 106.4 may split an
-- activation's mana between pools (Yurlok of Scorch Thrash), and "the amount of
-- mana this creature produced" asks what the permanent made rather than whose
-- pool it landed in -- Pawl.Types.TappedForMana's `mana` is read the same way.
data ManaAbilityResolved = MkManaAbilityResolved
  { permanent :: ObjectId.ObjectId,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
