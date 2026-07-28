module Pawl.Type.PlayerEffect where

import Numeric.Natural (Natural)
import Pawl.Type.Filter (Filter)
import Pawl.Type.ManaCost (ManaCost)

-- CR 611.1's third clause: a continuous effect that "affects players or the
-- rules of the game" rather than the characteristics of an object. The player
-- analogue of Pawl.Type.Modification, and NOT a member of it: CR 613.1 makes the
-- seven layers a machine for computing an OBJECT's characteristics, while CR
-- 613.10 and 613.11 apply these AFTER that machine has run. There is no Layer
-- constructor here and Pawl.Projection never sees this type.
--
-- Open-half card data. Pawl.PlayerEffect is the ONLY module that may case on it.
data PlayerEffect
  = -- CR 601.3 / Silence: this player can't cast spells at all.
    CantCastSpells
  | -- CR 601.3 / Rule of Law: this player can't cast more than this many spells
    -- each turn. The limit is carried, not hardcoded: Rule of Law and Arcane
    -- Laboratory both say one, and a card that says two must not need a sibling
    -- constructor.
    CantCastMoreThan Natural
  | -- CR 613.11 / 601.2f / Thalia: matching spells cost this much more generic
    -- mana to cast.
    IncreaseSpellCost Filter Natural
  | -- CR 613.11 / 601.2f / Sapphire Medallion, Edgewalker: matching spells cost
    -- this much less to cast.
    --
    -- A SEPARATE constructor from IncreaseSpellCost, never one signed delta. The
    -- rules distinguish them in two ways a signed integer cannot express: CR
    -- 601.2f applies every increase BEFORE any reduction, and CR 118.7a gives a
    -- reduction a restriction an increase does not have -- it "can't affect the
    -- colored or colorless mana components". Collapsing them would put both
    -- rules into arithmetic that cannot state either.
    --
    -- An AMOUNT OF MANA rather than a bare number, because CR 118.7 reduces by
    -- mana of a stated type and not only by generic: the Medallion's {1} and
    -- Edgewalker's {W}{B} are the same shape of thing. Pawl.Cost.applyAdjustments
    -- reads it component by component -- generic off generic (CR 118.7a), each
    -- typed symbol off one matching typed symbol.
    --
    -- An EXCESS typed symbol is dropped rather than spilling onto the generic
    -- component, which is Edgewalker's "This effect reduces only the amount of
    -- colored mana you pay" and not CR 118.7b-d (#309).
    ReduceSpellCost Filter ManaCost
  | -- CR 402.2 / Reliquary Tower: this player has no maximum hand size.
    NoMaximumHandSize
  deriving (Eq, Ord, Show)
