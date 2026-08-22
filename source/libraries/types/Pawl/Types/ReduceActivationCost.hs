module Pawl.Types.ReduceActivationCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.ManaCost as ManaCost

-- | The payload of Pawl.Types.PlayerEffect's ReduceActivationCost arm (#1305).
--
-- The FLOOR is carried rather than assumed, because it is card text (CR 101.1)
-- and not a rule: Heartstone says "This effect can't reduce the mana in that
-- cost to less than one mana" and so carries 1, while Blossoming Tortoise's
-- "Activated abilities of lands you control cost {1} less to activate" does not
-- say it and carries 0. See Pawl.Types.CostAdjustments.reductions for what zero
-- means, why a floor never raises a cost, and why the two kinds cannot share one
-- floor over the pool.
--
-- TWO criteria, and they ask about different things. `whichAbilities` names the
-- ability's SOURCE OBJECT -- Heartstone's "activated abilities of creatures",
-- Blossoming Tortoise's "of lands you control" -- and is matched by
-- Pawl.Engine.PlayerEffect.matchesObject against that object's projection.
-- `grantedBy` names the KIND of ability instead: Nothing is every activated
-- ability of a matching source, and Just a family is only the ability rule 702
-- mints for a keyword of that family, which is Fluctuator's "CYCLING abilities
-- you activate" (#1431). A source filter cannot say that -- a Barkhide Mauler in
-- a hand is not distinguished from itself by anything about the object -- and an
-- ability-shaped Filter atom put in `whichAbilities` could never be true there,
-- since that field is matched against the object and not the ability.
--
-- A rule-702 FAMILY designator and not an effect: what the closed half compares
-- here is which rule minted the ability (Pawl.Engine.Keyword.familyGranting),
-- never what the ability does.
data ReduceActivationCost = MkReduceActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    grantedBy :: Maybe KeywordFamily.KeywordFamily,
    reduction :: ManaCost.ManaCost,
    floor :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
