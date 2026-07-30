module Pawl.Types.Cost where

import Pawl.Types.CostComponent (CostComponent)
import Pawl.Types.ManaCost (ManaCost)

-- CR 118.1: "a cost is an action or payment necessary to take another action".
-- ONE type for both carriers -- a spell's cost (CR 601.2f) and an activated
-- ability's activation cost (CR 602.1a) -- because the rules make them the same
-- thing: a mana part plus the non-mana components.
--
-- `mana` carries CR 118.6's distinction in the type, and the two cases are NOT
-- interchangeable:
--
--   Nothing              = an UNPAYABLE cost. CR 118.6: "Some objects have no
--                          mana cost. This represents an unpayable cost. ...
--                          attempting to pay an unpayable cost is an illegal
--                          action." This is the same fact Card.manaCost's Maybe
--                          already carries for CR 202.1 (a land), passed straight
--                          through by Pawl.Cost.costsFor.
--   Just (MkManaCost []) = {0}, which is real and payable. CR 118.5: "the action
--                          necessary for a player to pay such a cost is the
--                          player's acknowledgment that they are paying it";
--                          CR 118.5a: an activated ability whose cost is {0} is
--                          still activated the normal way. ManaCost is a list of
--                          symbols and the empty list IS {0}.
--
-- Scryfall spells the difference exactly: Ancestral Vision's mana_cost is '',
-- Ornithopter's is '{0}'.
data Cost = MkCost
  { mana :: Maybe ManaCost,
    components :: [CostComponent]
  }
  deriving (Eq, Ord, Show)
