module Pawl.Types.PermissionCost where

import qualified Pawl.Types.ManaCost as ManaCost

-- | CR 118.9: the alternative cost a CR 601.3 permission states for the card it
-- lets a player cast, paid rather than that card's mana cost. Rides the
-- permission rather than the card, for Pawl.Types.ExilePlayPermission's
-- CR 118.14 reason: the same card cast any other way pays what it prints.
data PermissionCost
  = -- | CR 118.9: this mana rather than the mana cost -- an empty cost is
    -- "without paying its mana cost" (Extract Power), {2} is rule 701.65a's.
    InsteadOfManaCost ManaCost.ManaCost
  | -- | CR 118.9 / 701.67a: "by waterbending {X} rather than paying its mana
    -- cost, where X is its mana value" (Hama, the Bloodbender), X read off the
    -- face being cast (CR 202.3d).
    WaterbendManaValue
  deriving (Eq, Ord, Show)
