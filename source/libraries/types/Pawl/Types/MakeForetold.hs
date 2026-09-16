module Pawl.Types.MakeForetold where

import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 702.143d's two sentences: the cards the reference names become foretold
-- cards, and the effect may give them a foretell cost.
--
-- A record where Pawl.Types.Effect's MakePlotted is a bare ObjectRef, and rule
-- 702.143d's second sentence is the whole difference: CR 702.170d's cast is
-- free, so plot has no cost to carry.
data MakeForetold = MkMakeForetold
  { -- | The cards that become foretold.
    cards :: ObjectRef.ObjectRef,
    -- | CR 702.143d's "that effect may give the card a foretell cost", as the
    -- amount that cost takes off the card's OWN mana cost -- Ethereal Valkyrie's
    -- "its foretell cost is its mana cost reduced by {2}".
    --
    -- A reduction rather than a cost outright, because that is the only sentence
    -- printed: Scryfall @o:"becomes foretold" or o:"foretell cost is"@,
    -- 2026-09-15, returns Ethereal Valkyrie, The Foretold Soldier, Dream
    -- Devourer and Bohn, Beguiling Balladeer, and every one of the three that
    -- states a cost states it this way. A printing naming a cost outright would
    -- be the one that earned a second arm here.
    --
    -- Nothing is rule 702.143d's first sentence alone (The Foretold Soldier),
    -- which leaves the card whatever foretell cost its own keyword prints.
    manaCostReducedBy :: Maybe ManaCost.ManaCost
  }
  deriving (Eq, Ord, Show)
