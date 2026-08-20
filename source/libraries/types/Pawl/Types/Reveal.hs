module Pawl.Types.Reveal where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's Reveal arm: WHICH cards CR 701.20a's
-- reveal shows, and the slot -- if any -- the resolution remembers them in.
--
-- Pawl.Types.LookAt's shape, minus that type's requirement on the slot. A look
-- that binds nothing records nothing at all (CR 701.20e shows the cards to one
-- player, which pawl cannot represent), so LookAt's slot is the whole point of
-- the opcode; a reveal is public, so the GameEvent.Revealed it appends is
-- already a record and a slotless reveal is the whole of rule 701.20a. Merfolk
-- Spy writes one; Wild Evocation's "that player reveals a card at random from
-- their hand ... the player casts IT" needs the name, and so does Carth the
-- Lion's "reveal a planeswalker card from among them AND put it into your hand",
-- where the slot is the whole of what keeps the reveal and the move on one card.
--
-- Maybe rather than a second opcode, for the reason #1743 declined a
-- RevealAtRandom: one rule, one arm. A sibling that revealed AND bound would
-- duplicate rule 701.20a over a wider radius -- every consumer that classifies
-- effects would have to learn both.
data Reveal = MkReveal
  { ref :: ObjectRef.ObjectRef,
    slot :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
