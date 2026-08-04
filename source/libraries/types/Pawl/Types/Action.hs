module Pawl.Types.Action where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ObjectId as ObjectId

-- | Grows: special actions beyond Play.
data Action
  = Pass
  | Play ObjectId.ObjectId
  | -- | CR 709.3: which half of a split card is being cast is chosen BEFORE the
    -- card is put onto the stack, so the choice rides on the ACTION rather than
    -- becoming a prompt partway through the announcement. CR 601.2b's last
    -- sentence is what that buys: a previously made choice may restrict the
    -- later ones.
    --
    -- A CardName, not an index into Card.faces. CR 709.4a is what gives a
    -- card's faces names at all, and CR 709.3 has the player choose a half by
    -- naming it, so a name is the reference the rules themselves use. It is
    -- also the one that survives a decision log: a name either resolves to a
    -- face or fails loudly, where an index silently replays as the WRONG half
    -- if the card data is ever reordered.
    Cast ObjectId.ObjectId CardName.CardName
  | -- | CR 602: activate the source permanent's ability. Carries the ability
    -- VALUE (validated by membership in Projection.abilitiesOf), never an
    -- index.
    Activate ObjectId.ObjectId (ActivatedAbility.ActivatedAbility Card.Card)
  deriving (Eq, Ord, Show)
