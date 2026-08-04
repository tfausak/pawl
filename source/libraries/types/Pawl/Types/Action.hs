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
    -- The face's CardName, not an index into Card.faces (CR 709.4a refers to a
    -- card's faces by name): a name in a decision log either resolves or fails
    -- loudly, where an index silently replays as the wrong half if the card
    -- data is ever reordered. Unlike Activate below this is a positional-but-
    -- stable reference rather than a value, which is safe because no effect
    -- grants a card a face.
    Cast ObjectId.ObjectId CardName.CardName
  | -- | CR 602: activate the source permanent's ability. Carries the ability
    -- VALUE (validated by membership in Projection.abilitiesOf), never an
    -- index.
    Activate ObjectId.ObjectId (ActivatedAbility.ActivatedAbility Card.Card)
  deriving (Eq, Ord, Show)
