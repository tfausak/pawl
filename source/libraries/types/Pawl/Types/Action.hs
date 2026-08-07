module Pawl.Types.Action where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.ObjectId as ObjectId

-- | What a player with priority may do. CR 116.2 lists twelve SPECIAL actions
-- and two of them are here -- CR 116.2a's land play and CR 116.2b's turning a
-- face-down permanent face up; the tracker for the other ten is #875. Grows.
data Action
  = Pass
  | -- | CR 305.1's special action: put this land card onto the battlefield. The
    -- CardName is CR 712.12's chosen face -- "a player playing a modal
    -- double-faced card ... as a land chooses one of its faces that's a land
    -- BEFORE putting it onto the battlefield" -- so the choice rides on the
    -- action for the reason Cast's does below, and the same argument picks a
    -- name over an index into Card.faces.
    --
    -- A MAYBE, where Cast's is not. Every castable face has a name of its own
    -- (CR 709.4a), but the face a one-faced land is played as is not a chosen
    -- one at all, and CR 709.4's COMBINED view of a split card is not any single
    -- face's either -- so Nothing is "no face was chosen", which
    -- Pawl.Engine.Game.resolveFace already reads as CR 712.8a's default.
    Play ObjectId.ObjectId (Maybe CardName.CardName)
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
    -- CR 702.37c / 708.4: the Facing is the same kind of already-made choice one
    -- rule over. "To cast a card using its morph ability, turn it face down and
    -- announce that you're using a morph ability", and CR 708.4 puts that
    -- turning-over BEFORE the card is put onto the stack -- so casting face down
    -- and casting face up are two actions over one card, and the player chooses
    -- by choosing an action rather than by answering a prompt inside the
    -- announcement. FaceUp is every ordinary cast.
    --
    -- Beside the CardName rather than replacing it: the two answer different
    -- questions. The name is WHICH HALF of the card is underneath (CR 709.3),
    -- which a face-down cast still has to fix -- CR 702.37e reads the morph cost
    -- off that half when the permanent is turned face up -- and the Facing is
    -- whether the object arrives showing it.
    Cast ObjectId.ObjectId CardName.CardName Facing.Facing
  | -- | CR 602: activate the source permanent's ability. Carries the ability
    -- VALUE (validated by membership in Projection.abilitiesOf), never an
    -- index.
    Activate ObjectId.ObjectId (ActivatedAbility.ActivatedAbility Card.Card)
  | -- | CR 116.2b / 702.37e: turn a face-down permanent you control face up.
    -- "A player can take this action any time they have priority", and it does
    -- not use the stack -- so it is an Action rather than anything that goes
    -- through Pawl.Engine.Stack, exactly as CR 116.2a's land play is.
    --
    -- Carries only the permanent. What it costs is not a choice: CR 702.37e
    -- fixes it as "the permanent's morph cost ... if it were face up", so
    -- Pawl.Engine.FaceDown reads it off the card rather than the player naming
    -- it. Nor is the RESULT one -- turning face up reveals what the card is, and
    -- the card is already decided.
    TurnFaceUp ObjectId.ObjectId
  deriving (Eq, Ord, Show)
