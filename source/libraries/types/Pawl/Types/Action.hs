module Pawl.Types.Action where

import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.TurnUpProcedure as TurnUpProcedure

-- | What a player with priority may do. The SPECIAL actions CR 116.2 lists that
-- are here are CR 116.2a's land play, CR 116.2b's turning a face-down permanent
-- face up, CR 116.2c's paying to end a continuous effect, CR 116.2d's ignoring a
-- static ability's effect, CR 116.2e's Circling Vultures discard, CR 116.2h's
-- foretell, CR 116.2k's plot and CR 116.2m's unlock cost; the tracker for the
-- rest of rule 116.2 is #875. Grows.
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
  | -- | CR 605.3a's FIRST window: activate one of this permanent's mana
    -- abilities while holding priority, with no payment in flight. Separate from
    -- Activate above because CR 605.3b keeps a mana ability off the stack, so
    -- the two arms do different things with the ability they name.
    --
    -- Carries the SOURCE alone, where Activate carries an ability value. CR
    -- 305.6's intrinsic "{T}: Add [type]" is printed on no card and is no member
    -- of the permanent's activated abilities, so an ability value cannot name
    -- the very thing a basic land is tapped for. WHICH mana ability, in which
    -- mode, and in which color is one question about the yield, and it is the
    -- one Prompt.ChooseManaYield already asks inside a payment (CR 605.3a's
    -- other two windows) -- so both windows reach the pool through one
    -- mechanism rather than two.
    ActivateManaAbility ObjectId.ObjectId
  | -- | CR 116.2b / 702.37e: turn a face-down permanent you control face up.
    -- "A player can take this action any time they have priority", and it does
    -- not use the stack -- so it is an Action rather than anything that goes
    -- through Pawl.Engine.Stack, exactly as CR 116.2a's land play is.
    --
    -- Carries the PROCEDURE as well as the permanent, Unlock's shape below,
    -- because CR 701.40c makes it a choice: "if a card with morph is manifested,
    -- its controller may turn that card face up using EITHER the procedure
    -- described in rule 702.37e ... OR the procedure described above" -- two
    -- procedures at two different prices, on one permanent. Offering each as its
    -- own legal action is how the engine declines to pick.
    --
    -- What each one COSTS is not a choice: CR 702.37e fixes one as "the
    -- permanent's morph cost ... if it were face up" and CR 701.40b the other as
    -- the card's mana cost, so Pawl.Engine.FaceDown reads both off the card
    -- rather than the player naming either. Nor is the RESULT one -- turning
    -- face up reveals what the card is, and the card is already decided.
    TurnFaceUp ObjectId.ObjectId TurnUpProcedure.TurnUpProcedure
  | -- | CR 116.2m / 709.5e: pay a locked half's mana cost to give this permanent
    -- the matching unlocked designation. "A player can take this action any time
    -- they have priority and the stack is empty during a main phase of their
    -- turn", and it does not use the stack -- so it is an Action rather than
    -- anything that goes through Pawl.Engine.Stack, exactly as CR 116.2a's land
    -- play is.
    --
    -- Carries the DOOR as well as the permanent, TurnFaceUp's shape above,
    -- because here there is a choice to make: CR 709.5e says "a locked half", and
    -- a Room with both doors shut offers two different actions at two different
    -- prices.
    -- Naming it rather than indexing into Card.faces is Cast's argument
    -- unchanged -- CR 709.4a gives the halves names, and a name either resolves
    -- or fails loudly where an index silently replays as the wrong door.
    --
    -- What it COSTS is not a choice: CR 709.5e fixes it as the named half's mana
    -- cost, so Pawl.Engine.Room reads it off the card rather than the player
    -- naming it.
    Unlock ObjectId.ObjectId CardName.CardName
  | -- | CR 116.2e: discard a card in your hand whose own text grants the
    -- permission. It does not use the stack (CR 116.1), so it is an Action
    -- rather than anything that goes through Pawl.Engine.Stack, exactly as CR
    -- 116.2a's land play is.
    --
    -- Carries only the object, where TurnFaceUp and Unlock above carry a choice
    -- beside it: CR 116.2e leaves nothing to choose. Which card is discarded is
    -- the card whose ability it is, and there is no cost.
    --
    -- CR 116.1 is also why this is only ever OFFERED: a special action is not
    -- one the game generates, so the engine never takes it unasked.
    DiscardFromHand ObjectId.ObjectId
  | -- | CR 116.2d: pay a cost to ignore, until end of turn, the effect of the
    -- static abilities this permanent grants the permission on. It does not use
    -- the stack (CR 116.1), so it is an Action rather than anything that goes
    -- through Pawl.Engine.Stack, exactly as CR 116.2a's land play is.
    --
    -- Carries only the SOURCE PERMANENT, DiscardFromHand's shape. What it costs is
    -- printed on that permanent, and which of its abilities is ignored is not a
    -- choice pawl offers -- see Pawl.Types.SpecialAction (#1267). WHO ignores it
    -- is the player taking the action, which is the priority holder; whether it
    -- is offered to them at all is CR 116.2d's own question, answered by
    -- Pawl.Engine.Ignore.canIgnore.
    Ignore ObjectId.ObjectId
  | -- | CR 116.2k / 702.170a: pay a card's plot cost and exile it from your hand,
    -- making it a plotted card. "Any time you have priority during your main
    -- phase while the stack is empty", and it does not use the stack (CR
    -- 702.170b) -- so it is an Action rather than anything that goes through
    -- Pawl.Engine.Stack, exactly as CR 116.2a's land play is.
    --
    -- Carries only the card, Unlock's argument in reverse: CR 702.170a leaves
    -- nothing to choose. What it costs is the keyword's own payload, which
    -- Pawl.Engine.Plot reads off the card, and the destination is fixed at exile.
    --
    -- WHICH HALF is not carried either, where Cast and Play both carry a name. CR
    -- 702.170a exiles "this card" rather than a half, and the keyword is a
    -- characteristic of a face rather than of the card -- so a split card with
    -- plot on one half would plot the whole card all the same. No printing has
    -- plot on a multi-faced card.
    Plot ObjectId.ObjectId
  | -- | CR 116.2h / 702.143a: pay {2} and exile a card with foretell from your
    -- hand face down, making it a foretold card. "Any time a player has priority
    -- during their turn", and it does not use the stack (CR 702.143b) -- so it is
    -- an Action rather than anything that goes through Pawl.Engine.Stack, exactly
    -- as CR 116.2a's land play is.
    --
    -- Carries only the card, Plot's shape: rule 702.143a leaves nothing to
    -- choose. What it costs is not the keyword's payload but the rule's own {2},
    -- which Pawl.Engine.Foretell mints, and the destination is fixed at exile.
    --
    -- WHICH HALF is not carried, for Plot's reason: rule 702.143a exiles "a card
    -- with foretell" rather than a half. No printing has foretell on a
    -- multi-faced card.
    Foretell ObjectId.ObjectId
  | -- | CR 116.2c: pay a cost an effect named to END that effect. "You may pay
    -- {U} to end this effect", the clause every Licid prints. "A player can take
    -- such an action any time they have priority, unless that effect specifies
    -- another timing restriction", and it does not use the stack (CR 116.1) -- so
    -- it is an Action rather than anything that goes through Pawl.Engine.Stack,
    -- exactly as CR 116.2a's land play is.
    --
    -- Carries the effect's SOURCE, Ignore's shape above, because pawl gives a
    -- stored continuous effect no id of its own. That is exact rather than
    -- convenient: one printed sentence stores several effects at once and CR
    -- 116.2c ends "that effect", the sentence -- so the payment must reach all of
    -- them together, and the source is what they share. Two live pay-to-end
    -- offers from one object cannot coexist, since a Licid that has activated has
    -- lost the ability and can no longer activate it.
    --
    -- What it COSTS is not a choice: the effect named the price when it was
    -- stored (Expiry.WhenPaid), so Pawl.Engine.EndEffect reads it off the stored
    -- effect rather than the player naming it. WHO may take it is the player who
    -- ACTIVATED the ability that stored the effect (CR 109.5's "you", for an
    -- activated ability), answered by Pawl.Engine.EndEffect.canEnd off the seat
    -- baked into the stored effect.
    EndEffect ObjectId.ObjectId
  deriving (Eq, Ord, Show)
