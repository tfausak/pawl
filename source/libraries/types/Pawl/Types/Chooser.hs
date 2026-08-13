module Pawl.Types.Chooser where

-- | WHO announces a choice an effect offers as it is applied (CR 608.2d), for
-- Pawl.Types.ObjectRef's ChosenCardInGraveyard.
--
-- CR 608.2c makes the controller of the spell or ability the player who follows
-- its instructions, so the default needs no spelling at all; this type exists
-- because a sentence can hand the instruction to somebody else, and then the
-- player who announces the choice is the player the instruction names.
--
-- It also decides HOW MANY choices there are, which is not a second axis: one
-- instruction addressed to one player is one choice, and the same instruction
-- addressed to each player in a scope is one choice EACH (CR 608.2e, whose
-- choices are made in APNAP order -- CR 101.4 -- before the actions are
-- processed).
data Chooser
  = -- | CR 608.2c \/ 608.2d's default: the resolving controller, choosing ONCE
    -- across every graveyard the scope names -- Port of Karfell's "return a
    -- creature card from your graveyard to the battlefield tapped", and
    -- Extract from Darkness' "a graveyard", where the scope widens and the
    -- chooser does not.
    TheController
  | -- | Each player the scope names, choosing in THEIR OWN graveyard -- Exhume's
    -- "each player puts a creature card from their graveyard onto the
    -- battlefield". The scope does double duty here, which is what the sentence
    -- itself does: "each player ... their graveyard" names the choosers and the
    -- graveyards with one phrase, and a player is never offered a card out of
    -- somebody else's.
    --
    -- Up to ONE card per player in scope, so the ref names as many cards as
    -- there are players who have one. A player with no matching card is not
    -- asked and contributes nothing, which is CR 101.3 applied to that player's
    -- share of the instruction rather than to the whole of it.
    --
    -- WHO CONTROLS what arrives is not this type's question: CR 110.2a gives a
    -- battlefield arrival to the player the effect instructed, which for this
    -- arm is the graveyard's own player, and Pawl.Types.EntryRiders' underOwner
    -- is what a card says to get that -- a graveyard is filed under the card's
    -- owner (CR 400.3), so the two name the same seat. Dredge the Mire's "each
    -- opponent chooses a creature card in their graveyard. Put those cards onto
    -- the battlefield under YOUR control" is the same chooser with the rider
    -- left off.
    EachInScope
  deriving (Eq, Ord, Show)
