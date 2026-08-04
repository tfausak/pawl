-- | CR 709-722: the comprehensive rules' own enumeration of card layouts. One
-- constructor per layout that has actually landed; casing on this in the closed
-- half is the same act as casing on a Phase or a Keyword (design.md section 1),
-- since these are numbered sections of the rulebook rather than the identity of
-- an effect.
module Pawl.Types.Layout where

data Layout
  = -- | A card with exactly one face: every card printed without a second set
    -- of characteristics, which is the whole pool today.
    Normal
  | -- | CR 709.1: "Split cards have two card faces on a single card. The back
    -- of a split card is the normal Magic card back."
    Split
  | -- | CR 715.1: a card with "a two-part card frame, with a smaller frame
    -- inset within their text box". CR 715.2 makes the inset frame's text a set
    -- of ALTERNATIVE characteristics the object may have while it is a spell,
    -- which is what separates this from Split: a split card's halves are peers
    -- and CR 709.4 combines them, where CR 715.4 says an adventurer card has
    -- only its NORMAL characteristics everywhere but the stack.
    --
    -- FIRST face normal, the rest alternative. The rules give the two frames
    -- fixed positions (CR 715.2's inset frame on the left, the card's own on the
    -- right) rather than names, so the order Pawl.Types.Card.faces already
    -- carries is what says which is which -- the same positional reading CR
    -- 712.8a's front face will take.
    Adventure
  deriving (Eq, Ord, Show)
