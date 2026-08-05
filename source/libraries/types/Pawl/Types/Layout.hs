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
  | -- | CR 712.2: a NONMODAL double-faced card -- one Magic card face on each
    -- side, with an ability on one or both faces that turns the card over. The
    -- constructor keeps the name CR 712.1 records as the older one for the same
    -- kind of card ("previously called 'transforming double-faced cards'"),
    -- since turning over is exactly what separates it from CR 712.3's modal kind
    -- here: CR 712.11b lets a player choose which face of a MODAL card they cast,
    -- where CR 712.11 casts this one with its front face up and nothing else.
    --
    -- FIRST face front, the rest back -- the positional reading Adventure above
    -- already takes. CR 712.2a/712.2b give the two faces printed SYMBOLS rather
    -- than an order, so Pawl.Types.Card.faces' printed order is what stands in
    -- for them, and CR 712.8a/712.8d is what makes the front face the one
    -- Pawl.Engine.Card.combined answers with.
    --
    -- The other two kinds of double-faced card CR 712.1 lists have no constructor
    -- here: modal double-faced cards (CR 712.3, #697) and meld cards (CR 712.4,
    -- #369).
    Transforming
  deriving (Eq, Ord, Show)
