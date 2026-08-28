-- | CR 709-722: the comprehensive rules' own enumeration of card layouts. One
-- constructor per layout that has actually landed; casing on this in the closed
-- half is the same act as casing on a Phase or a Keyword (design.md section 1),
-- since these are numbered sections of the rulebook rather than the identity of
-- an effect.
module Pawl.Types.Layout where

data Layout
  = -- | A card with exactly one face: every card printed without a second set
    -- of characteristics, which is all but a handful of the pool.
    Normal
  | -- | CR 709.1: "Split cards have two card faces on a single card. The back
    -- of a split card is the normal Magic card back."
    Split
  | -- | CR 709.5: "Some split cards are permanent cards with a single shared type
    -- line." A Room -- the opposite shape to Split above, and not a narrowing of
    -- it. CR 709.3b makes an ordinary split card's other half stop existing once
    -- one half is on the stack; CR 709.5b says the reverse of this one ("The
    -- existence of each half of an object with a shared type line is part of that
    -- object's copiable values, even if that object is a spell on the stack. This
    -- is an exception to rule 709.3b."), and both halves go on being halves of the
    -- one permanent that results.
    --
    -- What separates the two everywhere else is that a Room's halves are
    -- SUBTRACTED rather than combined. CR 709.5's shared type line "represents two
    -- static abilities that function on the battlefield" -- "As long as this
    -- permanent doesn't have the 'left half unlocked' designation, it doesn't have
    -- the name, mana cost, or rules text of this object's left half", and the
    -- mirror of it for the right -- so CR 709.4's combined view is where a Room
    -- STARTS and the locked halves are then taken back out
    -- (Pawl.Engine.Card.roomFace). Off the battlefield no half is locked, because
    -- CR 709.5c's designations are ones "a permanent on the battlefield can have",
    -- and there the combined view is all there is.
    --
    -- LEFT and RIGHT are Pawl.Types.Card.faces' printed order, the same positional
    -- reading Adventure and the two double-faced arms below take. CR 709.5d
    -- ("given the 'left half unlocked' designation as it enters the battlefield if
    -- its left half was cast as a spell") is what makes the order load-bearing
    -- here rather than decorative.
    Room
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
    -- 712.8a's front face takes below.
    Adventure
  | -- | CR 712.2: a NONMODAL double-faced card -- one Magic card face on each
    -- side, with an ability on one or both faces that turns the card over. The
    -- constructor keeps the name CR 712.1 records as the older one for the same
    -- kind of card ("previously called 'transforming double-faced cards'"),
    -- since turning over is exactly what separates it from CR 712.3's modal kind
    -- here: CR 712.11b lets a player choose which face of a MODAL card they cast,
    -- where CR 712.11 casts this one with its front face up by DEFAULT. Not
    -- always -- CR 712.11a's cast "transformed" or "converted" puts a back face
    -- on the stack, and CR 702.162a's more than meets the eye is the ability a
    -- card prints to reach it -- but the default is what CR 712.11b displaces and
    -- so is what separates the two layouts.
    --
    -- FIRST face front, the rest back -- the positional reading Adventure above
    -- already takes. CR 712.2a/712.2b give the two faces printed SYMBOLS rather
    -- than an order, so Pawl.Types.Card.faces' printed order is what stands in
    -- for them, and CR 712.8a/712.8d is what makes the front face the one
    -- Pawl.Engine.Card.combined answers with.
    Transforming
  | -- | CR 712.3: a MODAL double-faced card -- "Modal double-faced cards have a
    -- Magic card face on each side. These faces are usually independent from one
    -- another, but they may have an ability that allows them to 'transform' or
    -- 'convert' on either face."
    --
    -- Independent is the whole of what separates it from Transforming above.
    -- CR 712.11b lets a player choose which face they are casting where CR
    -- 712.11 gives a nonmodal card its front face by default (CR 712.11a's
    -- "converted" cast being the printed exception, more than meets the eye), and
    -- CR 712.8f gives the
    -- resulting spell or permanent "only the characteristics of the face that's
    -- up" with none of CR 712.8e's mana-value exception carried over.
    --
    -- FIRST face front, the rest back -- the positional reading Adventure and
    -- Transforming above already take, and CR 712.3a/712.3b's printed symbols
    -- are what Pawl.Types.Card.faces' printed order stands in for.
    --
    -- Named in full rather than `Modal`, because "modal" already means CR 700.2's
    -- modal spells everywhere else in pawl (Pawl.Types.Modal, Pawl.Engine.Modal,
    -- Face.spell's modes) and the two have nothing to do with each other.
    ModalDoubleFaced
  | -- | CR 712.4: "Meld cards have a Magic card face on one side and half of an
    -- oversized card face on the other." A COMPONENT card of a meld pair -- CR
    -- 712.4a's "one card in each meld pair has an ability that exiles both that
    -- object and its counterpart and melds them", and its counterpart -- and
    -- never the combined back face, which is that rule's "resulting permanent"
    -- rather than a printing.
    --
    -- ONE face, where the two double-faced arms above carry two, and CR 712.4b is
    -- why nothing is lost: a meld card's half of the oversized face "fails to
    -- determine its characteristics" anywhere but on a melded permanent, so there
    -- is nothing for pawl to store and no reader that could ask for it. Where the
    -- combined face DOES live is the ability that melds, which is where the
    -- printed text names it.
    --
    -- Two rules need the classification, and they are the whole of what this
    -- constructor buys over Normal. CR 701.42b: "Tokens, cards that aren't meld
    -- cards, or meld cards that don't form a meld pair can't be melded." CR
    -- 712.4c: "Unlike other double-faced cards, meld cards cannot be transformed
    -- or converted. Any instructions to do so are ignored" -- which CR 712.9
    -- states again from the permanent's side, and which is what keeps this arm off
    -- Pawl.Engine.Card.turnedOver's permitted pair.
    --
    -- The keyword action itself (CR 701.42a) is Pawl.Types.Effect's Meld, and
    -- Pawl.Engine.Event.meld reads this arm to answer CR 701.42b.
    Meld
  deriving (Bounded, Enum, Eq, Ord, Show)
