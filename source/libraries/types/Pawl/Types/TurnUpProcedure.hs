module Pawl.Types.TurnUpProcedure where

-- | WHICH of CR 708.7's turn-face-up procedures a player is taking. Two rules
-- write one, and CR 701.40c is why the choice has to be sayable at all: "if a
-- card with morph is manifested, its controller may turn that card face up using
-- EITHER the procedure described in rule 702.37e ... OR the procedure described
-- above", at two different costs.
--
-- A SECOND type rather than FaceDownReason reused, because the two are not the
-- same question and answering one with the other would be wrong in both
-- directions. Morph's procedure asks nothing about what turned the permanent
-- over -- CR 702.37e admits "a face-down permanent you control with a morph
-- ability", so a permanent Backslide turned face down qualifies -- while
-- manifest's asks about nothing else, since CR 701.40b's subject is "a
-- manifested permanent". And FaceDownReason.TurnedFaceDown and
-- FaceDownReason.EnteredFaceDown name no procedure at all, which a reused type
-- would let a caller ask for.
data TurnUpProcedure
  = -- | CR 702.37e: pay what the permanent's morph cost would be if it were face
    -- up.
    Morph
  | -- | CR 702.168d: pay what the permanent's disguise cost would be if it were
    -- face up. Rule 702.37e's procedure with a different price list, and its
    -- subject is the same shape -- "a face-down permanent you control WITH A
    -- DISGUISE ABILITY" -- so this one asks nothing about the allower either. CR
    -- 701.58d is where both this and the Cloak procedure below are open at once,
    -- the case CR 701.40c already makes for morph and manifest.
    Disguise
  | -- | CR 701.40b: show that the card is a creature card, pay that card's mana
    -- cost.
    Manifest
  | -- | CR 701.58b: show that the card is a creature card, pay that card's mana
    -- cost. Rule 701.40b's procedure word for word, and its own arm because the
    -- two rules' SUBJECTS differ: rule 701.58b's is "a cloaked permanent you
    -- control" and rule 701.40b's is a manifested one, so one procedure covering
    -- both would open each rule's price to the other rule's permanent.
    Cloak
  deriving (Bounded, Enum, Eq, Ord, Show)
