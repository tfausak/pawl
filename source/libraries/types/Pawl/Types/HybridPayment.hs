module Pawl.Types.HybridPayment where

-- | CR 107.4e: "a monocolored hybrid symbol such as {2/B} can be paid with either
-- one black mana or two mana of any type." This is WHICH of those two, announced
-- under CR 118.13a as its controller proposes the spell or ability -- CR 601.2b's
-- "the player announces the nonhybrid equivalent cost they intend to pay", read
-- one symbol at a time -- and under CR 118.13b immediately before a cost paid
-- during a resolution is paid.
--
-- A named sum rather than a Bool, the Pawl.Types.PhyrexianPayment posture, so a
-- transcript reads as the decision it records.
--
-- Neither way needs a payload: the type is the symbol's own and the {2} is fixed
-- by CR 107.4e (Pawl.Types.ManaSymbol.MonocoloredHybrid carries the {2/X} shape
-- ONLY). CR 107.4e's OTHER half -- the colour/colour hybrid {W/U} -- is not this
-- type and could not use it: its two ways are two COLOURS, not one type against
-- an amount of generic mana, so its announcement answers with the mana type
-- itself (Pawl.Types.Prompt.AnnounceHybridHalf).
data HybridPayment
  = -- | CR 107.4e: one mana of the symbol's stated type.
    PaysTyped
  | -- | CR 107.4e: two mana of any type, which CR 601.2b's nonhybrid equivalent
    -- writes as {2} -- generic mana, and so the component CR 118.7a's reductions
    -- come off.
    PaysGeneric
  deriving (Eq, Ord, Show)
