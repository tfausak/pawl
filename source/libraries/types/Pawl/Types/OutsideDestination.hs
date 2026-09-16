module Pawl.Types.OutsideDestination where

-- | CR 400.11b: where an instruction that brings a card into the game from
-- outside it puts that card. The rule says only that such an instruction can
-- exist and that what it brings stays; every card that reaches out there then
-- says where what it finds goes.
--
-- Not a 'Pawl.Types.Zone.Zone', for 'Pawl.Types.SearchDestination''s reason:
-- most zones a Zone can name have no card reaching outside the game behind
-- them, and the library arm below names an END of one (CR 401.2's order), which
-- a Zone cannot say.
data OutsideDestination
  = -- | Death Wish\'s "put a card you own from outside the game into your hand",
    -- and the rest of the wish cycle\'s (CR 400.11b).
    Hand
  | -- | The Raven\'s Warning\'s "put a card you own from outside the game on top
    -- of your library" (CR 401.2).
    --
    -- The END is in the constructor rather than a
    -- 'Pawl.Types.LibraryPosition.LibraryPosition' field, this type\'s "one arm
    -- per sentence" -- Scryfall @o:"outside the game"@, 2026-09-16, no printing
    -- names the bottom, and the only other non-hand destination printed is
    -- Research\'s shuffle, which names no end at all.
    LibraryTop
  deriving (Bounded, Enum, Eq, Ord, Show)
