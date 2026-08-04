module Pawl.Types.SearchDestination where

-- | CR 701.23: where a search puts what it finds. The searching rule says only
-- how to find a card; every card that searches then says what happens to it.
--
-- Not a Pawl.Types.Zone, which is wrong twice over: a zone cannot say "tapped",
-- which Evolving Wilds needs, and most zones a Zone can name have no searching
-- card behind them. This type names the WHOLE instruction, one arm per sentence.
data SearchDestination
  = -- | Evolving Wilds' "put it onto the battlefield tapped".
    BattlefieldTapped
  | -- | Braidwood Sextant's "reveal that card, put it into your hand", which
    -- CR 702.29e's typecycling also says.
    --
    -- The reveal is named in the constructor because CR 701.23e makes it part of
    -- the card's own instruction. A search-to-hand that stays private (Demonic
    -- Tutor) is a DIFFERENT sentence and gets its own arm, not this one with a
    -- flag.
    RevealThenHand
  deriving (Eq, Ord, Show)
