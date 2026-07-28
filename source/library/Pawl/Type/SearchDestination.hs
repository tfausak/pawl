module Pawl.Type.SearchDestination where

-- CR 701.23: where a search puts what it finds. The searching rule itself says
-- only "look at [a] hidden zone ... to find a card"; every card that searches
-- then says what happens to the card, and those instructions differ.
--
-- Not a Pawl.Type.Zone, which is the obvious guess and is wrong twice over. A
-- zone cannot say "tapped", which Evolving Wilds needs (CR 701.23 has no such
-- clause -- "put it onto the battlefield tapped" is the card's own text), and
-- most of the zones a Zone can name have no searching card behind them. This
-- type names the WHOLE instruction, so each arm is one card's sentence.
data SearchDestination
  = -- Evolving Wilds: "Search your library for a basic land card, put it onto
    -- the battlefield tapped, then shuffle."
    BattlefieldTapped
  | -- CR 702.29e's typecycling: "Search your library for a [type] card, reveal
    -- it, and put it into your hand. Then shuffle your library." The reveal is
    -- not performed (#320).
    Hand
  deriving (Eq, Ord, Show)
