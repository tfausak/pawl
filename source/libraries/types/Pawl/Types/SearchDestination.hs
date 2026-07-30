module Pawl.Types.SearchDestination where

-- CR 701.23: where a search puts what it finds. The searching rule itself says
-- only "look at [a] hidden zone ... to find a card"; every card that searches
-- then says what happens to the card, and those instructions differ.
--
-- Not a Pawl.Types.Zone, which is the obvious guess and is wrong twice over. A
-- zone cannot say "tapped", which Evolving Wilds needs (CR 701.23 has no such
-- clause -- "put it onto the battlefield tapped" is the card's own text), and
-- most of the zones a Zone can name have no searching card behind them. This
-- type names the WHOLE instruction, so each arm is one card's sentence.
data SearchDestination
  = -- Evolving Wilds: "Search your library for a basic land card, put it onto
    -- the battlefield tapped, then shuffle."
    BattlefieldTapped
  | -- Braidwood Sextant: "Search your library for a basic land card, reveal that
    -- card, put it into your hand, then shuffle." CR 702.29e's typecycling says
    -- the same sentence in the rulebook's words ("reveal it, and put it into
    -- your hand").
    --
    -- The reveal is named in the constructor because it is named in the card,
    -- and CR 701.23e is why that has to be so: "If the effect that contains the
    -- search instruction doesn't also contain instructions to reveal the found
    -- card(s), then they're not revealed." A search-to-hand that stays private
    -- (Demonic Tutor) is a DIFFERENT sentence and gets a different arm when a
    -- card in the pool prints it -- it is not this one with a flag.
    RevealThenHand
  deriving (Eq, Ord, Show)
