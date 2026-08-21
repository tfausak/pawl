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
  | -- | Hoarding Dragon's "exile it". No reveal: CR 701.23e leaves a found card
    -- unrevealed unless the card says otherwise, and this sentence does not --
    -- what makes the card public afterwards is CR 400.2's exile zone, not a
    -- reveal, so the two are not the same act and this arm is not RevealThenHand
    -- pointed at another zone.
    Exile
  | -- | Auratouched Mage's "put that Aura card onto the battlefield attached to
    -- it", where "it" is the searching ability's own source.
    --
    -- CR 303.4i's entry-attached move rather than a plain battlefield entry
    -- followed by CR 701.3's attach: the card enters ALREADY attached, so CR
    -- 303.4f never asks its controller to choose a host. A search whose filter
    -- does not name Filter.CanAttachToSubject can still reach this arm and find a
    -- card the fixed host can't legally hold; CR 303.4i then leaves it in the
    -- library, which is what Pawl.Engine.Resolve.putFound does.
    --
    -- Not implemented: Auratouched Mage's "Otherwise, reveal the Aura card and
    -- put it into your hand", the branch for a source that has left the
    -- battlefield by the time the ability resolves (#2027).
    BattlefieldAttachedToSource
  deriving (Bounded, Enum, Eq, Ord, Show)
