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
    -- CR 303.4's entry-attached move rather than a plain battlefield entry
    -- followed by CR 701.3's attach: the card enters ALREADY attached, and
    -- because the effect SPECIFIES what it will enchant, CR 303.4f never asks its
    -- controller to choose a host. A search whose filter
    -- does not name Filter.CanAttachToSubject can still reach this arm and find a
    -- card the fixed host can't legally hold; CR 303.4i then leaves it in the
    -- library, which is what Pawl.Engine.Resolve.putFound does.
    --
    -- The WHOLE of the card's two sentences, not just the first: "If this
    -- creature is still on the battlefield, put that Aura card onto the
    -- battlefield attached to it. Otherwise, reveal the Aura card and put it into
    -- your hand." One arm rather than two, because a search has one destination
    -- and the card prints one instruction -- which of its branches runs is a fact
    -- about the board at resolution (CR 608.2h), not a second thing a card could
    -- ask for. Pawl.AuraSpec's pair of Auratouched Mage cases, alike but for
    -- whether the Mage was killed in response to its own trigger, is what proves
    -- both branches.
    BattlefieldAttachedToSource
  deriving (Bounded, Enum, Eq, Ord, Show)
