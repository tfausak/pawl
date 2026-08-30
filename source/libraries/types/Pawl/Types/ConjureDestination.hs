module Pawl.Types.ConjureDestination where

-- | Where an Alchemy conjure puts the card it creates.
--
-- Conjure is a DIGITAL-ONLY keyword action and is in no rule of the CR --
-- @docs\/rules.txt@ does not contain the word -- so the authority for this type
-- is the printed sentence rather than a rule number: every card that conjures
-- says where the card goes, and this is that half of the sentence.
--
-- Not a 'Pawl.Types.Zone.Zone', for 'Pawl.Types.SearchDestination''s reason:
-- most zones a Zone can name have no conjuring card behind them, and an
-- exhaustive case over the seven would be answering about zones no printing
-- reaches.
--
-- Not implemented: a graveyard (Shellfish Scholar\'s "conjure a card named Think
-- Twice into your graveyard") and the battlefield (Gilt-Leaf Alchemist\'s
-- "conjure a card named Forest onto the battlefield"), which is the expensive
-- one -- that arrival is an ENTRY and wants CR 614's replacements and CR 603's
-- triggers, where a hand or library arrival is no zone change at all (#2638).
data ConjureDestination
  = -- | Emporium Thopterist\'s "conjure a card named Ornithopter into your
    -- hand".
    Hand
  | -- | Toralf\'s Disciple\'s "conjure four cards named Lightning Bolt into your
    -- library, then shuffle".
    --
    -- No position rides the arm. Every printing that states one states it as its
    -- own sentence-worth of detail -- Calim, Djinn Emperor\'s "into your library
    -- seventh from the top" and Jessie Zane, Fangbringer\'s "into the top six
    -- cards of your library at random" -- and neither is an END of the library,
    -- which is all 'Pawl.Types.LibraryPosition.LibraryPosition' can say. The
    -- producer here shuffles immediately afterwards, so where in the library the
    -- card lands is unobservable (gap #2638).
    Library
  deriving (Bounded, Enum, Eq, Ord, Show)
