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
-- Not implemented: every destination but the hand -- a library (Caldera
-- Breaker, Case of the Lost Witness), a graveyard (Shellfish Scholar) and the
-- battlefield (Gilt-Leaf Alchemist), which a card states as often as this one
-- (#2638).
data ConjureDestination
  = -- | Emporium Thopterist\'s "conjure a card named Ornithopter into your
    -- hand".
    Hand
  deriving (Bounded, Enum, Eq, Ord, Show)
