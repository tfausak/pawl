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
-- Twice into your graveyard"), exile (Dazzling Flameweaver\'s "conjure a random
-- card from Dazzling Flameweaver\'s spellbook into exile", Gyox, Brutal
-- Carnivora\'s "conjure X duplicates of it into exile") and the battlefield
-- (Gilt-Leaf Alchemist\'s "conjure a card named Forest onto the battlefield"),
-- which is the expensive one -- that arrival is an ENTRY and wants CR 614's
-- replacements and CR 603's triggers, where a hand, library or exile arrival is
-- no zone change at all (#2638).
data ConjureDestination
  = -- | Emporium Thopterist\'s "conjure a card named Ornithopter into your
    -- hand".
    Hand
  | -- | Toralf\'s Disciple\'s "conjure four cards named Lightning Bolt into your
    -- library, then shuffle".
    --
    -- Not implemented: a stated position. Printings state two different things,
    -- and only one of them is a thing
    -- 'Pawl.Types.LibraryPosition.LibraryPosition' could say. An END -- always
    -- the TOP: Jewel Mine Overseer\'s "conjure seven cards named Seven Dwarves on
    -- top of your library" and Pampered Loamfrill\'s "onto the top of your
    -- library". A depth, which is no end at all: Calim, Djinn Emperor\'s
    -- "seventh from the top" and Jessie Zane, Fangbringer\'s "into the top six
    -- cards of your library at random".
    --
    -- The resolver hands 'Pawl.Types.LibraryPosition.defaultValue' to every
    -- arrival, and that is the BOTTOM -- the opposite end from the one every
    -- printing above names. Nothing is red because none of those printings is in
    -- @data\/cards\/@: Jewel Mine Overseer\'s rider has to NAME the seven cards it
    -- conjured, which wants them bound to a slot (#2638), Pampered Loamfrill
    -- conjures a duplicate (#2643), and this arm\'s own producer shuffles
    -- immediately, which makes the end unobservable there. Pampered Loamfrill is
    -- the one that would OBSERVE it, since it never shuffles (#2638).
    Library
  deriving (Bounded, Enum, Eq, Ord, Show)
