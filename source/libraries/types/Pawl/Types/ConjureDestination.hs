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
-- Not implemented: exile (Dazzling Flameweaver\'s "conjure a random card from
-- Dazzling Flameweaver\'s spellbook into exile", Gyox, Brutal Carnivora\'s
-- "conjure X duplicates of it into exile"), which is an axis of its own rather
-- than a fifth arm nothing else was waiting on -- Scryfall @o:conjure o:exile@,
-- 2026-08-29, five printings name this destination and not one of them names the
-- card it conjures, so each is held out by #2643 as well and no board reaches
-- such an arm (#2653).
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
  | -- | Shellfish Scholar\'s "conjure a card named Think Twice into your
    -- graveyard" (CR 404.1).
    Graveyard
  | -- | Lam, Storm Crane Elder\'s "conjure a card named Monastery Mentor onto
    -- the battlefield" (CR 403.1).
    --
    -- The only arm that is an ENTRY: the other three put the card into a zone
    -- and stop, where this one wants CR 616.1's entry loop and the CR 603.6a
    -- trigger scan, so Pawl.Engine.Event.conjureOntoBattlefield is a road of its
    -- own rather than another argument to Pawl.Engine.Event.conjure.
    --
    -- Not implemented: a STATED status. The arrival is untapped and out of
    -- combat, which is CR 110.5b's default and what this arm\'s producer prints,
    -- but several printings state otherwise and this arm cannot carry it --
    -- Thendar, the Overminer\'s "onto the battlefield tapped" and Kari Zev, Crew
    -- of Two\'s "onto the battlefield tapped and attacking" (#2638).
    Battlefield
  deriving (Bounded, Enum, Eq, Ord, Show)
