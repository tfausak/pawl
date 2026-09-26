module Pawl.Types.ConjureDestination where

import qualified Pawl.Types.ConjureEntry as ConjureEntry
import qualified Pawl.Types.LibraryDepth as LibraryDepth

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
data ConjureDestination
  = -- | Emporium Thopterist\'s "conjure a card named Ornithopter into your
    -- hand".
    Hand
  | -- | Toralf\'s Disciple\'s "conjure four cards named Lightning Bolt into your
    -- library, then shuffle" names no place, so Nothing, and the card lands at
    -- 'Pawl.Types.LibraryPosition.defaultValue' for the shuffle to move. Calim,
    -- Djinn Emperor\'s "seventh from the top" and Mine Security\'s "into the top
    -- eight cards of your library at random" state a depth, and "on top of your
    -- library" (Jewel Mine Overseer) is @FromTop 1@.
    Library (Maybe LibraryDepth.LibraryDepth)
  | -- | Shellfish Scholar\'s "conjure a card named Think Twice into your
    -- graveyard" (CR 404.1).
    Graveyard
  | -- | Lam, Storm Crane Elder\'s "conjure a card named Monastery Mentor onto
    -- the battlefield" (CR 403.1).
    --
    -- The only arm that is an ENTRY: the other four put the card into a zone
    -- and stop, where this one wants CR 616.1's entry loop and the CR 603.6a
    -- trigger scan, so Pawl.Engine.Event.conjureOntoBattlefield is a road of its
    -- own rather than another argument to Pawl.Engine.Event.conjure.
    --
    -- The 'Pawl.Types.ConjureEntry.ConjureEntry' is what the sentence states
    -- about the arrival: CR 110.5b's status (Foundry Groundbreaker\'s "onto the
    -- battlefield tapped") and CR 508.4's combat state (Kari Zev, Crew of
    -- Two\'s "tapped and attacking").
    --
    -- On the ARM rather than beside 'Pawl.Types.Conjure.destination', which is
    -- what makes a conjure into a hand, a library, a graveyard or exile unable to
    -- state one: CR 110.5d gives a card outside the battlefield no status at all,
    -- so a field there would be a key four of the five destinations could write and
    -- nothing could read.
    Battlefield ConjureEntry.ConjureEntry
  | -- | Smog Smasher\'s "conjure a duplicate of target nontoken creature into
    -- exile" (CR 406.1).
    --
    -- A later clause names the card this arm exiled through
    -- 'Pawl.Types.Conjure.slot' -- Dazzling Flameweaver\'s "you may play that
    -- card until the end of your next turn".
    Exile
  deriving (Eq, Ord, Show)
