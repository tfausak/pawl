module Pawl.Types.ConjureDestination where

import qualified Pawl.Types.TapState as TapState

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
    -- conjured, which wants them bound to a slot (#3971), Pampered Loamfrill\'s
    -- "the duplicate perpetually gets +1\/+1 and gains deathtouch" names its own
    -- the same way and wants the same slot, and this arm\'s own producer shuffles
    -- immediately, which makes the end unobservable there. Pampered Loamfrill is
    -- the one that would OBSERVE it, since it never shuffles (#3972).
    Library
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
    -- The 'Pawl.Types.TapState.TapState' is CR 110.5b's status, STATED by the
    -- effect rather than carried by the arriving card -- Foundry
    -- Groundbreaker\'s "conjure two cards named Mishra\'s Foundry onto the
    -- battlefield tapped" and Thendar, the Overminer\'s "onto the battlefield
    -- tapped". CR 110.5b\'s untapped is the default, which Lam prints and the
    -- codec elides.
    --
    -- On the ARM rather than beside 'Pawl.Types.Conjure.destination', which is
    -- what makes a conjure into a hand, a library, a graveyard or exile unable to
    -- state one: CR 110.5d gives a card outside the battlefield no status at all,
    -- so a field there would be a key four of the five destinations could write and
    -- nothing could read. 'Pawl.Types.EntryRiders.EntryRiders', what the other
    -- three entry doors carry, is not what this arm holds: beyond the status and
    -- the combat state below, its riders are counters, two kinds of
    -- face-downness, transformation, attachment, blocking and CR 110.2a\'s
    -- controller, and no printed conjure states one.
    --
    -- Not implemented: CR 508.4\'s combat state, which two printings state
    -- beside the status -- Stormforged Armor\'s equipped creature "conjure a card
    -- named Ball Lightning onto the battlefield tapped and attacking", and Kari
    -- Zev, Crew of Two\'s same sentence, whose rider then names the conjured card
    -- and so wants the binding see #3971 as well (#3973).
    Battlefield TapState.TapState
  | -- | Smog Smasher\'s "conjure a duplicate of target nontoken creature into
    -- exile" (CR 406.1).
    --
    -- Not implemented: Dazzling Flameweaver\'s "you may play that card until
    -- the end of your next turn", Darigaaz, Shivan Champion\'s "face down with
    -- three egg counters on it", Gyox, Brutal Carnivora\'s "those duplicates
    -- perpetually get +X\/+X" and Limitless Rekindling\'s "you may cast that
    -- card" each name the card this arm exiled in a later clause, which wants the
    -- slot the conjure does not bind (#3971).
    Exile
  deriving (Eq, Ord, Show)
