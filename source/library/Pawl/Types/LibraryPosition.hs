module Pawl.Types.LibraryPosition where

-- | CR 401.2's "single face-down pile" is an ORDERED one -- players "can't ...
-- change the order of cards in a library" -- so a card put into a library
-- arrives at a position. Griptide's "on top of its owner's library" is the
-- pool's producer for the end that is not the default.
--
-- Its own type rather than a pair of Zone constructors: a Zone is a zone (CR
-- 400.1), and only a library has ends -- a `Zone.LibraryTop` would make every
-- exhaustive case over the seven zones answer a question about libraries twice.
--
-- Two-valued, and STAYS two-valued: "the object's owner chooses which end"
-- (Aetherspouts) is Pawl.Types.LibraryPlacement's job, so that every consumer of
-- this type -- Pawl.Engine.Game.insertIntoZone above all -- gets an end that is
-- already settled.
--
-- Not a Bool, for the reason Regenerability and TapState are not: at a call site
-- `insertIntoZone Zone.Library Top` says which end without a comment.
data LibraryPosition
  = Top
  | Bottom
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The position a move that says nothing about one uses.
--
-- BOTTOM, because that is what every library arrival in the tree did before a
-- position could be stated, and two of them still OBSERVE it: CR 103.5's
-- mulligan puts the returned cards on the bottom "in any order" through this
-- same funnel, which Pawl.MulliganSpec pins in both the count and the order, and
-- Pawl.TargetSpec's Riftsweeper control reads a bottomed arrival as the
-- unshuffled comparand for CR 701.24a. Flipping this default fails all three.
-- A card that means the top says so.
defaultValue :: LibraryPosition
defaultValue = Bottom
