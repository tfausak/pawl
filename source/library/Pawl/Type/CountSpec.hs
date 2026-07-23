module Pawl.Type.CountSpec where

-- What a Quantity.Count counts. A first-order, analyzable classification, never
-- a predicate function -- one hand-carved variant per card, specific before
-- general.
--
-- Deliberately its own type rather than flat arms on Quantity: Quantity is a
-- small, closed numeric-tower type (CR 107.3, 208.2), and card-shaped growth
-- belongs somewhere the count/compare phase can retire WHOLESALE.
--
-- P9 built the per-object Filter this will reuse; the aggregation-and-threshold
-- concept is deferred to that later count/compare phase (#39).
--
-- The first two inhabitants read only zone membership and PRINTED card types, and
-- CreaturesDiedThisTurn reads only the event log's own snapshots -- never the LIVE
-- projection. Pawl.Quantity cannot import Pawl.Projection (Projection imports
-- Quantity), and a count evaluated inside the layer fold would recurse into the
-- fold that called it. A count over live projected state ("lands you control") is
-- not projected (#41).
data CountSpec
  = -- CR 208.2a: Tarmogoyf. The number of DISTINCT card types among the cards in
    -- every graveyard -- a count of types, not of objects.
    CardTypesInAllGraveyards
  | -- CR 608.2h: Inner Calm, Outer Strength. The size of the "you" player's hand,
    -- where "you" is supplied by the caller (the resolving spell's controller, or
    -- the object's own controller for a characteristic-defining ability).
    CardsInYourHand
  | -- CR 608.2i / 700.4: Khabál Ghoul. How many creatures DIED this turn -- were
    -- put into a graveyard FROM THE BATTLEFIELD (CR 700.4's definition). Folds
    -- GameState.events, and reads creature-ness from each event's last-known-
    -- information snapshot (CR 608.2h), never from a printed card: a token has no
    -- printed card at all (CR 111.3) and an animated land died as a creature.
    CreaturesDiedThisTurn
  deriving (Eq, Ord, Show)
