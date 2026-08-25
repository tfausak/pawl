module Pawl.Types.WithCounters where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Quantity as Quantity

-- | CR 614.1c's as-enters rewrite: which counters the permanent enters with, and
-- how many of each.
--
-- A MAP BY KIND, EntryRiders.counters' shape rather than one kind and one count,
-- because a permanent's own text can name several kinds at once -- Agent's
-- Toolkit's "this artifact enters with a +1/+1 counter, a flying counter, a
-- deathtouch counter, and a shield counter on it" (#2314). One row per kind is
-- not the same sentence: each row would be its own candidate in CR 616.1's pool,
-- offering an ordering the card does not have, where CR 614.5 gives a scaling
-- replacement ONE opportunity across the whole entry. Pawl.Engine.Event's entry
-- arm therefore places every kind in one application.
--
-- The amount is a Quantity rather than a literal because CR 614.1c admits
-- "enters with a number of +1/+1 counters on it equal to ..." (Undergrowth
-- Scavenger). Pawl.Types.PutCounters, the resolution-time mirror of this
-- payload, has always been a Quantity; so is Pawl.Types.EntryRiders' count.
--
-- The EMPTY map is representable in the type and unsayable on the wire:
-- Pawl.Codec.WithCounters decodes through Common.nonEmptyKeyedList, so a card
-- file placing nothing is a decode failure rather than a row that does nothing.
-- Every engine minting site builds a singleton by construction.
newtype WithCounters = MkWithCounters
  { counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Quantity.Quantity
  }
  deriving (Eq, Ord, Show)

-- | The one-kind row every keyword and every intrinsic ability mints: vanishing's
-- time counters (CR 702.62a), fading's fade counters (CR 702.32a), modular's
-- +1/+1 counters (CR 702.4b), a Saga's lore counter (CR 714.3a), a planeswalker's
-- loyalty (CR 306.5b) and a battle's defense (CR 310.6a).
one :: CounterKind.CounterKind Keyword.Keyword -> Quantity.Quantity -> WithCounters
one kind amount = MkWithCounters (Map.singleton kind amount)
