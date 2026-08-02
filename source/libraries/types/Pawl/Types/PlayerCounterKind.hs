module Pawl.Types.PlayerCounterKind where

-- | CR 122: a counter is a marker on an object OR a player (CR 122.1). Player
-- counters are a DISJOINT domain from object CounterKind: CR 122 gives no kind
-- that goes on both -- +1/+1, keyword, shield, stun, finality, loyalty and
-- defense counters are object-only (CR 122.1a-e,g-h); poison, energy,
-- experience and rad counters are player-only (CR 122.1f,i; CR 107.14). So this
-- is its own type, not an extension of CounterKind: "a +1/+1 counter on a
-- player" and "a poison counter on a creature" stay unrepresentable.
--
-- Like CounterKind and Keyword this is a CLASSIFICATION (a citation), not an
-- effect identity: the rules core reads counts by kind (the CR 704.5c poison
-- SBA; the CR 107.14 energy payment) and never cases on a card.
--
-- Ord is load-bearing: PlayerCounterKind is a Map key on Player.counters.
-- Constructors are ordered by rule number so the type stays diffable against the
-- rules, matching CounterKind's and Keyword's posture. Experience and rad
-- counters (CR 122.1i) are one constructor each once a card wants them.
data PlayerCounterKind
  = Energy -- CR 107.14
  | Poison -- CR 122.1f
  deriving (Eq, Ord, Show)
