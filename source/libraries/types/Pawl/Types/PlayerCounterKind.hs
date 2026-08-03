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
-- rules, matching CounterKind's and Keyword's posture. Rad counters (CR
-- 122.1i) are a constructor once a card wants them, and are NOT the same kind of
-- addition experience is: that rule hands them to rule 728, which mints an
-- inherent triggered ability out of them, so the constructor would arrive with a
-- turn-based rules attachment rather than alone (#123).
data PlayerCounterKind
  = Energy -- CR 107.14
  | Poison -- CR 122.1f
  | -- | CR 122.1's bare first sentence and nothing else: "a counter is a marker
    -- placed on an object or player that modifies its characteristics and/or
    -- interacts with a rule, ability, or effect." No rule number of its own to
    -- cite -- the word "experience" does not appear in docs/rules.txt at all --
    -- and that ABSENCE is the citation. Every other player counter is named by
    -- a rule that attaches behaviour to it: poison by CR 122.1f and the CR
    -- 704.5c state-based action, energy by CR 107.14's payment, rad by CR
    -- 122.1i and rule 728's inherent trigger. Nothing in the rules reads an
    -- experience counter, so the closed half must not either; a card's own text
    -- is the only thing that ever counts them, which is exactly why this is a
    -- pure vocabulary constructor.
    --
    -- LAST rather than sorted into place, breaking the by-rule-number order
    -- above, because it has no rule number to sort by.
    Experience
  deriving (Eq, Ord, Show)
