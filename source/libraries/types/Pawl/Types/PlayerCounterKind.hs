module Pawl.Types.PlayerCounterKind where

-- | CR 122.1: a marker on an object OR a player. Player counters are a DISJOINT
-- domain from object CounterKind -- object-only kinds are CR 122.1a-e,g-h,
-- player-only ones CR 122.1f,i and CR 107.14 -- so this is its own type, keeping
-- "a +1/+1 counter on a player" unrepresentable.
--
-- Like CounterKind and Keyword this is a CLASSIFICATION, not an effect identity:
-- the rules core reads counts by kind (CR 704.5c, CR 107.14) and never cases on
-- a card.
--
-- Ord is load-bearing: PlayerCounterKind is a Map key on Player.counters.
-- Constructors are ordered by rule number so the type stays diffable against the
-- rules.
data PlayerCounterKind
  = Energy -- CR 107.14
  | Poison -- CR 122.1f
  | -- | CR 122.1i. The one player counter the rules attach a whole ABILITY to:
    -- "one or more rad counters on a player cause a triggered ability to trigger
    -- at the beginning of that player's precombat main phase", whose text rule
    -- 728.1 writes out and Pawl.Engine.Rad mints. So unlike Experience below,
    -- this constructor is not vocabulary alone -- the closed half reads it,
    -- exactly as it reads Poison for CR 704.5c.
    Rad
  | -- | CR 122.1's bare first sentence and nothing else. The word "experience"
    -- does not appear in docs/rules.txt, and that ABSENCE is the citation: every
    -- other player counter is named by a rule that attaches behaviour to it, so
    -- since nothing in the rules reads an experience counter the closed half must
    -- not either. A pure vocabulary constructor, only ever counted by a card's
    -- own text.
    --
    -- LAST rather than sorted into place, having no rule number to sort by.
    Experience
  deriving (Eq, Ord, Show)
