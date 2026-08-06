module Pawl.Types.CounterKind where

import qualified Pawl.Types.Keyword as Keyword

-- | CR 122.1: a marker that modifies characteristics or interacts with a rule.
-- Its KIND is a closed-half classification, the same posture as Keyword: the
-- rules core reads counts by kind (CR 613.4c, the CR 704.5q SBA) and never cases
-- on a card. CR 122.1a for the P/T kinds, CR 122.1b for keyword, CR 122.1e for
-- loyalty, and rule 714 for lore -- which rule 122.1 never lists at all. The rest
-- of CR 122.1c-i are future.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
data CounterKind
  = PlusOnePlusOne -- CR 122.1a: +1/+1
  | MinusOneMinusOne -- CR 122.1a: -1/-1
  | -- | CR 122.1b: a keyword counter causes the object to gain that keyword.
    -- Carries the keyword rather than one constructor per keyword, since Keyword
    -- is already the closed-half classification this would duplicate.
    --
    -- CR 122.1b's list of fifteen eligible keywords is NOT enforced here --
    -- `Keyword Defender` is representable and is not a counter any card can
    -- print. Enforcing it would mean a second keyword enumeration to keep in step
    -- with CR 702; the card data carries the restriction instead.
    --
    -- CR 613.1f is the layer: this grants an ability, so Projection gathers it at
    -- Layer.Ability, NOT at layer 7c where CR 122.1a's P/T counters land.
    Keyword Keyword.Keyword
  | -- | CR 122.1e / 306.5c: a planeswalker's loyalty on the battlefield is this
    -- count, never a Pawl.Types.Loyalty -- that type carries only CR 306.5a's
    -- PRINTED number.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind. What
    -- reads it is CR 704.5i's state-based action and CR 606.6's activation gate,
    -- both counting Object.counters directly.
    Loyalty
  | -- | CR 714.3: the counters a Saga tracks its progress with. Rule 122.1 gives
    -- lore counters no lettered clause of their own -- 122.1a-i never name them --
    -- so rule 714 is the whole citation. Contrast CR 122.1e, which does give
    -- loyalty a clause of its own and cross-refers rule 704 from it.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind either.
    -- What reads it is CR 714.2b's chapter trigger
    -- (TriggerCondition.SelfCountersReached), CR 714.3c's turn-based action and CR
    -- 704.5s's state-based action, all three counting Object.counters directly.
    Lore
  deriving (Eq, Ord, Show)
