module Pawl.Types.CounterKind where

-- | CR 122.1: a marker that modifies characteristics or interacts with a rule.
-- Its KIND is a closed-half classification, the same posture as Keyword: the
-- rules core reads counts by kind (CR 613.4c, the CR 704.5q SBA) and never cases
-- on a card. CR 122.1a for the P/T kinds, CR 122.1b for keyword, CR 122.1e for
-- loyalty, rule 714 for lore, CR 702.63 for time and CR 702.32 for fade -- none of
-- which rule 122.1 lists at all, and CR 122.1c for shield. What rule 122.1 names and this
-- type does not are CR 122.1d's stun counter and CR 122.1h's finality counter;
-- 122.1f's poison and 122.1i's rad are a PLAYER's and live in
-- Pawl.Types.PlayerCounterKind.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
--
-- PARAMETRIC in the keyword, for the reason Pawl.Types.Filter is and only that
-- reason: CR 122.1b's arm below names a keyword, and Pawl.Types.Keyword already
-- names Filter, which now names this type (Filter.HasCounters). Every module but
-- Filter writes the single application `CounterKind Keyword`.
data CounterKind keyword
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
    Keyword keyword
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
  | -- | CR 122.1g / 310.4c: a battle's defense on the battlefield is this count,
    -- never a Pawl.Types.Defense -- that type carries only CR 310.4a's PRINTED
    -- number. The exact twin of Loyalty above, down to the rule that puts the
    -- permanent into its owner's graveyard when the count reaches 0.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind.
    --
    -- Three readers, and unlike Loyalty's they are the whole of the count's story:
    -- CR 310.6 / 120.3h takes counters off in Pawl.Engine.Damage, CR 310.11b's
    -- intrinsic Siege ability fires when the last one goes, and CR 704.5v buries a
    -- battle sitting at 0 that owes no ability. All three count Object.counters
    -- directly, as loyalty's and lore's readers do.
    Defense
  | -- | CR 702.63a: the counters vanishing counts down. Rule 122.1 gives time
    -- counters no lettered clause either -- 122.1a-i never name them -- so rule
    -- 702.63 is the whole citation, exactly as rule 714 is Lore's.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind. Its
    -- readers are vanishing's own three abilities, minted by
    -- Pawl.Engine.Keyword, which count Object.counters directly.
    Time
  | -- | CR 702.32a: the counters fading counts down. Rule 122.1 gives fade
    -- counters no lettered clause either, so rule 702.32 is the whole citation,
    -- exactly as rule 702.63 is Time's.
    --
    -- A KIND OF ITS OWN rather than reusing Time, even though both count a
    -- permanent's remaining upkeeps: the rules name them apart, and a card reads
    -- one and not the other -- Clockspinning names time counters, Jolting Merfolk
    -- and Ancient Hydra pay fade counters as an activation cost. A permanent with
    -- both keywords would count one pile twice if they shared a kind.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind. Its
    -- reader is fading's own upkeep ability, minted by Pawl.Engine.Keyword, which
    -- counts Object.counters directly.
    Fade
  | -- | CR 122.1c: shield counters on a permanent create one replacement effect
    -- and one prevention effect that protect it. Unlike every kind above, what
    -- the count does is not read by a rule that counts it: the pair is MINTED
    -- from the presence of any such counter (Pawl.Engine.Projection.shieldOf),
    -- and applying either takes one counter off, so the count is how many times
    -- the pair may still be applied rather than a number any rule reads.
    --
    -- Contributes nothing to the CR 613 layer system, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind -- a
    -- shield counter is not CR 122.1b's indestructible counter and grants no
    -- keyword. What the rule does with it is replace two events, and CR 614/615
    -- is where that lives.
    Shield
  deriving (Eq, Ord, Show)
