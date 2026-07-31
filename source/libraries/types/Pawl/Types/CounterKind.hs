module Pawl.Types.CounterKind where

import Pawl.Types.Keyword (Keyword)

-- CR 122: a counter is a marker that modifies characteristics or interacts with a
-- rule (CR 122.1). Its KIND is a closed-half classification -- the same posture as
-- Keyword (a citation, not an effect identity): the rules core reads counts by
-- kind (the P/T contribution in CR 613.4c; the CR 704.5q annihilation SBA) and
-- never cases on a card. The two P/T-modifying kinds are CR 122.1a and the keyword
-- kind is CR 122.1b and the loyalty kind is CR 122.1e; charge/shield/stun counters
-- (the rest of CR 122.1c-i) are future.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
data CounterKind
  = PlusOnePlusOne -- CR 122.1a: +1/+1
  | MinusOneMinusOne -- CR 122.1a: -1/-1
  | -- CR 122.1b: "A keyword counter on a permanent ... causes that object to gain
    -- that keyword." Carries the keyword rather than one constructor per keyword,
    -- because the rule's own list is fifteen entries plus "any variants of those
    -- keywords" -- and because Keyword is already the closed-half classification
    -- this would otherwise duplicate.
    --
    -- CR 122.1b's list is NOT enforced here. It names flying, first strike,
    -- double strike, deathtouch, decayed, exalted, haste, hexproof,
    -- indestructible, lifelink, menace, reach, shadow, trample and vigilance --
    -- so `Keyword Flying` is legal and `Keyword Defender` is not a counter any
    -- card can print. Making that unrepresentable would mean a second keyword
    -- enumeration to keep in step with CR 702; the card data is where the
    -- restriction lives, as it does for every other "no card prints this"
    -- constraint in the open half.
    --
    -- CR 613.1f is the layer: this grants an ability, so Projection gathers it at
    -- Layer.Ability, NOT at the layer 7c where CR 122.1a's P/T counters land.
    Keyword Keyword
  | -- CR 122.1e: "The number of loyalty counters on a planeswalker on the
    -- battlefield indicates how much loyalty it has." CR 306.5c says the same
    -- thing from the other side, which is why a planeswalker's loyalty on the
    -- battlefield is a count here and never a Pawl.Types.Loyalty -- that type
    -- carries only CR 306.5a's PRINTED number.
    --
    -- Contributes nothing to the CR 613 layer system: no layer reads loyalty, so
    -- Pawl.Engine.Projection.counterGathered grants nothing for this kind, unlike
    -- CR 122.1a's P/T counters (layer 7c) and CR 122.1b's keyword counters
    -- (layer 6). What reads it instead is CR 704.5i's state-based action and CR
    -- 606.6's activation gate, both of which count Object.counters directly.
    Loyalty
  deriving (Eq, Ord, Show)
