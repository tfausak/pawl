module Pawl.Type.CounterKind where

import Pawl.Type.Keyword (Keyword)

-- CR 122: a counter is a marker that modifies characteristics or interacts with a
-- rule (CR 122.1). Its KIND is a closed-half classification -- the same posture as
-- Keyword (a citation, not an effect identity): the rules core reads counts by
-- kind (the P/T contribution in CR 613.4c; the CR 704.5q annihilation SBA) and
-- never cases on a card. The two P/T-modifying kinds are CR 122.1a and the keyword
-- kind is CR 122.1b; charge/loyalty/shield/stun counters (CR 122.1c-i) are future.
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
  deriving (Eq, Ord, Show)
