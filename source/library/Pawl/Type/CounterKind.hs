module Pawl.Type.CounterKind where

-- CR 122: a counter is a marker that modifies characteristics or interacts with a
-- rule (CR 122.1). Its KIND is a closed-half classification -- the same posture as
-- Keyword (a citation, not an effect identity): the rules core reads counts by
-- kind (the P/T contribution in CR 613.4c; the CR 704.5q annihilation SBA) and
-- never cases on a card. Only the two P/T-modifying kinds exist at M4f (CR 122.1a);
-- keyword/charge/loyalty/poison/shield/stun counters (CR 122.1b-i) are future.
-- Ord is load-bearing: CounterKind is a Map key on Object.counters.
data CounterKind
  = PlusOnePlusOne -- CR 122.1a: +1/+1
  | MinusOneMinusOne -- CR 122.1a: -1/-1
  deriving (Eq, Ord, Show)
