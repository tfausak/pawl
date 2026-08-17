module Pawl.Engine.Claim where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim
import Pawl.Types.ObjectId (ObjectId)

-- CR 118.3's "fully", asked of several claims TOGETHER: is there some
-- assignment of distinct objects under which every one of them is met? Not
-- whether each, asked alone against the untouched board, could find enough.
--
-- PER AXIS (Pawl.Types.ClaimAxis), because the resources are disjoint and a claim
-- on one can never be paid out of another -- a claim on the untapped permanents
-- cannot be met out of a graveyard, and a graveyard's cards are not made scarcer
-- by tapping. A subset spanning two axes is satisfied whenever each axis's part
-- is, so grouping first loses no case and saves the enumeration.
--
-- HALL'S CONDITION over the subsets of one axis's claims, which is exactly
-- necessary and sufficient here. Every object a claim will take is
-- interchangeable with every other object that claim will take, so all its slots
-- share a neighbourhood and the tightest subset of slots is always some whole
-- number of claims' worth. A greedy pass would not do: a claim with the wider
-- pool can eat the only object a narrower one could have used. A per-claim check
-- is the singleton subset of this one, and so subsumed rather than replaced.
satisfiable :: [Claim] -> Bool
satisfiable claims = all (all fits . List.subsequences) (byAxis claims)
  where
    fits subset = Natural.length (Set.unions (fmap fst subset)) >= sum (fmap snd subset)

-- How many times over could every one of these claims be met, given that each
-- repetition asks for one more of each? The pools are the same objects every
-- time, so the answer is the smallest floor(pool / claimed) over the subsets
-- `satisfiable` walks -- and 1 when nothing is claimed at all, since a claimless
-- payment is limited by something this module cannot see.
repeats :: [Claim] -> Natural
repeats claims = case concatMap (Maybe.mapMaybe limit . List.subsequences) (byAxis claims) of
  [] -> 1
  limits -> minimum limits
  where
    limit subset = case sum (fmap snd subset) of
      0 -> Nothing
      wanted -> Just (div (Natural.length (Set.unions (fmap fst subset))) wanted)

-- The same claims made `n` times over, which is what one source activated `n`
-- times contends for. Exact rather than an approximation: `byAxis` adds the
-- counts of claims sharing a pool, so n copies of a claim and one claim of n
-- times the count are the same question.
scale :: Natural -> [Claim] -> [Claim]
scale n = fmap (\claim -> claim {Claim.count = n * Claim.count claim})

-- The claims grouped into the pools they draw on: one list per axis, and within
-- it one entry per DISTINCT pool with everything claimed from it added up.
--
-- Merging equal pools before the enumeration is exact and not a prune. A subset
-- violating Hall's condition stays violating when the claims sharing a pool with
-- one of its members join it -- same union, no smaller a demand -- so the merged
-- subsets already cover every violation, and each of them is a real subset of
-- the claims. What it buys is the size of the enumeration: it is exponential in
-- the number of distinct pools rather than in the number of activations, so a
-- source repeatable ten times still asks about one pool.
byAxis :: [Claim] -> [[(Set.Set ObjectId, Natural)]]
byAxis claims =
  fmap Map.toList
    . Map.elems
    $ Map.fromListWith
      (Map.unionWith (+))
      (fmap (\claim -> (Claim.axis claim, Map.singleton (Claim.pool claim) (Claim.count claim))) claims)
