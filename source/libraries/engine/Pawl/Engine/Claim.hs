module Pawl.Engine.Claim where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim
import qualified Pawl.Types.ClaimAxis as ClaimAxis
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
--
-- ISOLATED POOLS ARE TAKEN OUT FIRST, which is exact and not a prune. A pool
-- that meets no other pool in its axis contributes its whole size to every
-- subset it joins and its whole count to that subset's demand, so a subset
-- holds iff the isolated part holds on its own and the rest holds without it.
-- Asking every subset containing it therefore proves nothing the singleton did
-- not. What it buys is the enumeration: a board of twenty lands is twenty
-- singleton self-pools that meet nothing, and that is twenty checks rather than
-- 2^20 (#1725).
satisfiable :: [Claim] -> Bool
satisfiable claims = all satisfiableAxis (byAxis claims)

-- One axis's entries, isolated ones apart. `contested` is the objects two or
-- more entries could each take; an entry drawing on none of them is isolated.
satisfiableAxis :: [(Set.Set ObjectId, Natural)] -> Bool
satisfiableAxis entries =
  let shared = objectsWantedTwice (fmap fst entries)
      (lone, met) = List.partition (\(pool, _) -> Set.disjoint pool shared) entries
   in all (\entry -> fits [entry]) lone && all fits (List.subsequences met)
  where
    fits subset = Natural.length (Set.unions (fmap fst subset)) >= sum (fmap snd subset)

-- The objects that appear in two or more of these pools -- what makes a pool
-- meet another one. Counted rather than compared pairwise, so this is linear in
-- the pools' total size where the pairwise question is quadratic.
objectsWantedTwice :: [Set.Set ObjectId] -> Set.Set ObjectId
objectsWantedTwice pools =
  Map.keysSet
    . Map.filter (> (1 :: Natural))
    $ Map.fromListWith (+) [(oid, 1) | pool <- pools, oid <- Set.toList pool]

-- The same question asked of whole CLAIMS rather than of one axis's merged
-- pools, and asked GROUPWISE: `contested` takes the claim groups -- one per mana
-- source, plus the claims of the cost being paid -- and answers the objects two
-- or more DIFFERENT groups could each take. A group's own two claims naming one
-- object is not contention, since nothing chooses between them.
--
-- Pawl.Engine.Mana.payableResolutionsGiven is the reader: a source whose claims
-- meet no other group's is worth taking as many times as it can be, so it offers
-- one option instead of one per repeat.
contested :: [[Claim]] -> Set.Set (ClaimAxis.ClaimAxis, ObjectId)
contested groups =
  Map.keysSet
    . Map.filter (> (1 :: Natural))
    $ Map.fromListWith
      (+)
      [ (key, 1)
      | group <- groups,
        key <- Set.toList (Set.fromList [(Claim.axis claim, oid) | claim <- group, oid <- Set.toList (Claim.pool claim)])
      ]

-- Whether any of these claims draws on an object `contested` above marked.
contends :: Set.Set (ClaimAxis.ClaimAxis, ObjectId) -> [Claim] -> Bool
contends marked = any (\claim -> any (\oid -> Set.member (Claim.axis claim, oid) marked) (Claim.pool claim))

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
