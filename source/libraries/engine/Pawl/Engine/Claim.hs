module Pawl.Engine.Claim where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Extra.Natural as Natural
import Pawl.Types.Claim (Claim)
import qualified Pawl.Types.Claim as Claim
import qualified Pawl.Types.ClaimAxis as ClaimAxis
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Threshold as Threshold

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
-- HALL'S CONDITION over the subsets of one axis's claims (`countable`), which
-- is exactly necessary and sufficient where no claim carries a threshold. Every
-- object such a claim will take is interchangeable with every other object that
-- claim will take, so all its slots share a neighbourhood and the tightest
-- subset of slots is always some whole number of claims' worth. A greedy pass
-- would not do: a claim with the wider pool can eat the only object a narrower
-- one could have used. A per-claim check is the singleton subset of this one,
-- and so subsumed rather than replaced.
--
-- A THRESHOLD claim breaks that premise: only the selections reaching its total
-- serve it, so another claim can take exactly the objects it needed and leave a
-- count that still fits. Hall's condition over its fewest (`objectsOf`) stays
-- NECESSARY, and is asked first; `assignable` then settles it exactly. Heritage
-- Druid tapping the only Elf whose power reaches 3 is Pawl.ManaSpec's Synthetic
-- Muster Dynamo board, and Headless Skaab exiling the only card whose mana value
-- reaches 3 its Cryptex one.
satisfiable :: [Claim] -> Bool
satisfiable claims =
  all
    (\axisClaims -> countable (merged axisClaims) && (all (Maybe.isNothing . Claim.threshold) axisClaims || assignable axisClaims))
    (byAxis claims)

-- Hall's condition over one axis's merged entries, the isolated ones taken
-- apart: `shared` is the objects two or more entries could each take, and an
-- entry drawing on none of them meets nothing, so its own singleton check is the
-- whole of what it owes.
--
-- ISOLATED POOLS ARE TAKEN OUT FIRST, which is exact and not a prune. A pool
-- that meets no other pool in its axis contributes its whole size to every
-- subset it joins and its whole count to that subset's demand, so a subset
-- holds iff the isolated part holds on its own and the rest holds without it.
-- Asking every subset containing it therefore proves nothing the singleton did
-- not. What it buys is the enumeration: a board of twenty lands is twenty
-- singleton self-pools that meet nothing, and that is twenty checks rather than
-- 2^20; see #1725.
countable :: [(Set.Set ObjectId, Natural)] -> Bool
countable entries =
  let shared = objectsWantedTwice (fmap fst entries)
      (lone, met) = List.partition (\(pool, _) -> Set.disjoint pool shared) entries
      fits subset = Natural.length (Set.unions (fmap fst subset)) >= sum (fmap snd subset)
   in all (\entry -> fits [entry]) lone && all fits (List.subsequences met)

-- The exact question for one axis holding a threshold claim: pick each of its
-- selections, a set of objects reaching the total, and ask Hall's condition of
-- the counted claims over what the selections leave.
--
-- Over CLASSES of objects rather than the objects: two objects with the same
-- amount for every threshold and in the same counted pools are interchangeable
-- to every claim here, so a selection is a number per class. Only MINIMAL
-- selections are tried -- dropping any member falls short -- since a selection
-- holding more leaves the other claims less. One claim's selections are taken
-- in non-increasing order, its repeats being interchangeable too.
--
-- A counted claim meeting no other pool is left to `countable`, which has
-- already asked it, for that function's reason.
assignable :: [Claim] -> Bool
assignable claims =
  let shared = objectsWantedTwice (fmap Claim.pool claims)
      thresholds = [(threshold, Claim.pool claim, Claim.count claim) | claim <- claims, Just threshold <- [Claim.threshold claim]]
      counted =
        zip [0 :: Int ..]
          . Map.toList
          $ Map.fromListWith
            (+)
            [ (Claim.pool claim, Claim.count claim)
            | claim <- claims,
              Maybe.isNothing (Claim.threshold claim),
              not (Set.disjoint (Claim.pool claim) shared)
            ]
      amountFor oid (threshold, pool, _) =
        if Set.member oid pool then Just (Maybe.fromMaybe 0 (Map.lookup oid (Threshold.amounts threshold))) else Nothing
      keyOf oid = (fmap (amountFor oid) thresholds, Set.fromList [position | (position, (pool, _)) <- counted, Set.member oid pool])
      objects = Set.unions (fmap (\(_, pool, _) -> pool) thresholds <> fmap (fst . snd) counted)
      classes = zip [0 :: Int ..] (Map.toList (Map.fromListWith (+) [(keyOf oid, 1 :: Natural) | oid <- Set.toList objects]))
      initial = Map.fromList [(index, n) | (index, (_, n)) <- classes]
      -- Each threshold's candidate classes, the largest amount first.
      candidatesOf position =
        List.sortOn
          (\(_, amount) -> negate amount)
          [(index, amount) | (index, ((amounts, _), _)) <- classes, (at, Just amount) <- zip [0 :: Int ..] amounts, at == position, amount > 0]
      countedFits remaining =
        let sizeOf subset =
              sum [n | (index, ((_, members), _)) <- classes, any ((`Set.member` members) . fst) subset, n <- Maybe.maybeToList (Map.lookup index remaining)]
         in all (\subset -> sizeOf subset >= sum (fmap (snd . snd) subset)) (List.subsequences counted)
      go owed bound remaining = case owed of
        [] -> countedFits remaining
        (_, _, 0) : rest -> go rest Nothing remaining
        (position, total, k) : rest ->
          any
            (\selection -> go ((position, total, k - 1) : rest) (Just selection) (Map.unionWith (-) remaining (Map.fromList selection)))
            (filter (\selection -> maybe True (selection <=) bound) (reaching total [(index, amount, Map.findWithDefault 0 index remaining) | (index, amount) <- candidatesOf position]))
   in go [(position, Threshold.total threshold, n) | (position, (threshold, _, n)) <- zip [0 ..] thresholds] Nothing initial

-- Every MINIMAL way to reach `need` out of these classes, each an index, its
-- amount and how many it holds, the largest amount first: as a number per
-- class. Minimal because the last class taken is the one that reaches it, and
-- it holds the smallest amount taken.
reaching :: Integer -> [(Int, Integer, Natural)] -> [[(Int, Natural)]]
reaching need candidates
  | need <= 0 = [[]]
  | otherwise = case candidates of
      [] -> []
      (index, amount, held) : rest
        | sum [a * toInteger n | (_, a, n) <- candidates] < need -> []
        | otherwise ->
            let enough = div (need + amount - 1) amount
                finishing = [[(index, Integer.toNaturalSaturating enough)] | enough <= toInteger held]
                partial = do
                  taken <- [0 .. min (toInteger held) (enough - 1)]
                  completion <- reaching (need - taken * amount) rest
                  pure (if taken > 0 then (index, Integer.toNaturalSaturating taken) : completion else completion)
             in finishing <> partial

-- The objects that appear in two or more of these pools -- what makes a pool
-- meet another one. Counted rather than compared pairwise, so this is linear in
-- the pools' total size where the pairwise question is quadratic.
objectsWantedTwice :: [Set.Set ObjectId] -> Set.Set ObjectId
objectsWantedTwice pools =
  Map.keysSet
    . Map.filter (> (1 :: Natural))
    $ Map.fromListWith
      (+)
      ( do
          pool <- pools
          oid <- Set.toList pool
          pure (oid, 1)
      )

-- The same question asked of whole CLAIMS rather than of one axis's merged
-- pools, and asked GROUPWISE: the groups are one per mana source plus the claims
-- of the cost being paid, and the answer is the objects two or more DIFFERENT
-- groups could each take, keyed by the axis they take them on. A group's own two
-- claims naming one object is not contention, since nothing chooses between
-- them.
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
      ( do
          group <- groups
          key <-
            Set.toList
              ( Set.fromList
                  ( do
                      claim <- group
                      oid <- Set.toList (Claim.pool claim)
                      pure (Claim.axis claim, oid)
                  )
              )
          pure (key, 1)
      )

-- Whether any of these claims draws on an object `contested` above marked.
contends :: Set.Set (ClaimAxis.ClaimAxis, ObjectId) -> [Claim] -> Bool
contends marked = any (\claim -> any (\oid -> Set.member (Claim.axis claim, oid) marked) (Claim.pool claim))

-- How many times over could every one of these claims be met, given that each
-- repetition asks for one more of each? The pools are the same objects every
-- time, so the answer is the smallest floor(pool / claimed) over the subsets
-- `countable` walks -- and 1 when nothing is claimed at all, since a claimless
-- payment is limited by something this module cannot see. A threshold claim is
-- counted at its fewest (`objectsOf`), which OVERSTATES its repeats;
-- Pawl.Engine.Cost.uncountedCeiling caps them.
repeats :: [Claim] -> Natural
repeats claims =
  let limit subset = case sum (fmap snd subset) of
        0 -> Nothing
        wanted -> Just (div (Natural.length (Set.unions (fmap fst subset))) wanted)
   in case concatMap (Maybe.mapMaybe limit . List.subsequences . merged) (byAxis claims) of
        [] -> 1
        limits -> minimum limits

-- The same claims made `n` times over, which is what one source activated `n`
-- times contends for. Exact rather than an approximation: `merged` adds the
-- counts of claims sharing a pool, so n copies of a claim and one claim of n
-- times the count are the same question, and `assignable` takes a threshold
-- claim's count as that many selections.
scale :: Natural -> [Claim] -> [Claim]
scale n = fmap (\claim -> claim {Claim.count = n * Claim.count claim})

-- How many objects a claim takes at the least: its count, or for a threshold
-- claim that many selections of the fewest objects reaching its total.
objectsOf :: Claim -> Natural
objectsOf claim = case Claim.threshold claim of
  Nothing -> Claim.count claim
  Just threshold ->
    Claim.count claim
      * max 1 (fewestReaching (Threshold.total threshold) (fmap (\oid -> Map.findWithDefault 0 oid (Threshold.amounts threshold)) (Set.toList (Claim.pool claim))))

-- The fewest of these amounts reaching `threshold` together: the largest first,
-- until they do. Every amount, where they never do.
fewestReaching :: Integer -> [Integer] -> Natural
fewestReaching threshold amounts =
  let go reached picked = case picked of
        [] -> 0
        amount : rest
          | reached >= threshold -> 0
          | otherwise -> 1 + go (reached + amount) rest
   in go 0 (List.sortBy (flip compare) (filter (> 0) amounts))

-- The claims grouped by the axis they draw on.
byAxis :: [Claim] -> [[Claim]]
byAxis claims = Map.elems (Map.fromListWith (flip (<>)) (fmap (\claim -> (Claim.axis claim, [claim])) claims))

-- One axis's claims merged into the pools they draw on: one entry per DISTINCT
-- pool with everything claimed from it added up, at `objectsOf`.
--
-- Merging equal pools before the enumeration is exact and not a prune. A subset
-- violating Hall's condition stays violating when the claims sharing a pool with
-- one of its members join it -- same union, no smaller a demand -- so the merged
-- subsets already cover every violation, and each of them is a real subset of
-- the claims. What it buys is the size of the enumeration: it is exponential in
-- the number of distinct pools rather than in the number of activations, so a
-- source repeatable ten times still asks about one pool.
merged :: [Claim] -> [(Set.Set ObjectId, Natural)]
merged claims = Map.toList (Map.fromListWith (+) (fmap (\claim -> (Claim.pool claim, objectsOf claim)) claims))
