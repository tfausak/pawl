-- CR 508.1d / 509.1c: the shape both combat requirement gatherers answer in, and
-- the one thing Pawl.Engine.Combat's two maximizations have in common. Neither
-- rule is about a card: both count REQUIREMENTS a declaration obeys, and this
-- module is that count and nothing else.
--
-- Parametric in the declaration's atom -- a (blocker, attacker) pair on
-- Pawl.Engine.BlockRequirement's side, a (creature, attack target) pair on
-- Pawl.Engine.AttackRequirement's -- because the counting argument is the same on
-- both and the atom is the only difference.
module Pawl.Engine.Requirement where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)

-- CR 508.1d / 509.1c's requirements in force, instantiated against the atoms a
-- declaration is made of, in the TWO arities the printings use
-- (Pawl.Types.RequirementArity).
--
-- `pairs` is one requirement per atom, weighted by how many name it -- Lure over
-- three able creatures is three requirements, and a Razorgrass Screen under a
-- Lure is two on one pair.
--
-- `groups` is one requirement over a SET of atoms, obeyed by a declaration
-- containing ANY of them -- Gaea's Protector's "this creature must be blocked if
-- able", Seeker of Slaanesh's "each opponent must attack with at least one
-- creature each combat if able". Keyed by the set, so two permanents stating the
-- same group are one shape carrying two: that is what keeps the attacking
-- maximization's cost in the number of DISTINCT groups rather than in the number
-- of permanents.
data Instances key = MkInstances
  { pairs :: Map key Natural,
    groups :: Map (Set key) Natural
  }
  deriving (Eq, Ord, Show)

-- Both multisets built at once, from one gathering walk's two outputs.
--
-- An EMPTY group is dropped, which is the "if able" of "must be blocked if
-- able": a requirement no declaration can obey raises the maximum by nothing.
-- Dropping it changes no answer -- 'met' would never count it either -- but it
-- keeps 'vacuous' honest, so a board where nothing is able skips the search
-- entirely, the pruning posture Pawl.Engine.BlockRequirement.instances takes
-- with its `able` predicate. Mutating the drop away leaves the Combat subtree
-- green, and this states why.
gather :: (Ord key) => [key] -> [Set key] -> Instances key
gather atoms sets =
  MkInstances
    { pairs = Map.fromListWith (+) (fmap (\atom -> (atom, 1)) atoms),
      groups = Map.fromListWith (+) [(set, 1) | set <- sets, not (Set.null set)]
    }

-- Is nothing in force at all? The board almost every game is played on, and what
-- lets both maximizations skip their search entirely.
vacuous :: Instances key -> Bool
vacuous instances = Map.null (pairs instances) && Map.null (groups instances)

-- CR 508.1d / 509.1c: how many requirements this declaration obeys. Summing
-- multiplicities rather than counting keys, because both rules count
-- REQUIREMENTS: two naming one atom are both obeyed by declaring it.
met :: (Ord key) => Instances key -> Set key -> Natural
met instances declaration =
  sum (Map.filterWithKey (\atom _ -> Set.member atom declaration) (pairs instances))
    + groupsMet instances declaration

-- The group half of 'met' alone, which is what a search that tracks the two
-- halves separately adds as it pins a witness.
groupsMet :: (Ord key) => Instances key -> Set key -> Natural
groupsMet instances declaration =
  sum (Map.filterWithKey (\set _ -> not (Set.disjoint set declaration)) (groups instances))

-- The most the groups NOT yet obeyed could still add. An admissible bound for a
-- search that prunes on one, since obeying a group is worth its weight once
-- however many of its atoms the declaration ends up containing.
unobeyed :: (Ord key) => Instances key -> Set (Set key) -> Natural
unobeyed instances obeyed =
  sum (Map.filterWithKey (\set _ -> not (Set.member set obeyed)) (groups instances))

-- The groups any of these atoms would newly obey, as the sets themselves so a
-- caller can accumulate the obeyed ones without an index.
covering :: (Ord key) => Instances key -> Set (Set key) -> [key] -> [(Set key, Natural)]
covering instances obeyed atoms =
  Map.toList
    ( Map.filterWithKey
        (\set _ -> not (Set.member set obeyed) && any (\atom -> Set.member atom set) atoms)
        (groups instances)
    )
