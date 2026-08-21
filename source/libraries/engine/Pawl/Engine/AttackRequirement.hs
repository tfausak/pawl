-- CR 508.1d / 613.11: the continuous effects that REQUIRE an attack. The twin
-- of Pawl.Engine.BlockRequirement on the other side of the combat phase, and
-- one of the modules on the axis CR 613.11 reaches past the layer system. None
-- is a layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.AttackRequirement, of
-- Pawl.Types.ActiveAttackRequirement and of Object.goadedBy -- the printed
-- carrier, the stored one and CR 701.15b's designation, where
-- Pawl.Engine.BlockRequirement has only the first two. Pawl.Engine.Combat asks
-- for requirement INSTANCES -- bare (attacker, target) pairs -- and never learns
-- which card produced one, nor which carrier.
module Pawl.Engine.AttackRequirement where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 508.1d: every requirement in force right now, INSTANTIATED as the
-- (creature, target) pairs the active player is required to declare, each
-- carrying HOW MANY requirements name it.
--
-- Keyed by the pair, because CR 508.1d counts requirements being OBEYED by a
-- particular declaration, and a declaration obeys an instance exactly when it
-- attacks with that creature and announces that target for it (CR 508.1b) -- so
-- each one must stay identifiable against the declaration it is checked against.
-- Per CREATURE, not per ability: CR 508.1d checks each creature the active
-- player controls, so one Curse over three able creatures is three requirements.
--
-- A MULTISET and not a set, because the rule counts requirements rather than
-- creatures: a Berserkers of Blood Ridge ("this creature attacks each combat if
-- able") under a Curse of the Nightly Hunt is two requirements on that creature
-- and one on each other creature the cursed player controls, so under a Silent
-- Arbiter's bound of one only the Berserkers attains the maximum. Proved by
-- boundedDeclarationSpec's "two requirements on ONE creature count twice".
-- BlockRequirement.instances is the twin, on its own pairs.
--
-- The multiplicity counts DISTINCT REQUIREMENTS, not gathering events: the
-- battlefield walk is over a Set, `candidates` and `targets` are duplicate-free
-- (Combat filters the first from that same Set and derives the second from
-- Combat.attackTargets), and fromRequirement filters `candidates` once per
-- requirement -- so a pair is emitted exactly once per requirement naming it,
-- and two Curses are two sources and therefore two emissions.
--
-- `candidates` is CR 508.1a's chosen-from set, and it carries the "if able" of
-- "attacks each combat if able". `targets` is CR 508.1b's announcement list,
-- every one of which any candidate may legally be announced as attacking --
-- pawl models no restriction on WHAT a creature attacks, only on whether it
-- attacks. Both are passed IN rather than computed here, as BlockRequirement
-- takes its `able` predicate and its `attackers`, so this module never learns
-- the restrictions.
--
-- A requirement that does NOT name its object mints one pair per target, which
-- is Curse of the Nightly Hunt's "attack each combat if able" and the posture
-- BlockRequirement.instances takes for an absent axis. CR 508.1a caps how many
-- of those one creature can obey at one -- a creature attacks a single target --
-- so attacking anything attains that requirement's maximum.
--
-- What the instance multiset is NOT is CR 508.1d's maximum. Its total is an
-- upper bound on it, and Combat.attackCeiling is what turns the bound into the
-- number: a creature restricted by its declaration's SIZE (Bonded Construct) is
-- a candidate and mints an instance, yet the declaration obeying every instance
-- at once can be one no player may make. The bound and the maximum coincide on a
-- board with no such card, which is what attackCeiling's closed form exploits.
--
-- No `able` predicate BESIDE the candidate list, where the blocking twin has
-- one: there, CR 509.1b's restrictions are pairwise (flying, fear) and cannot
-- be decided per blocker. Every attacking restriction pawl models is either per
-- creature -- already inside Combat.canAttack -- or about the whole declaration,
-- which no per-creature predicate could carry either.
instances :: [ObjectId] -> [AttackTarget.AttackTarget] -> GameState -> Map (ObjectId, AttackTarget.AttackTarget) Natural
instances candidates targets gs =
  let -- Hoisted out of the walk as BlockRequirement.instances hoists them, and
      -- both unforced until some permanent actually declares a requirement.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.attackRequirements face of
          -- Every permanent in almost every game.
          [] -> []
          requirements ->
            -- The same two ability losses BlockRequirement.instances asks
            -- about: CR 305.7's basic-land subtype set, and CR 604.2 against a
            -- CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
            -- requirement that has started to apply is argued there. Why CR
            -- 613.11 also lets the CR 305.7 gate be liveAfterLayers rather than
            -- liveGiven is argued there too.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then
                -- CR 612.1's word swap over the source's own text, computed HERE
                -- rather than hoisted beside setEffs, the placement
                -- CombatRestriction.restricted argues for: textChangesAffecting
                -- folds the whole continuous-effect list, and the empty case above
                -- already turned away every permanent that prints no requirement.
                --
                -- The SOURCE's changes and not the required creature's: CR 612.1
                -- changes the words printed on THAT object, and the subject clause
                -- below is printed on the card stating the requirement.
                concatMap (fromRequirement source (Projection.textChangesAffecting source gs)) requirements
              else []
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source subject creature =
        Projection.affects
          source
          creature
          subject
          (Projection.project creature gs)
          gs
      -- CR 612.1: a hacked "Swamps attack each combat if able" requires Islands.
      --
      -- The printed carrier states no object (Pawl.Types.AttackRequirement says
      -- why), so every target is admissible and the requirement mints a pair per
      -- one.
      fromRequirement source changes requirement =
        let subject = AttackRequirement.subject requirement
         in [ (creature, target)
            | creature <- filter (named source (if null changes then subject else Projection.rewriteAffected changes subject)) candidates,
              target <- targets
            ]
      -- CR 508.1d again, off the STORED carrier. No CR 305.7 or CR 604.2 gate and
      -- no CR 612.1 rewrite, which is the posture BlockRequirement.instances takes
      -- for its stored rows: those three ask what a permanent's TEXT still says,
      -- and a resolution-created requirement has outlived its source's text (CR
      -- 611.2).
      --
      -- Pruned by membership exactly as the printed pairs are: a creature that has
      -- since left the battlefield, or that can no longer attack, is not among
      -- `candidates`, and a player who has left the game is not among `targets`
      -- (Combat.attackTargets is derived from Game.stillPlayingInOrder).
      fromStored active =
        let creature = ActiveAttackRequirement.attacker active
            target = AttackTarget.OfPlayer (ActiveAttackRequirement.defender active)
         in [ (creature, target)
            | creature `elem` candidates,
              target `elem` targets
            ]
      -- CR 701.15b, off the THIRD carrier (Object.goadedBy): a goaded creature
      -- "attacks each combat if able and attacks a player other than the
      -- controller of the permanent, spell, or ability that caused it to be
      -- goaded if able". TWO requirements per goader and not one, which is what
      -- CR 701.15c counts ("doing so creates additional combat requirements"),
      -- and neither of them is a CR 508.1c restriction: both say "if able", so
      -- CR 508.1's maximization is what decides them, and a board admitting no
      -- announcement that obeys the second leaves it unmet rather than making
      -- the declaration illegal.
      --
      -- The first mints a pair per target, the posture a requirement naming no
      -- object takes above. The second mints one per target that IS another
      -- player: attacking a planeswalker or a battle is not attacking a player
      -- (CR 508.1b lists the three separately), so it obeys the first
      -- requirement and not the second -- which is the only thing that makes the
      -- second observable, pawl choosing ONE defending player per combat -- see #175.
      --
      -- Per GOADER, since CR 701.15b names "the controller of the permanent,
      -- spell, or ability that caused it to be goaded" and two goaders exclude
      -- two different seats. Deduplication is Object.goadedBy's, by CR 701.15d.
      --
      -- No CR 305.7 or CR 604.2 gate and no CR 612.1 rewrite, fromStored's
      -- reasons: goad is a designation the game remembers rather than text a
      -- permanent still prints. Walks `candidates` rather than the battlefield
      -- because that is the pruning fromStored gets from membership -- only a
      -- creature that can attack can obey either requirement.
      fromGoad creature =
        [ pair
        | goader <- Set.toList (Goad.goadedBy creature gs),
          pair <-
            [(creature, target) | target <- targets]
              <> [(creature, target) | target <- targets, target /= AttackTarget.OfPlayer goader, isPlayer target]
        ]
      isPlayer target = case target of
        AttackTarget.OfPlayer _ -> True
        AttackTarget.OfPlaneswalker _ -> False
        AttackTarget.OfBattle _ -> False
   in Map.fromListWith
        (+)
        ( fmap
            (\pair -> (pair, 1))
            ( concatMap fromPermanent (Set.toList (GameState.battlefield gs))
                <> concatMap fromStored (GameState.attackRequirements gs)
                <> concatMap fromGoad candidates
            )
        )
