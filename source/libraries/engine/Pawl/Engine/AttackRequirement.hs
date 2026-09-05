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
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Goad as Goad
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.RequiredDefender as RequiredDefender

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
-- board with no such card, which is the case attackCeiling's search reduces to
-- when no restriction binds.
--
-- A FENCE on the KEY, because nothing else is one: attackCeiling's search is
-- greedy, and it is exact only while a requirement is a weight on ONE (creature,
-- target) pair, so that a declaration's obedience is a sum of independent
-- non-negative terms. A requirement spanning two creatures -- anything that
-- widened this key -- would break that, and -Werror would say nothing. Re-derive
-- the argument at Combat.attackCeilingGiven before widening it.
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
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      named source subject creature =
        Projection.affectsOn
          pcs
          grants
          source
          creature
          subject
          gs
      -- CR 612.1: a hacked "Swamps attack each combat if able" requires Islands.
      fromRequirement source changes requirement =
        let subject = AttackRequirement.subject requirement
         in [ (creature, target)
            | inForce source changes requirement,
              creature <- filter (named source (if null changes then subject else Projection.rewriteAffected changes subject)) candidates,
              target <- admissible source requirement
            ]
      -- CR 508.1d's OBJECT axis. An unnarrowed requirement mints a pair per
      -- announcement CR 508.1b admits, so any announcement obeys it; a narrowed
      -- one mints only the announcements against the player it names, and
      -- NOTHING at all when that player is not among `targets`. Alluring Siren's
      -- ruling is the same reading fromStored gets from membership: a
      -- requirement to attack a player who cannot be attacked this combat is
      -- obeyed by no declaration and so raises CR 508.1d's maximum by nothing.
      --
      -- No CR 612.1 rewrite, unlike the subject and the gate: RequiredDefender
      -- names its player structurally rather than by any word the source prints,
      -- so Projection.rewriteAffected has nothing to act on.
      admissible source requirement = case AttackRequirement.object requirement of
        Nothing -> targets
        Just RequiredDefender.ControllerOfAttached ->
          -- Read LIVE off the source's attachment and the host's controller (CR
          -- 613.1b's layer 2), for the reason every other field here is: the
          -- Aura can be moved and the host's controller can change, and CR
          -- 613.11 puts this effect after every layer.
          case Projection.hostOf source gs >>= \host -> Projection.controllerOf host gs of
            Nothing -> []
            Just pid -> filter (== AttackTarget.OfPlayer pid) targets
        Just RequiredDefender.OpponentWithMostLife ->
          -- CR 109.5 fixes the "your" as the source's controller, and CR 102.3
          -- its opponents; Game.opponentsOf drops a departed seat (CR 102.1), so
          -- a player who has lost is not counted into the maximum they used to
          -- lead. EVERY opponent tied for the lead, not one of them: CR 508.1d
          -- asks whether a declaration attacks a player the requirement names,
          -- and "an opponent with the most life" names each of them.
          --
          -- Life is read live off the board, as this module reads everything
          -- else: CR 613.11 puts the effect after every layer, so a life total
          -- changing between the beginning of combat and the declaration moves
          -- the requirement with it.
          --
          -- The intersection with `targets` and not a bare seat list, so a
          -- leader who is not a defending player this combat contributes no pair
          -- at all -- the same pruning ControllerOfAttached gets, and Alluring
          -- Siren's ruling read off membership.
          case Projection.controllerOf source gs of
            Nothing -> []
            Just you -> case fmap (\pid -> (pid, lifeOf pid gs)) (Game.opponentsOf you gs) of
              [] -> []
              lives ->
                let best = maximum (fmap snd lives)
                    leaders = [pid | (pid, life) <- lives, life == best]
                 in filter (\target -> any (\pid -> target == AttackTarget.OfPlayer pid) leaders) targets
      -- CR 119.1, as an Integer and not the Natural Pawl.Engine.Cost.lifeTotalOf
      -- answers with: CR 104.3b lets a total sit below zero until a state-based
      -- action sees it, and a clamp there would order two such seats alike. The
      -- default is unreachable, Game.opponentsOf naming only seats
      -- GameState.players holds.
      lifeOf :: PlayerId -> GameState -> Integer
      lifeOf pid state = maybe 0 Player.life (Map.lookup pid (GameState.players state))
      -- CR 508.1d's second shape -- "or that it attacks if some condition is
      -- met" -- read as CR 604.2's "as long as" clause and asked exactly as
      -- BlockPermission.instances asks its own `while`: a gate that does not
      -- hold mints nothing. The opposite polarity to
      -- CombatRestriction.inForce's "unless", where a gate that holds lifts the
      -- restriction.
      --
      -- Projection.fullView and not a bounded one, because CR 613.11 puts this
      -- effect after every layer, so there is no layer to bound against -- the
      -- answer Projection.abilitiesGiven gives CR 702.178a's gate for the same
      -- reason, and the reason CR 604.7's ban on last known information costs
      -- nothing here.
      --
      -- Asked once per REQUIREMENT rather than per candidate: CR 109.5 fixes the
      -- "you" inside it as the source's controller, so no candidate could change
      -- the answer. The CR 612.1 rewrite is the same one the subject gets, since
      -- both clauses are printed on the source.
      inForce source changes requirement = case AttackRequirement.while requirement of
        Nothing -> True
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.contextFor (Game.teams gs) (Projection.controllerOf source gs) (Just source))
            gs
            source
            (if null changes then condition else Projection.rewriteCondition changes condition)
      -- CR 508.1d again, off the STORED carrier. No CR 305.7 or CR 604.2 gate and
      -- no CR 612.1 rewrite, which is the posture BlockRequirement.instances takes
      -- for its stored rows: those three ask what a permanent's TEXT still says,
      -- and a resolution-created requirement has outlived its source's text (CR
      -- 611.2).
      --
      -- Pruned by membership exactly as the printed pairs are: a creature that has
      -- since left the battlefield, or that can no longer attack, is not among
      -- `candidates`, and a player who is not being attacked this combat is not
      -- among `targets` (Combat.declarableTargets walks
      -- Defender.defendingPlayers). Liveness is not re-asked here and must not
      -- be: Combat.attackableOpponents applied it once when CR 703.4h settled
      -- the designation, and a player who leaves AFTER that stays a defending
      -- player -- CR 800.4e drops the damage, not the attack.
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
      -- requirement and not the second. CR 802.2's several defending players are
      -- the other thing that makes the second observable: a goaded creature that
      -- must attack a player other than its goader now has another player to
      -- attack.
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
