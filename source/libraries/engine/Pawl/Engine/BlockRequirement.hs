-- CR 509.1c / 613.11: the continuous effects that REQUIRE a block. One of the
-- modules on the axis CR 613.11 reaches past the layer system, alongside
-- Pawl.Engine.PlayerEffect (which answers rules questions about a PLAYER).
-- Neither is a layer, and Pawl.Engine.Projection sees neither.
--
-- The only reader of Pawl.Types.BlockRequirement and of
-- Pawl.Types.ActiveBlockRequirement -- the printed carrier and the stored one,
-- as Pawl.Engine.PlayerEffect reads its own pair. Pawl.Engine.Combat asks for
-- requirement INSTANCES -- bare (blocker, attacker) pairs -- and never learns
-- which card produced one, nor which carrier.
module Pawl.Engine.BlockRequirement where

import qualified Data.Set as Set
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Requirement as Requirement
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.RequirementArity as RequirementArity

-- CR 509.1c: every requirement in force right now, INSTANTIATED against the
-- (blocker, attacker) pairs the defending player may declare -- as a weight on
-- one pair where the sentence states a requirement per creature, and as a SET of
-- pairs where it states one over them all.
--
-- Keyed by the pair, because CR 509.1c counts requirements being OBEYED by a
-- particular declaration, and a declaration obeys an instance exactly when it
-- has that blocker blocking that attacker -- so each one must stay identifiable
-- against the declaration it is checked against. Per BLOCKER, not per ability:
-- CR 509.1c checks each creature the defending player controls, so one Lure
-- over three able creatures is three requirements.
--
-- A MULTISET and not a set, because the rule counts requirements rather than
-- pairs: a Razorgrass Screen ("this creature blocks each combat if able") under
-- a Lure aimed at one of two attackers is two requirements on that pair and one
-- on the other, so blocking the Lured attacker obeys two and blocking the plain
-- one obeys a single requirement and is illegal. Proved by blockRequirementSpec's
-- "two requirements on ONE pair count twice".
--
-- The multiplicity counts DISTINCT REQUIREMENTS, not gathering events: every
-- carrier is walked once (the battlefield is a Set and the stored rows are their
-- own carriers), and within one of them `attackers` and `candidates` are
-- duplicate-free, so a pair is emitted once per requirement that names it.
--
-- The GROUP half is Pawl.Types.RequirementArity.AnySubject -- "this creature
-- must be blocked if able", one requirement obeyed by any single able blocker.
-- It is a set of pairs and not a weight because CR 509.1c counts it once
-- however many creatures block, and Combat.blockCeilingGiven's search reads it
-- that way. Combat.bestBlockDeclaration is where widening either half is
-- re-derived: its bound assumes a pair is worth its weight to whichever blocker
-- takes it and a group is worth its weight once.
--
-- `able` is the caller's CR 509.1b restriction check, which is what Lure's
-- "able to block" means. Passed IN rather than computed here, so this module
-- never learns the restrictions. Pruning by it changes no answer -- a pair that
-- disobeys a restriction is obeyed by no legal declaration -- but it keeps the
-- maximization's search space to the pairs that can actually happen.
--
-- `candidates` is CR 509.1a's chosen-from set and `attackers` the attacking
-- creatures. Both are handed in because they are the defending player's, and
-- only the caller knows who that is.
instances ::
  (ObjectId -> ObjectId -> Bool) ->
  [ObjectId] ->
  [ObjectId] ->
  GameState ->
  Requirement.Instances (ObjectId, ObjectId)
instances able candidates attackers gs =
  let -- Hoisted out of the walk as PlayerEffect.applying hoists them, and both
      -- unforced until some permanent actually declares a requirement.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> ([], [])
        Just face -> case Face.blockRequirements face of
          -- Every permanent in almost every game.
          [] -> ([], [])
          requirements ->
            -- TWO ability losses, the same pair PlayerEffect.applying asks
            -- about.
            --
            -- CR 305.7: a land whose subtype has been SET to a basic type loses
            -- its rules-text abilities, this one included.
            --
            -- CR 604.2: a static ability's continuous effect is active only
            -- while the permanent remains on the battlefield and HAS the
            -- ability, so a CR 613.1f layer-6 removal takes this one with it
            -- (Humility on Prized Unicorn). CR 613.6's rescue for an effect
            -- that has started to apply cannot reach it: CR 613.11 applies a
            -- requirement after every layer, so there is no layer for it to
            -- have started in. Nor could another part of the same card's text
            -- have started on its behalf -- a requirement is its OWN carrier,
            -- never part of a StaticAbility, so CR 613.6 has nothing here to
            -- hold together. The cut is unconditional.
            --
            -- CR 613.11's placement is also why the CR 305.7 gate is
            -- liveAfterLayers: the projection is finished here, so the setter's
            -- affected set is read against it, exactly as `named` below reads it
            -- for the subject set.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then
                -- CR 612.1's word swap over the source's own text, computed HERE
                -- rather than hoisted beside setEffs, the placement
                -- CombatRestriction.restricted argues for: textChangesAffecting
                -- folds the whole continuous-effect list, and the empty case above
                -- already turned away every permanent that prints no requirement.
                --
                -- The SOURCE's changes and not the attacker's: CR 612.1 changes the
                -- words printed on THAT object, and the attacker clause below is
                -- printed on the card stating the requirement.
                mconcat (fmap (fromRequirement source (Projection.textChangesAffecting source gs)) requirements)
              else ([], [])
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of
      -- Projection.affects's callers inside the layer fold, which read
      -- characteristics as of their own layer.
      -- Asked of both axes: an Affected is an Affected whether it names the
      -- attacker to be blocked or the creatures required to block it.
      named source clause creature =
        Projection.affectsOn
          pcs
          grants
          source
          creature
          clause
          gs
      -- CR 612.1: a hacked "all creatures able to block Swamps do so" lures
      -- blockers onto Islands. Both of CR 509.1c's axes are rewritten, because
      -- they are two halves of one sentence printed on the source's card --
      -- CombatRestriction.cantBeBlockedBy makes the same argument for its pair.
      --
      -- An absent clause is UNRESTRICTED on that axis, not empty: Nothing on the
      -- subject is Lure's "all creatures able to", and Nothing on the attacker is
      -- Razorgrass Screen's "each combat", which mints one pair per attacker the
      -- Screen may legally block. CR 509.1a caps how many of those it can obey at
      -- one, so blocking either attains the maximum.
      fromRequirement source changes requirement =
        let rewrite clause = if null changes then clause else Projection.rewriteAffected changes clause
            narrow field these = case field requirement of
              Nothing -> these
              Just clause -> filter (named source (rewrite clause)) these
            subjects = narrow BlockRequirement.subject candidates
            wanted = narrow BlockRequirement.attacker attackers
            blockersOf attacker = [(blocker, attacker) | blocker <- subjects, able blocker attacker]
         in if not (inForce source changes requirement)
              then ([], [])
              else case BlockRequirement.arity requirement of
                RequirementArity.EachSubject -> (concatMap blockersOf wanted, [])
                -- ONE group per ATTACKER the object axis names, which is what
                -- Gaea's Protector's sentence is about; the subject set is the
                -- creatures that could obey it. Two attackers under one such
                -- requirement are two group requirements, not one obeyed by
                -- blocking either.
                RequirementArity.AnySubject -> ([], fmap (Set.fromList . blockersOf) wanted)
      -- CR 509.1c's second shape -- "or that it must block if some condition is
      -- met" -- read as CR 604.2's "as long as" clause and asked exactly as
      -- AttackRequirement.instances and BlockPermission.instances ask their own
      -- `while`: a gate that does not hold mints nothing. The opposite polarity
      -- to CombatRestriction.inForce's "unless", where a gate that holds lifts
      -- the restriction.
      --
      -- Projection.fullView and not a bounded one, because CR 613.11 puts this
      -- effect after every layer, so there is no layer to bound against.
      --
      -- Asked once per REQUIREMENT rather than per pair: CR 109.5 fixes the
      -- "you" inside it as the source's controller, so neither the blocker nor
      -- the attacker could change the answer. The CR 612.1 rewrite is the same
      -- one both axes get, since all three clauses are printed on the source.
      -- Proved by blockRequirementSpec's Seton's Desire cases.
      inForce source changes requirement = case BlockRequirement.while requirement of
        Nothing -> True
        Just condition ->
          Condition.holds
            (Projection.fullView gs)
            (Filter.contextFor (Game.teams gs) (Projection.controllerOf source gs) (Just source))
            gs
            source
            (if null changes then condition else Projection.rewriteCondition changes condition)
      -- CR 509.1c again, off the STORED carrier. No CR 305.7 or CR 604.2 gate and
      -- no CR 612.1 rewrite, which is the posture PlayerEffect.applying takes for
      -- its stored rows: those three ask what a permanent's TEXT still says, and a
      -- resolution-created requirement has outlived its source's text (CR 611.2 --
      -- the effect exists on its own once the ability has resolved).
      --
      -- Pruned by `able` and by membership exactly as the printed pairs are: a
      -- provoked creature that has since left the battlefield, or that the
      -- defending player no longer controls, is not among `candidates`, and its
      -- attacker may have left combat.
      fromStored active =
        let blocker = ActiveBlockRequirement.blocker active
            attacker = ActiveBlockRequirement.attacker active
         in ( [ (blocker, attacker)
              | blocker `elem` candidates,
                attacker `elem` attackers,
                able blocker attacker
              ],
              []
            )
      gathered =
        fmap fromPermanent (Set.toList (GameState.battlefield gs))
          <> fmap fromStored (GameState.blockRequirements gs)
   in Requirement.gather (concatMap fst gathered) (concatMap snd gathered)
