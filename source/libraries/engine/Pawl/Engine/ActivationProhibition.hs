-- CR 602.2 / 101.2 / 613.11: the continuous effects that FORBID an activation,
-- printed at an OBJECT. One of the modules on the axis CR 613.11 reaches past
-- the layer system (alongside Pawl.Engine.PlayerEffect,
-- Pawl.Engine.CombatRestriction, Pawl.Engine.SacrificeRestriction and its
-- siblings). None is a layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.ActivationProhibition. Every caller asks for one
-- permanent's answer and never learns which card produced it.
--
-- TWO places ask, and both are needed, exactly as Pawl.Engine.Detain's header
-- lays out for rule 701.35a's third clause: pawl has no single "may this
-- permanent act" funnel, so CR 602.2's window is Pawl.Engine.Activate's
-- activatableGiven and CR 605.3a's two mana windows are Pawl.Engine.Cost's
-- manaActivations. Gating only the first would leave every mana ability in the
-- pool going through, which is what Arrest's sentence forbids and
-- Realmbreaker's Grasp's exempts.
--
-- CR 605.1a's kind is therefore the caller's to state rather than this module's
-- to derive: only the caller holds the ability. Compared and never inspected
-- further, the posture Pawl.Engine.PlayerEffect's cost adjustments take with the
-- same type -- which side of a rulebook classification the ability falls on,
-- never what it does.
module Pawl.Engine.ActivationProhibition where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.Rewrite as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.ActivationProhibition as ActivationProhibition
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 602.2 with CR 101.2: which of `candidates` an effect in force right now
-- says can't have an activated ability of this CR 605.1a kind activated. Arrest
-- and Realmbreaker's Grasp are the pool's printings.
--
-- A set of ids and not a per-candidate predicate, for the reason
-- Pawl.Engine.CombatRestriction.restricted gives: a caller narrowing a whole
-- battlefield asks this once and tests against the answer, where a predicate
-- would re-walk the battlefield per candidate.
cantActivate :: AbilityKind.AbilityKind -> [ObjectId] -> GameState -> Set ObjectId
cantActivate asked candidates gs =
  let -- Hoisted out of the walk as SacrificeRestriction.cantBeSacrificed hoists
      -- them, and both unforced until some permanent actually declares a
      -- prohibition.
      setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- One whole-board projection and one grant walk for the whole walk, both
      -- unforced until some permanent actually reaches `named`.
      pcs = Projection.projectAll gs
      grants = Projection.controlGrants gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection -- the opposite of Projection.affects's
      -- callers inside the layer fold, which read characteristics as of their
      -- own layer.
      named source affected candidate =
        Projection.affectsOn
          pcs
          grants
          source
          candidate
          affected
          gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> []
        Just face -> case Face.activationProhibitions face of
          -- Every permanent in almost every game.
          [] -> []
          prohibitions ->
            -- The same two ability losses SacrificeRestriction.cantBeSacrificed
            -- asks about: CR 305.7's basic-land subtype set, and CR 604.2
            -- against a CR 613.1f layer-6 removal. Why CR 613.6 cannot rescue a
            -- prohibition that has started to apply is argued in
            -- Pawl.Engine.BlockRequirement.instances, as is why CR 613.11 also
            -- lets the CR 305.7 gate be liveAfterLayers rather than liveGiven.
            if (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              then
                -- CR 612.1's word swap over the SOURCE's own text, computed here
                -- rather than hoisted beside setEffs, the placement
                -- CombatRestriction.restricted argues for: textChangesAffecting
                -- folds the whole continuous-effect list, and the empty case
                -- above already turned away every permanent that prints no
                -- prohibition.
                concatMap (fromProhibition source (Projection.textChangesAffecting source gs)) prohibitions
              else []
      -- CR 605.1a's division, asked of the row rather than of the ability:
      -- Nothing is every activated ability (Arrest), and a named kind is only
      -- the abilities on that side of the rule (Realmbreaker's Grasp, whose
      -- "unless they're mana abilities" leaves NonManaAbility).
      fromProhibition source changes prohibition =
        if maybe True (== asked) (ActivationProhibition.kind prohibition)
          then
            let affected = ActivationProhibition.affected prohibition
             in filter (named source (if null changes then affected else Projection.rewriteAffected changes affected)) candidates
          else []
   in Set.fromList (concatMap fromPermanent (Set.toList (GameState.battlefield gs)))

-- The same question about ONE permanent, which is what both gates hold: an
-- activation window is asked about the ability's own source.
--
-- Not a cheaper computation, just a narrower one -- the walk above is over the
-- battlefield either way, and the candidate list it filters is the singleton.
prohibited :: AbilityKind.AbilityKind -> ObjectId -> GameState -> Bool
prohibited kind oid gs = Set.member oid (cantActivate kind [oid] gs)
