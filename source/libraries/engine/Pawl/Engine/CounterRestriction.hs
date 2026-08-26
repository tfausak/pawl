-- CR 101.2 / 122.6 / 613.11: the continuous effects that FORBID counters being put
-- on an object. One of the modules on the axis CR 613.11 reaches past the layer
-- system (alongside Pawl.Engine.PlayerEffect, Pawl.Engine.BlockRequirement,
-- Pawl.Engine.AttackRequirement, Pawl.Engine.CombatRestriction,
-- Pawl.Engine.AttackCost, Pawl.Engine.SacrificeRestriction,
-- Pawl.Engine.UntapRestriction, Pawl.Engine.AttachRestriction and
-- Pawl.Engine.EntryRestriction). None is a layer, and Pawl.Engine.Projection sees
-- none of them.
--
-- The only reader of Pawl.Types.CounterRestriction. Its callers ask a Bool about
-- one object and one kind and never learn which card produced it.
--
-- TWO places ask, for the reason Pawl.Engine.SacrificeRestriction's header gives
-- about its own two: CR 101.2 makes the "can't" beat the "can", so a refused
-- placement must neither be WRITTEN (Pawl.Engine.Event.settleCounters, the door
-- both of CR 122.6's roads share -- counters put on a permanent already on the
-- battlefield, and counters an object is given as it enters) nor be reached
-- around by CR 122.5's move, whose third impossibility is "the second object
-- can't have counters put onto it" (Pawl.Engine.Resolve's Effect.MoveCounters
-- arm). Gating only the first would still take the counter OFF the source, which
-- rule 122.5 forbids outright.
--
-- Asked AFTER CR 616.1's replacement loop has settled the placement, not before,
-- and the settled object is what it is asked about. A row may redirect a
-- placement onto a different object (Replacement.asCounters answers an ObjectId),
-- so the object the prohibition is about is the one that would actually receive
-- the counters rather than the one the effect named. Rule 614 replaces events
-- that would happen; rule 101.2 then refuses what rule 614 settled on.
module Pawl.Engine.CounterRestriction where

import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterRestriction as CounterRestriction
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import Pawl.Types.ObjectId (ObjectId)

-- CR 101.2 with CR 122.6: does an effect in force right now say that counters of
-- `kind` CAN'T BE PUT ON `oid`? Solemnity's second sentence and Melira, Sylvok
-- Outcast's second.
--
-- Pawl.Engine.EntryRestriction.prohibited's body, gate for gate, and every step of
-- its argument holds here unchanged: the same CR 305.7 and CR 613.1f ability
-- losses are asked of the SOURCE, the same CR 612.1 word swap is applied to its
-- affected set, and CR 613.11 is why the set is read against the FULL projection
-- rather than a layer-bounded one.
--
-- A Bool about ONE object and ONE kind rather than a set over a candidate list,
-- which is the shape of the question rather than a narrowing: the kind is a
-- property of the individual placement, and both callers hold exactly one
-- placement.
prohibited :: ObjectId -> CounterKind.CounterKind Keyword.Keyword -> GameState -> Bool
prohibited oid kind gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- CR 613.11 puts these effects after every layer, so the affected set is
      -- read against the FULL projection.
      named source affected =
        Projection.affects
          source
          oid
          affected
          (Projection.project oid gs)
          gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> False
        Just face -> case Face.counterRestrictions face of
          -- Every permanent in almost every game.
          [] -> False
          restrictions ->
            (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              && any (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
      -- The CR 305.7 setEffs gate and the CR 612.1 rewrite are REGRESSION FENCES
      -- rather than proven behaviour, exactly as Pawl.Engine.EntryRestriction's
      -- header records of its own: neither printing in the pool is a land, and no
      -- card in data/cards text-changes a Solemnity or a Melira. The CR 604.2
      -- ability-loss gate is likewise unproven here. They are copied from
      -- Pawl.Engine.EntryRestriction because the CR states them and an
      -- inconsistency between six carriers is the worse answer.
      fromRestriction source changes restriction =
        let affected = CounterRestriction.affected restriction
         in -- Nothing is Solemnity's "counters", every kind; Just is Melira's
            -- "-1/-1 counters" and refuses that kind alone.
            maybe True (== kind) (CounterRestriction.kind restriction)
              && named source (if null changes then affected else Projection.rewriteAffected changes affected)
   in any fromPermanent (Set.toList (GameState.battlefield gs))
