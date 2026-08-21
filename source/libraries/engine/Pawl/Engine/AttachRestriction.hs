-- CR 303.4 / 301.5 / 101.2 / 613.11: the continuous effects a DESTINATION puts on
-- what may become attached to it. One of the modules on the axis CR 613.11
-- reaches past the layer system (alongside Pawl.Engine.PlayerEffect,
-- Pawl.Engine.BlockRequirement, Pawl.Engine.AttackRequirement,
-- Pawl.Engine.CombatRestriction, Pawl.Engine.AttackCost,
-- Pawl.Engine.SacrificeRestriction and Pawl.Engine.UntapRestriction). None is a
-- layer, and Pawl.Engine.Projection sees none of them.
--
-- The only reader of Pawl.Types.AttachRestriction. Its callers ask about a PAIR
-- and never learn which card produced the answer.
--
-- TWO places ask, and both are needed for the reason
-- Pawl.Engine.SacrificeRestriction's header gives about its own two: CR 101.2
-- makes the "can't" beat the "can", so a refused pairing must neither be
-- ATTACHED (Pawl.Engine.Attach.attachmentFor, which every CR 701.3 move and every
-- Filter.CanHostSubject offer goes through) nor be left STANDING once it somehow
-- is (Pawl.Engine.Sba.fallsOff, CR 704.5m with CR 303.4c). Gating only the first
-- would leave CR 608.3c's Aura spell -- which enters attached to its target
-- whatever the destination says -- permanently enchanting a permanent that
-- refuses it.
--
-- TARGETING is not one of those places, and that is the rule rather than an
-- omission: CR 702.5a gives the enchant ability both jobs and this restriction is
-- neither, so an Aura spell may still target a permanent that refuses it (CR
-- 608.2b keeps it legal, CR 608.3c attaches it, CR 704.5m buries it on the next
-- state-based check). Protection is the one quality that also forbids the
-- targeting, in a clause of its own (CR 702.16b), and pawl has no protection
-- keyword at all. Not implemented: an Aura spell with protection's quality is
-- still a legal target for the permanent that has protection from it (#1731).
module Pawl.Engine.AttachRestriction where

import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.AttachRestriction as AttachRestriction
import qualified Pawl.Types.Face as Face
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)

-- CR 303.4's last sentence and CR 301.5 with CR 101.2: does any effect in force
-- right now say `subject` can't become attached to `host`? Consecrate Land's
-- "can't be enchanted by other Auras" and Goblin Brawler's "can't be equipped".
--
-- A Bool about a PAIR, where Pawl.Engine.SacrificeRestriction answers a set of
-- ids: this question has two object positions, so there is no one set to hand
-- back. Both of its callers hold the pair already.
--
-- Pawl.Engine.SacrificeRestriction.cantBeSacrificed's body, gate for gate, plus
-- the second position: the same CR 305.7 and CR 613.1f ability losses are asked
-- of the RESTRICTING permanent, the same CR 612.1 word swap is applied -- to the
-- affected set AND to the attachers filter, since both are words on that one
-- card, which is Pawl.Engine.CombatRestriction.cantBeBlockedBy's posture toward
-- its own two halves -- and CR 613.11 is why the affected set is read against the
-- FULL projection rather than a layer-bounded one.
--
-- The filter's frame is the RESTRICTING permanent: its id is Filter.IsSource, so
-- Consecrate Land's "other Auras" is a Not over that atom, and its controller is
-- CR 109.5's "you". The subject is matched through its own projection, so an Aura
-- that has lost the subtype under CR 613's layer 4 is barred by nothing.
refuses :: ObjectId -> ObjectId -> GameState -> Bool
refuses subject host gs =
  let setEffs = Projection.setLandSubtypeEffects gs
      removed = Projection.abilityRemoval gs
      -- Forced at most once, and only by a board on which some permanent
      -- actually declares a prohibition.
      subjectView = Projection.viewOfObject subject gs
      fromPermanent source = case Game.faceOf source gs of
        Nothing -> False
        Just face -> case Face.attachRestrictions face of
          -- Every permanent in almost every game.
          [] -> False
          restrictions ->
            (null setEffs || Projection.liveAfterLayers setEffs source gs)
              && not (removed source)
              && any (fromRestriction source (Projection.textChangesAffecting source gs)) restrictions
      fromRestriction source changes restriction =
        let affected = AttachRestriction.affected restriction
            attachers = AttachRestriction.attachers restriction
            named =
              Projection.affects
                source
                host
                (if null changes then affected else Projection.rewriteAffected changes affected)
                (Projection.project host gs)
                gs
            barred =
              Filter.matches
                (Filter.contextFor (Projection.controllerOf source gs) (Just source))
                subjectView
                (if null changes then attachers else Filter.rewrite changes attachers)
         in named && barred
   in any fromPermanent (Set.toList (GameState.battlefield gs))
