-- CR 113.6m's "or effect" half, asked of an EFFECT: does it move the object the
-- ability is on out of a particular zone, and which zone? The ABILITY-level
-- readings that rule defines fold this over an ability's effects, one per kind
-- of ability: Pawl.Engine.Activate.zoneFunctionedFrom takes the cost half
-- (Pawl.Engine.Cost.zoneFunctionedFrom) alongside it, and
-- Pawl.Engine.Event.zoneFunctionedFrom has no cost half to take.
--
-- Here rather than in Pawl.Engine.Cost because it is a classification of an
-- EFFECT and that module's whole contract is to be the sole casing home for a
-- CostComponent; Pawl.Engine.ManaAbility is the precedent for the shape and for
-- the module boundary.
--
-- Casing on Effect here is not a breach of design.md section 1, for the reason
-- Pawl.Engine.ManaAbility gives: the closed half may depend on a CLASSIFICATION
-- of effects, and this function is one. What stays forbidden is a case that acts
-- on WHICH effect it is, and every arm below answers the one question in the
-- type.
module Pawl.Engine.EffectZone where

import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Types.Card as Card.Type
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ObjectRef as ObjectRef
import Pawl.Types.Zone (Zone)

-- CR 113.6m: "an ability whose cost or effect specifies that it moves the object
-- it's on out of a particular zone". Nothing for an effect that specifies no
-- such move, which leaves CR 113.6's own default in place -- the ability
-- functions on the battlefield.
--
-- TWO conditions, and both are the rule's own words. The effect has to name the
-- zone it moves the object out of, which is the origin Effect.MoveToZone
-- carries; and the object moved has to be "THE OBJECT IT'S ON", which is the
-- reserved slot CR 113.7's source is bound under. An effect that moves some
-- other object out of a graveyard -- Raise Dead's target -- says nothing about
-- where its own ability functions, and answering Just for it would strand every
-- such ability in the graveyard.
--
-- The origin is only ever consulted here, so a card file that states one on a
-- move of anything but its own source states something nothing reads. That is a
-- card-data error rather than a rules question, and it is inert: this function
-- answers Nothing for it, which is the same answer the effect had before.
zoneFunctionedFrom :: Effect Card.Type.Card -> Maybe Zone
zoneFunctionedFrom effect = case effect of
  -- Only an InSlot naming the reserved source slot can be "the object it's on".
  -- A swept set is never one object, so EachMatching answers Nothing however the
  -- card file states the origin -- the same inert card-data error the note above
  -- describes for a move of somebody else's permanent.
  Effect.MoveToZone ref _ _ _ origin _ -> case ref of
    ObjectRef.InSlot slot -> if slot == Binding.triggerSource then origin else Nothing
    ObjectRef.EachMatching _ -> Nothing
  Effect.DealDamage _ _ -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText {} -> Nothing
  Effect.AddMana _ -> Nothing
  Effect.Search _ _ -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.TemptWithTheRing -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.TurnFaceDown _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.ExchangeLifeTotals _ -> Nothing
  Effect.IncreaseSpeed {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.CreateCopy _ -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  -- CR 615.5's rider is not descended into, and the answer stands: the rider
  -- does not run as an effect of this ability at all, but from
  -- Pawl.Engine.Resolve.runPreventionRiders against the shielded permanent, so
  -- nothing it does can be this ability moving "the object it's on".
  Effect.PreventNextDamage {} -> Nothing
  Effect.PreventAllDamage {} -> Nothing
  Effect.RedirectDamage {} -> Nothing
  Effect.Counter _ -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.RemoveCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.RemovePlayerCounters {} -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.Transform _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.GainControl _ _ -> Nothing
  Effect.ArmDelayedTrigger {} -> Nothing
  Effect.AffectPlayers {} -> Nothing
  Effect.RequireBlock {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.BecomeRenowned _ -> Nothing
  Effect.BecomeMonstrous _ -> Nothing
  Effect.Suspect _ -> Nothing
  Effect.Evolve _ -> Nothing
  Effect.ItBecomes _ -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary _ -> Nothing
  Effect.OfferCast {} -> Nothing
  -- CR 113.6m names a zone an ability FUNCTIONS in by moving its own object out
  -- of it. This opcode moves nothing -- it writes a permission onto objects an
  -- earlier effect already placed -- so it names no zone.
  Effect.GrantPlayFromExile {} -> Nothing
