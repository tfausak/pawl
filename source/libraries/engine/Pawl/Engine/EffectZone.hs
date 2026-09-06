-- CR 113.6m's "or effect" half, asked of an EFFECT: does it move the object the
-- ability is on out of a particular zone, and which zone? The ABILITY-level
-- readings that rule defines fold this over an ability's effects, one per kind
-- of ability: Pawl.Engine.Activate.zoneFunctionedFrom takes the cost half
-- (Pawl.Engine.Cost.zoneFunctionedFrom) alongside it, and
-- Pawl.Engine.Event.Trigger.zoneFunctionedFrom has no cost half to take.
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

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.Designate as Designate
import qualified Pawl.Types.DurationRef as DurationRef
import Pawl.Types.Effect (Effect)
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SetHalfLocked as SetHalfLocked
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import Pawl.Types.Zone (Zone)

-- CR 113.6m: "an ability whose cost or effect specifies that it moves the object
-- it's on out of a particular zone". Nothing for an effect that specifies no
-- such move, which leaves CR 113.6's own default in place -- the ability
-- functions on the battlefield.
--
-- TWO conditions, and both are the rule's own words. The effect has to name the
-- zone it moves the object out of, which is the origin Effect.MoveToZone
-- carries; and the object moved) has to be "THE OBJECT) IT'S ON", which is the
-- reserved slot CR 113.7's source is bound under. An effect that moves some
-- other object out of a graveyard -- Raise Dead's target -- says nothing about
-- where its own ability functions, and answering Just for it would strand every
-- such ability in the graveyard.
--
-- The origin is only ever consulted here, so a card file that states one on a
-- move of anything but its own source states something nothing reads. That is a
-- card-data error rather than a rules question, and it is inert: this function
-- answers Nothing for it, which is the same answer the effect had before.
--
-- The DELAYED ABILITIES the reading is against -- CR 113.6m's final sentence
-- needs the text an Effect.ArmDelayedTrigger's name stands for, and the opcode
-- carries only the name (Pawl.Types.Face.delayedAbilities holds the payload,
-- Effect being first-order). Every caller supplies the map off the same card the
-- ability is read from, which is where Pawl.Engine.Resolve.Effect.declaredDelayedAbility
-- resolves the name at run time.
--
-- Not implemented: rule 702's own delayed abilities
-- (Pawl.Engine.Keyword.mintedDelayedAbilities), which Resolve falls back to and
-- no caller unions in here, so an arm naming one reads as a name no face
-- declares. Inert as that roster stands -- decayed's is a sacrifice, which every
-- arm below answers Nothing for -- and a minted ability that MOVED its object
-- out of a zone is what would want the union (#3083).
zoneFunctionedFrom :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) -> Effect Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card) -> Maybe Zone
zoneFunctionedFrom delayed effect = case effect of
  -- Only an InSlot naming the reserved source slot can be "the object it's on".
  -- A swept set is never one object, so no sweeping arm can be; a library's
  -- top card is one object, but it is named by POSITION rather than by that slot,
  -- so it cannot be one either, and a chosen card in a graveyard or a hand is
  -- named by a CHOICE among many, which is not "the object it's on" for the same
  -- reason -- and neither is a card chosen out of a bound group. All of them
  -- answer Nothing however the card file states the origin -- the same inert
  -- card-data error the note above describes for a move of somebody else's
  -- permanent.
  Effect.MoveToZone (MoveToZone.MkMoveToZone ref _ _ _ origin _ _) -> case ref of
    ObjectRef.InSlot slot -> if slot == Binding.triggerSource then origin else Nothing
    ObjectRef.EachMatching _ -> Nothing
    ObjectRef.EachCardInGraveyard {} -> Nothing
    ObjectRef.EachCardInYourHand -> Nothing
    ObjectRef.EachCardInHand {} -> Nothing
    ObjectRef.EachCardInYourLibrary _ -> Nothing
    ObjectRef.EachCardExiledWithSource {} -> Nothing
    ObjectRef.EachSpell _ -> Nothing
    ObjectRef.EachOnStack _ -> Nothing
    ObjectRef.EachPlayer -> Nothing
    ObjectRef.EachOpponent -> Nothing
    ObjectRef.ChosenPlayer -> Nothing
    ObjectRef.Players _ -> Nothing
    ObjectRef.TopOfLibrary {} -> Nothing
    ObjectRef.TopOfLibraryUntil {} -> Nothing
    ObjectRef.TopOfGraveyard _ -> Nothing
    ObjectRef.ChosenCardInGraveyard {} -> Nothing
    ObjectRef.ChosenCardInHand {} -> Nothing
    ObjectRef.ChosenCardFromAmong {} -> Nothing
    ObjectRef.EachCardFromAmong {} -> Nothing
    ObjectRef.RandomCardInHand _ -> Nothing
    ObjectRef.AnyNumberMatching _ -> Nothing
    ObjectRef.ChosenPermanent _ -> Nothing
    ObjectRef.SourceAndChosenPermanent _ -> Nothing
  Effect.DealDamage (DealDamage.MkDealDamage {}) -> Nothing
  Effect.Fight {} -> Nothing
  Effect.ModifyTarget {} -> Nothing
  Effect.ChangeText {} -> Nothing
  Effect.AddMana _ -> Nothing
  Effect.Search {} -> Nothing
  Effect.ExileAllGraveyards -> Nothing
  Effect.Proliferate -> Nothing
  Effect.ChooseCardName _ -> Nothing
  Effect.FromOutsideTheGame _ -> Nothing
  -- CR 608.2n: the spell exiles ITSELF, so this says nothing about where an
  -- ability functions -- and no ability can print it at all.
  Effect.ExileThisSpell -> Nothing
  Effect.Bolster _ -> Nothing
  Effect.Amass _ -> Nothing
  Effect.Blight _ -> Nothing
  Effect.Earthbend _ -> Nothing
  Effect.TemptWithTheRing -> Nothing
  Effect.Venture {} -> Nothing
  Effect.ExileHandThenDraw -> Nothing
  Effect.PlayerSacrifices {} -> Nothing
  Effect.RestartGame _ -> Nothing
  Effect.ControlPlayerNextTurn _ -> Nothing
  Effect.Destroy {} -> Nothing
  Effect.Sacrifice _ -> Nothing
  Effect.TurnFaceDown _ -> Nothing
  Effect.TurnFaceUp _ -> Nothing
  Effect.RemoveFromCombat _ -> Nothing
  Effect.BecomesBlocked _ -> Nothing
  Effect.Draw {} -> Nothing
  Effect.Mill {} -> Nothing
  Effect.Reveal {} -> Nothing
  Effect.LookAt {} -> Nothing
  Effect.Scry {} -> Nothing
  Effect.Surveil {} -> Nothing
  Effect.Fateseal {} -> Nothing
  Effect.Explore {} -> Nothing
  Effect.Discard {} -> Nothing
  Effect.LoseLife {} -> Nothing
  Effect.GainLife {} -> Nothing
  Effect.ExchangeLifeTotals _ -> Nothing
  Effect.SetLifeTotal {} -> Nothing
  Effect.RedistributeLifeTotals -> Nothing
  Effect.IncreaseSpeed {} -> Nothing
  Effect.DecreaseSpeed {} -> Nothing
  Effect.Create {} -> Nothing
  Effect.Conjure {} -> Nothing
  Effect.CreateCopy {} -> Nothing
  -- CR 707.4 changes a permanent's copiable values while it stays on the
  -- battlefield, so nothing leaves a zone and this functions from none.
  Effect.BecomeCopy {} -> Nothing
  -- CR 707.10 mints a new object onto the stack; nothing the ability is on
  -- moves out of a zone.
  Effect.CopyStackObject {} -> Nothing
  Effect.Replace {} -> Nothing
  Effect.SkipNextPhase {} -> Nothing
  -- CR 615.5's rider is not descended into, and the answer stands: the rider
  -- does not run as an effect of this ability at all, but from
  -- Pawl.Engine.Resolve.Effect.runPreventionRider against the effect's own source, so
  -- nothing it does can be this ability moving "the object it's on".
  Effect.PreventNextDamage {} -> Nothing
  Effect.PreventAllDamage {} -> Nothing
  Effect.PreventNextDamageInstance {} -> Nothing
  Effect.RedirectDamage {} -> Nothing
  Effect.Counter {} -> Nothing
  Effect.PutCounters {} -> Nothing
  Effect.PutCountersFrom {} -> Nothing
  Effect.RemoveCounters {} -> Nothing
  Effect.MoveCounters {} -> Nothing
  Effect.GainPlayerCounters {} -> Nothing
  Effect.RemovePlayerCounters {} -> Nothing
  Effect.PayAnyEnergy _ -> Nothing
  Effect.Tap _ -> Nothing
  Effect.Untap _ -> Nothing
  Effect.Detain _ -> Nothing
  Effect.Goad _ -> Nothing
  Effect.MakePlotted _ -> Nothing
  Effect.DoesNotUntapNext _ -> Nothing
  Effect.Transform _ -> Nothing
  Effect.Convert _ -> Nothing
  -- CR 113.6m asks which zone an ability functions FROM by where the object it is
  -- on moves. A meld moves the cards the ability NAMED, and the melding ability's
  -- own source is one of them only because an earlier instruction exiled it --
  -- that instruction is the move, and this one puts cards onto the battlefield
  -- from wherever they already are.
  Effect.Meld {} -> Nothing
  Effect.PhaseOut _ -> Nothing
  Effect.AddPhases _ -> Nothing
  Effect.EndTurn -> Nothing
  Effect.EndCombatPhase -> Nothing
  Effect.GainControl (DurationRef.MkDurationRef _ _) -> Nothing
  -- CR 113.6m's final sentence: "the same is true if the effect of that ability
  -- creates a delayed triggered ability whose effect moves the object out of a
  -- particular zone". Prized Amalgam's "return this card from your graveyard to
  -- the battlefield tapped at the beginning of the next end step" is the printing
  -- it decides -- the arm is the ability's only zone-relevant content, so without
  -- this the Amalgam functions on the battlefield, where it can never do its work.
  --
  -- ONE LEVEL: the delayed ability's own effects are walked with an EMPTY map, so
  -- a delayed ability that arms a second one contributes nothing. The rule says
  -- "creates a delayed triggered ability whose effect moves the object", naming
  -- the created ability's effect and not what that in turn creates, and the empty
  -- map is also what makes this walk terminate on a card that arms itself.
  --
  -- Nothing for a name no face declares, which is a card-data error rather than a
  -- rules question: Pawl.CardSpec's D4 dataflow lint rejects such a card, and the
  -- answer here is the same Nothing the arm gave before.
  Effect.ArmDelayedTrigger (ArmDelayedTrigger.MkArmDelayedTrigger {ArmDelayedTrigger.name = name}) ->
    case Map.lookup name delayed of
      Nothing -> Nothing
      Just ability -> Maybe.listToMaybe (Maybe.mapMaybe (zoneFunctionedFrom Map.empty) (Modal.allEffects (TriggeredAbility.modal ability)))
  Effect.AffectPlayers {} -> Nothing
  Effect.RequireBlock {} -> Nothing
  Effect.CantBeRegenerated {} -> Nothing
  Effect.ForbidBlock {} -> Nothing
  Effect.ForbidAttack {} -> Nothing
  Effect.ForbidActivation {} -> Nothing
  Effect.RequireAttack {} -> Nothing
  Effect.CreateEmblem {} -> Nothing
  Effect.BecomeMonarch {} -> Nothing
  Effect.TakeTheInitiative {} -> Nothing
  Effect.Designate (Designate.MkDesignate _ _) -> Nothing
  Effect.SetClassLevel (SetClassLevel.MkSetClassLevel _ _) -> Nothing
  Effect.Unsuspect _ -> Nothing
  Effect.SetHalfLocked (SetHalfLocked.MkSetHalfLocked {}) -> Nothing
  Effect.Evolve _ -> Nothing
  Effect.Mentor _ -> Nothing
  Effect.Train _ -> Nothing
  Effect.ItBecomes _ -> Nothing
  Effect.ExileUntilMonarch _ -> Nothing
  Effect.ExileHaunting {} -> Nothing
  Effect.Attach _ -> Nothing
  Effect.AttachTarget {} -> Nothing
  Effect.AttachTargetToEach {} -> Nothing
  Effect.AttachBound {} -> Nothing
  Effect.PlaySubgame _ -> Nothing
  Effect.ChoosePlayer _ -> Nothing
  Effect.ChooseOpponentAtRandom _ -> Nothing
  Effect.RollDie {} -> Nothing
  Effect.FlipCoin {} -> Nothing
  Effect.TakeExtraTurn {} -> Nothing
  Effect.ShuffleIntoLibrary {} -> Nothing
  Effect.Shuffle {} -> Nothing
  Effect.OfferCast {} -> Nothing
  -- CR 113.6m names a zone an ability FUNCTIONS in by moving its own object out
  -- of it. This opcode moves nothing -- it writes a permission onto objects an
  -- earlier effect already placed -- so it names no zone.
  Effect.GrantPlayFromExile {} -> Nothing
  -- Descended into, unlike CR 615.5's rider above: rule 608.2f's body runs as
  -- part of THIS effect, so a move it states is a move this ability's effect
  -- states, and CR 113.6m reads it. The loop's own reference names the members
  -- and is never "the object it's on", so only the body can answer at all. No
  -- card in the pool writes such a body.
  Effect.ForEach (ForEach.MkForEach _ _ body) -> Maybe.listToMaybe (Maybe.mapMaybe (zoneFunctionedFrom delayed) (Foldable.toList body))
