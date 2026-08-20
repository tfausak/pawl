-- | Where the effect knot is tied. The card codec is a PARAMETER, so this module
-- names no concrete card type; Pawl.Codec.Card passes its own codec in.
--
-- RECURSIVE five times over: PreventNextDamage's CR 615.5 rider, PreventAllDamage's
-- rider under the same rule, the same rule's rider on a Replace's DamageR, and
-- ForEach's CR 608.2f body all hold effects, and CreateEmblem's payload is a whole
-- card whose faces hold effects. Each names 'codec' inside its own definition, which
-- terminates for Pawl.Codec.TriggerCondition's
-- reason -- 'Arm.tagged' reaches WHNF without forcing its arm list, and
-- 'Define.define' registers the type's name before running the schema body.
--
-- Casing on an effect's identity here is open-half machinery, not the rules core.
module Pawl.Codec.Effect where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AffectPlayers as AffectPlayers
import qualified Pawl.Codec.Amass as Amass
import qualified Pawl.Codec.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Codec.AttachTarget as AttachTarget
import qualified Pawl.Codec.BecomeCopy as BecomeCopy
import qualified Pawl.Codec.ChangeText as ChangeText
import qualified Pawl.Codec.Counter as Counter
import qualified Pawl.Codec.Create as Create
import qualified Pawl.Codec.CreateCopy as CreateCopy
import qualified Pawl.Codec.Daytime as Daytime
import qualified Pawl.Codec.DealDamage as DealDamage
import qualified Pawl.Codec.Designate as Designate
import qualified Pawl.Codec.Destroy as Destroy
import qualified Pawl.Codec.Discard as Discard
import qualified Pawl.Codec.DurationRef as DurationRef
import qualified Pawl.Codec.ExchangeSides as ExchangeSides
import qualified Pawl.Codec.ExileHaunting as ExileHaunting
import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.Codec.Fight as Fight
import qualified Pawl.Codec.ForEach as ForEach
import qualified Pawl.Codec.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Codec.LookAt as LookAt
import qualified Pawl.Codec.ManaAddition as ManaAddition
import qualified Pawl.Codec.Mill as Mill
import qualified Pawl.Codec.ModifyTarget as ModifyTarget
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Codec.MoveToZone as MoveToZone
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.OfferCast as OfferCast
import qualified Pawl.Codec.PlayerCounters as PlayerCounters
import qualified Pawl.Codec.PlayerQuantity as PlayerQuantity
import qualified Pawl.Codec.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Codec.PreventAllDamage as PreventAllDamage
import qualified Pawl.Codec.PreventNextDamage as PreventNextDamage
import qualified Pawl.Codec.PutCounters as PutCounters
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.RedirectDamage as RedirectDamage
import qualified Pawl.Codec.RemoveCounters as RemoveCounters
import qualified Pawl.Codec.Replace as Replace
import qualified Pawl.Codec.RequireBlock as RequireBlock
import qualified Pawl.Codec.Reveal as Reveal
import qualified Pawl.Codec.Search as Search
import qualified Pawl.Codec.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Codec.SkipNextPhase as SkipNextPhase
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.SpeedDecrease as SpeedDecrease
import qualified Pawl.Codec.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.Codec.TurnFaceDown as TurnFaceDown
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Effect as Effect

codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Effect.Effect card)
codec cardCodec =
  Arm.tagged
    [ Arm.payload "DealDamage" DealDamage.codec Effect.DealDamage (\x -> case x of Effect.DealDamage y -> Just y; _ -> Nothing),
      Arm.payload "ModifyTarget" ModifyTarget.codec Effect.ModifyTarget (\x -> case x of Effect.ModifyTarget y -> Just y; _ -> Nothing),
      Arm.payload "ChangeText" ChangeText.codec Effect.ChangeText (\x -> case x of Effect.ChangeText y -> Just y; _ -> Nothing),
      Arm.payload "AddMana" ManaAddition.codec Effect.AddMana (\x -> case x of Effect.AddMana y -> Just y; _ -> Nothing),
      Arm.payload "Search" Search.codec Effect.Search (\x -> case x of Effect.Search y -> Just y; _ -> Nothing),
      Arm.nullary "ExileAllGraveyards" Effect.ExileAllGraveyards,
      Arm.nullary "Proliferate" Effect.Proliferate,
      Arm.payload "Bolster" Quantity.codec Effect.Bolster (\x -> case x of Effect.Bolster y -> Just y; _ -> Nothing),
      Arm.payload "Amass" Amass.codec Effect.Amass (\x -> case x of Effect.Amass y -> Just y; _ -> Nothing),
      Arm.payload "Blight" PlayerQuantity.codec Effect.Blight (\x -> case x of Effect.Blight y -> Just y; _ -> Nothing),
      Arm.nullary "TemptWithTheRing" Effect.TemptWithTheRing,
      Arm.nullary "Venture" Effect.Venture,
      Arm.nullary "ExileHandThenDraw" Effect.ExileHandThenDraw,
      Arm.payload "PlayerSacrifices" PlayerSacrifices.codec Effect.PlayerSacrifices (\x -> case x of Effect.PlayerSacrifices y -> Just y; _ -> Nothing),
      Arm.optionalPayload "RestartGame" ObjectRef.codec Effect.RestartGame (\x -> case x of Effect.RestartGame y -> Just y; _ -> Nothing),
      Arm.payload "ControlPlayerNextTurn" SlotName.codec Effect.ControlPlayerNextTurn (\x -> case x of Effect.ControlPlayerNextTurn y -> Just y; _ -> Nothing),
      Arm.payload "Destroy" Destroy.codec Effect.Destroy (\x -> case x of Effect.Destroy y -> Just y; _ -> Nothing),
      Arm.payload "Fight" Fight.codec Effect.Fight (\x -> case x of Effect.Fight y -> Just y; _ -> Nothing),
      Arm.payload "Sacrifice" SlotName.codec Effect.Sacrifice (\x -> case x of Effect.Sacrifice y -> Just y; _ -> Nothing),
      Arm.payload "TurnFaceDown" TurnFaceDown.codec Effect.TurnFaceDown (\x -> case x of Effect.TurnFaceDown y -> Just y; _ -> Nothing),
      Arm.payload "TurnFaceUp" SlotName.codec Effect.TurnFaceUp (\x -> case x of Effect.TurnFaceUp y -> Just y; _ -> Nothing),
      Arm.payload "RemoveFromCombat" SlotName.codec Effect.RemoveFromCombat (\x -> case x of Effect.RemoveFromCombat y -> Just y; _ -> Nothing),
      Arm.payload "BecomesBlocked" SlotName.codec Effect.BecomesBlocked (\x -> case x of Effect.BecomesBlocked y -> Just y; _ -> Nothing),
      Arm.payload "Counter" Counter.codec Effect.Counter (\x -> case x of Effect.Counter y -> Just y; _ -> Nothing),
      Arm.payload "MoveToZone" MoveToZone.codec Effect.MoveToZone (\x -> case x of Effect.MoveToZone y -> Just y; _ -> Nothing),
      Arm.payload "Draw" PlayerQuantity.codec Effect.Draw (\x -> case x of Effect.Draw y -> Just y; _ -> Nothing),
      Arm.payload "Reveal" Reveal.codec Effect.Reveal (\x -> case x of Effect.Reveal y -> Just y; _ -> Nothing),
      Arm.payload "LookAt" LookAt.codec Effect.LookAt (\x -> case x of Effect.LookAt y -> Just y; _ -> Nothing),
      Arm.payload "Scry" PlayerQuantity.codec Effect.Scry (\x -> case x of Effect.Scry y -> Just y; _ -> Nothing),
      Arm.payload "Surveil" PlayerQuantity.codec Effect.Surveil (\x -> case x of Effect.Surveil y -> Just y; _ -> Nothing),
      Arm.payload "Fateseal" PlayerQuantity.codec Effect.Fateseal (\x -> case x of Effect.Fateseal y -> Just y; _ -> Nothing),
      Arm.payload "Explore" ObjectRef.codec Effect.Explore (\x -> case x of Effect.Explore y -> Just y; _ -> Nothing),
      Arm.payload "Mill" Mill.codec Effect.Mill (\x -> case x of Effect.Mill y -> Just y; _ -> Nothing),
      Arm.payload "Discard" Discard.codec Effect.Discard (\x -> case x of Effect.Discard y -> Just y; _ -> Nothing),
      Arm.payload "LoseLife" PlayerQuantity.codec Effect.LoseLife (\x -> case x of Effect.LoseLife y -> Just y; _ -> Nothing),
      Arm.payload "GainLife" PlayerQuantity.codec Effect.GainLife (\x -> case x of Effect.GainLife y -> Just y; _ -> Nothing),
      Arm.payload "ExchangeLifeTotals" ExchangeSides.codec Effect.ExchangeLifeTotals (\x -> case x of Effect.ExchangeLifeTotals y -> Just y; _ -> Nothing),
      Arm.payload "SetLifeTotal" PlayerQuantity.codec Effect.SetLifeTotal (\x -> case x of Effect.SetLifeTotal y -> Just y; _ -> Nothing),
      Arm.nullary "RedistributeLifeTotals" Effect.RedistributeLifeTotals,
      Arm.payload "IncreaseSpeed" PlayerQuantity.codec Effect.IncreaseSpeed (\x -> case x of Effect.IncreaseSpeed y -> Just y; _ -> Nothing),
      Arm.payload "DecreaseSpeed" SpeedDecrease.codec Effect.DecreaseSpeed (\x -> case x of Effect.DecreaseSpeed y -> Just y; _ -> Nothing),
      Arm.payload "Create" createCodec Effect.Create (\x -> case x of Effect.Create y -> Just y; _ -> Nothing),
      Arm.payload "CreateCopy" CreateCopy.codec Effect.CreateCopy (\x -> case x of Effect.CreateCopy y -> Just y; _ -> Nothing),
      Arm.payload "BecomeCopy" BecomeCopy.codec Effect.BecomeCopy (\x -> case x of Effect.BecomeCopy y -> Just y; _ -> Nothing),
      Arm.payload "Replace" replaceCodec Effect.Replace (\x -> case x of Effect.Replace y -> Just y; _ -> Nothing),
      Arm.payload "SkipNextPhase" SkipNextPhase.codec Effect.SkipNextPhase (\x -> case x of Effect.SkipNextPhase y -> Just y; _ -> Nothing),
      Arm.payload "PreventNextDamage" preventCodec Effect.PreventNextDamage (\x -> case x of Effect.PreventNextDamage y -> Just y; _ -> Nothing),
      Arm.payload "PreventAllDamage" preventAllCodec Effect.PreventAllDamage (\x -> case x of Effect.PreventAllDamage y -> Just y; _ -> Nothing),
      Arm.payload "RedirectDamage" RedirectDamage.codec Effect.RedirectDamage (\x -> case x of Effect.RedirectDamage y -> Just y; _ -> Nothing),
      Arm.payload "PutCounters" PutCounters.codec Effect.PutCounters (\x -> case x of Effect.PutCounters y -> Just y; _ -> Nothing),
      Arm.payload "RemoveCounters" RemoveCounters.codec Effect.RemoveCounters (\x -> case x of Effect.RemoveCounters y -> Just y; _ -> Nothing),
      Arm.payload "GainPlayerCounters" PlayerCounters.codec Effect.GainPlayerCounters (\x -> case x of Effect.GainPlayerCounters y -> Just y; _ -> Nothing),
      Arm.payload "RemovePlayerCounters" PlayerCounters.codec Effect.RemovePlayerCounters (\x -> case x of Effect.RemovePlayerCounters y -> Just y; _ -> Nothing),
      Arm.payload "PayAnyEnergy" SlotName.codec Effect.PayAnyEnergy (\x -> case x of Effect.PayAnyEnergy y -> Just y; _ -> Nothing),
      Arm.payload "Tap" ObjectRef.codec Effect.Tap (\x -> case x of Effect.Tap y -> Just y; _ -> Nothing),
      Arm.payload "Untap" ObjectRef.codec Effect.Untap (\x -> case x of Effect.Untap y -> Just y; _ -> Nothing),
      Arm.payload "Detain" ObjectRef.codec Effect.Detain (\x -> case x of Effect.Detain y -> Just y; _ -> Nothing),
      Arm.payload "DoesNotUntapNext" ObjectRef.codec Effect.DoesNotUntapNext (\x -> case x of Effect.DoesNotUntapNext y -> Just y; _ -> Nothing),
      Arm.payload "Transform" ObjectRef.codec Effect.Transform (\x -> case x of Effect.Transform y -> Just y; _ -> Nothing),
      Arm.payload "PhaseOut" ObjectRef.codec Effect.PhaseOut (\x -> case x of Effect.PhaseOut y -> Just y; _ -> Nothing),
      Arm.payload "AddPhases" (Common.list ExtraPhase.codec) Effect.AddPhases (\x -> case x of Effect.AddPhases y -> Just y; _ -> Nothing),
      Arm.payload "GainControl" DurationRef.codec Effect.GainControl (\x -> case x of Effect.GainControl y -> Just y; _ -> Nothing),
      Arm.payload "ArmDelayedTrigger" ArmDelayedTrigger.codec Effect.ArmDelayedTrigger (\x -> case x of Effect.ArmDelayedTrigger y -> Just y; _ -> Nothing),
      Arm.payload "AffectPlayers" AffectPlayers.codec Effect.AffectPlayers (\x -> case x of Effect.AffectPlayers y -> Just y; _ -> Nothing),
      Arm.payload "RequireBlock" RequireBlock.codec Effect.RequireBlock (\x -> case x of Effect.RequireBlock y -> Just y; _ -> Nothing),
      Arm.payload "CreateEmblem" cardCodec Effect.CreateEmblem (\x -> case x of Effect.CreateEmblem y -> Just y; _ -> Nothing),
      Arm.payload "BecomeMonarch" MonarchTarget.codec Effect.BecomeMonarch (\x -> case x of Effect.BecomeMonarch y -> Just y; _ -> Nothing),
      Arm.payload "Designate" Designate.codec Effect.Designate (\x -> case x of Effect.Designate y -> Just y; _ -> Nothing),
      Arm.payload "Unsuspect" ObjectRef.codec Effect.Unsuspect (\x -> case x of Effect.Unsuspect y -> Just y; _ -> Nothing),
      Arm.payload "Evolve" SlotName.codec Effect.Evolve (\x -> case x of Effect.Evolve y -> Just y; _ -> Nothing),
      Arm.payload "Mentor" SlotName.codec Effect.Mentor (\x -> case x of Effect.Mentor y -> Just y; _ -> Nothing),
      Arm.payload "Train" SlotName.codec Effect.Train (\x -> case x of Effect.Train y -> Just y; _ -> Nothing),
      Arm.payload "ItBecomes" Daytime.codec Effect.ItBecomes (\x -> case x of Effect.ItBecomes y -> Just y; _ -> Nothing),
      Arm.payload "ExileUntilMonarch" SlotName.codec Effect.ExileUntilMonarch (\x -> case x of Effect.ExileUntilMonarch y -> Just y; _ -> Nothing),
      Arm.payload "ExileHaunting" ExileHaunting.codec Effect.ExileHaunting (\x -> case x of Effect.ExileHaunting y -> Just y; _ -> Nothing),
      Arm.payload "Attach" SlotName.codec Effect.Attach (\x -> case x of Effect.Attach y -> Just y; _ -> Nothing),
      Arm.payload "AttachTarget" AttachTarget.codec Effect.AttachTarget (\x -> case x of Effect.AttachTarget y -> Just y; _ -> Nothing),
      Arm.payload "PlaySubgame" SlotName.codec Effect.PlaySubgame (\x -> case x of Effect.PlaySubgame y -> Just y; _ -> Nothing),
      Arm.payload "ChooseOpponent" SlotName.codec Effect.ChooseOpponent (\x -> case x of Effect.ChooseOpponent y -> Just y; _ -> Nothing),
      Arm.payload "TakeExtraTurn" TakeExtraTurn.codec Effect.TakeExtraTurn (\x -> case x of Effect.TakeExtraTurn y -> Just y; _ -> Nothing),
      Arm.payload "ShuffleIntoLibrary" ShuffleIntoLibrary.codec Effect.ShuffleIntoLibrary (\x -> case x of Effect.ShuffleIntoLibrary y -> Just y; _ -> Nothing),
      Arm.payload "OfferCast" OfferCast.codec Effect.OfferCast (\x -> case x of Effect.OfferCast y -> Just y; _ -> Nothing),
      Arm.payload "GrantPlayFromExile" GrantPlayFromExile.codec Effect.GrantPlayFromExile (\x -> case x of Effect.GrantPlayFromExile y -> Just y; _ -> Nothing),
      Arm.payload "ForEach" forEachCodec Effect.ForEach (\x -> case x of Effect.ForEach y -> Just y; _ -> Nothing)
    ]
  where
    createCodec = Create.codec cardCodec
    replaceCodec = Replace.codec (codec cardCodec)
    preventCodec = PreventNextDamage.codec (codec cardCodec)
    preventAllCodec = PreventAllDamage.codec (codec cardCodec)
    forEachCodec = ForEach.codec (codec cardCodec)
