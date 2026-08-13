-- | Where the effect knot is tied. The card codec is a PARAMETER, so this module
-- names no concrete card type; Pawl.Codec.Card passes its own codec in.
--
-- RECURSIVE twice over: PreventNextDamage's CR 615.5 rider holds effects, and
-- CreateEmblem's payload is a whole card whose faces hold effects. Both name
-- 'codec' inside its own definition, which terminates for Pawl.Codec.TriggerCondition's
-- reason -- 'Arm.tagged' reaches WHNF without forcing its arm list, and
-- 'Define.define' registers the type's name before running the schema body.
--
-- Casing on an effect's identity here is open-half machinery, not the rules core.
module Pawl.Codec.Effect where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.AffectPlayers as AffectPlayers
import qualified Pawl.Codec.ArmDelayedTrigger as ArmDelayedTrigger
import qualified Pawl.Codec.AttachTarget as AttachTarget
import qualified Pawl.Codec.ChangeText as ChangeText
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
import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.Mill as Mill
import qualified Pawl.Codec.ModifyTarget as ModifyTarget
import qualified Pawl.Codec.MonarchTarget as MonarchTarget
import qualified Pawl.Codec.MoveToZone as MoveToZone
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.OfferCast as OfferCast
import qualified Pawl.Codec.PlayerCounters as PlayerCounters
import qualified Pawl.Codec.PlayerQuantity as PlayerQuantity
import qualified Pawl.Codec.PlayerSacrifices as PlayerSacrifices
import qualified Pawl.Codec.PreventNextDamage as PreventNextDamage
import qualified Pawl.Codec.PutCounters as PutCounters
import qualified Pawl.Codec.RedirectDamage as RedirectDamage
import qualified Pawl.Codec.RemoveCounters as RemoveCounters
import qualified Pawl.Codec.Replace as Replace
import qualified Pawl.Codec.RequireBlock as RequireBlock
import qualified Pawl.Codec.Search as Search
import qualified Pawl.Codec.ShuffleIntoLibrary as ShuffleIntoLibrary
import qualified Pawl.Codec.SkipNextPhase as SkipNextPhase
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.SpeedDecrease as SpeedDecrease
import qualified Pawl.Codec.TakeExtraTurn as TakeExtraTurn
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Effect as Effect

codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Effect.Effect card)
codec cardCodec =
  Arm.tagged
    encode
    [ Arm.payload "DealDamage" DealDamage.codec Effect.DealDamage,
      Arm.payload "ModifyTarget" ModifyTarget.codec Effect.ModifyTarget,
      Arm.payload "ChangeText" ChangeText.codec Effect.ChangeText,
      Arm.payload "AddMana" ManaProduction.codec Effect.AddMana,
      Arm.payload "Search" Search.codec Effect.Search,
      Arm.nullary "ExileAllGraveyards" Effect.ExileAllGraveyards,
      Arm.nullary "Proliferate" Effect.Proliferate,
      Arm.nullary "TemptWithTheRing" Effect.TemptWithTheRing,
      Arm.nullary "Venture" Effect.Venture,
      Arm.nullary "ExileHandThenDraw" Effect.ExileHandThenDraw,
      Arm.payload "PlayerSacrifices" PlayerSacrifices.codec Effect.PlayerSacrifices,
      Arm.nullary "RestartGame" Effect.RestartGame,
      Arm.payload "ControlPlayerNextTurn" SlotName.codec Effect.ControlPlayerNextTurn,
      Arm.payload "Destroy" Destroy.codec Effect.Destroy,
      Arm.payload "Sacrifice" SlotName.codec Effect.Sacrifice,
      Arm.payload "TurnFaceDown" SlotName.codec Effect.TurnFaceDown,
      Arm.payload "RemoveFromCombat" SlotName.codec Effect.RemoveFromCombat,
      Arm.payload "Counter" SlotName.codec Effect.Counter,
      Arm.payload "MoveToZone" MoveToZone.codec Effect.MoveToZone,
      Arm.payload "Draw" PlayerQuantity.codec Effect.Draw,
      Arm.payload "Scry" PlayerQuantity.codec Effect.Scry,
      Arm.payload "Surveil" PlayerQuantity.codec Effect.Surveil,
      Arm.payload "Fateseal" PlayerQuantity.codec Effect.Fateseal,
      Arm.payload "Explore" ObjectRef.codec Effect.Explore,
      Arm.payload "Mill" Mill.codec Effect.Mill,
      Arm.payload "Discard" Discard.codec Effect.Discard,
      Arm.payload "LoseLife" PlayerQuantity.codec Effect.LoseLife,
      Arm.payload "GainLife" PlayerQuantity.codec Effect.GainLife,
      Arm.payload "ExchangeLifeTotals" ExchangeSides.codec Effect.ExchangeLifeTotals,
      Arm.payload "SetLifeTotal" PlayerQuantity.codec Effect.SetLifeTotal,
      Arm.nullary "RedistributeLifeTotals" Effect.RedistributeLifeTotals,
      Arm.payload "IncreaseSpeed" PlayerQuantity.codec Effect.IncreaseSpeed,
      Arm.payload "DecreaseSpeed" SpeedDecrease.codec Effect.DecreaseSpeed,
      Arm.payload "Create" createCodec Effect.Create,
      Arm.payload "CreateCopy" CreateCopy.codec Effect.CreateCopy,
      Arm.payload "Replace" Replace.codec Effect.Replace,
      Arm.payload "SkipNextPhase" SkipNextPhase.codec Effect.SkipNextPhase,
      Arm.payload "PreventNextDamage" preventCodec Effect.PreventNextDamage,
      Arm.payload "PreventAllDamage" DurationRef.codec Effect.PreventAllDamage,
      Arm.payload "RedirectDamage" RedirectDamage.codec Effect.RedirectDamage,
      Arm.payload "PutCounters" PutCounters.codec Effect.PutCounters,
      Arm.payload "RemoveCounters" RemoveCounters.codec Effect.RemoveCounters,
      Arm.payload "GainPlayerCounters" PlayerCounters.codec Effect.GainPlayerCounters,
      Arm.payload "RemovePlayerCounters" PlayerCounters.codec Effect.RemovePlayerCounters,
      Arm.payload "Tap" ObjectRef.codec Effect.Tap,
      Arm.payload "Untap" ObjectRef.codec Effect.Untap,
      Arm.payload "Transform" ObjectRef.codec Effect.Transform,
      Arm.payload "AddPhases" (Common.list ExtraPhase.codec) Effect.AddPhases,
      Arm.payload "GainControl" DurationRef.codec Effect.GainControl,
      Arm.payload "ArmDelayedTrigger" ArmDelayedTrigger.codec Effect.ArmDelayedTrigger,
      Arm.payload "AffectPlayers" AffectPlayers.codec Effect.AffectPlayers,
      Arm.payload "RequireBlock" RequireBlock.codec Effect.RequireBlock,
      Arm.payload "CreateEmblem" cardCodec Effect.CreateEmblem,
      Arm.payload "BecomeMonarch" MonarchTarget.codec Effect.BecomeMonarch,
      Arm.payload "Designate" Designate.codec Effect.Designate,
      Arm.payload "Unsuspect" ObjectRef.codec Effect.Unsuspect,
      Arm.payload "Evolve" SlotName.codec Effect.Evolve,
      Arm.payload "Mentor" SlotName.codec Effect.Mentor,
      Arm.payload "ItBecomes" Daytime.codec Effect.ItBecomes,
      Arm.payload "ExileUntilMonarch" SlotName.codec Effect.ExileUntilMonarch,
      Arm.payload "ExileHaunting" ExileHaunting.codec Effect.ExileHaunting,
      Arm.payload "Attach" SlotName.codec Effect.Attach,
      Arm.payload "AttachTarget" AttachTarget.codec Effect.AttachTarget,
      Arm.payload "PlaySubgame" SlotName.codec Effect.PlaySubgame,
      Arm.payload "ChooseOpponent" SlotName.codec Effect.ChooseOpponent,
      Arm.payload "TakeExtraTurn" TakeExtraTurn.codec Effect.TakeExtraTurn,
      Arm.payload "ShuffleIntoLibrary" ShuffleIntoLibrary.codec Effect.ShuffleIntoLibrary,
      Arm.payload "OfferCast" OfferCast.codec Effect.OfferCast,
      Arm.payload "GrantPlayFromExile" DurationRef.codec Effect.GrantPlayFromExile
    ]
  where
    createCodec = Create.codec cardCodec
    preventCodec = PreventNextDamage.codec (codec cardCodec)
    tag t = Common.tagged t . Just
    encode e = case e of
      Effect.DealDamage x -> tag "DealDamage" $ Codec.encode DealDamage.codec x
      Effect.ModifyTarget x -> tag "ModifyTarget" $ Codec.encode ModifyTarget.codec x
      Effect.ChangeText x -> tag "ChangeText" $ Codec.encode ChangeText.codec x
      Effect.AddMana x -> tag "AddMana" $ Codec.encode ManaProduction.codec x
      Effect.Search x -> tag "Search" $ Codec.encode Search.codec x
      Effect.ExileAllGraveyards -> Common.nullary "ExileAllGraveyards"
      Effect.Proliferate -> Common.nullary "Proliferate"
      Effect.TemptWithTheRing -> Common.nullary "TemptWithTheRing"
      Effect.Venture -> Common.nullary "Venture"
      Effect.ExileHandThenDraw -> Common.nullary "ExileHandThenDraw"
      Effect.PlayerSacrifices x -> tag "PlayerSacrifices" $ Codec.encode PlayerSacrifices.codec x
      Effect.RestartGame -> Common.nullary "RestartGame"
      Effect.ControlPlayerNextTurn x -> tag "ControlPlayerNextTurn" $ Codec.encode SlotName.codec x
      Effect.Destroy x -> tag "Destroy" $ Codec.encode Destroy.codec x
      Effect.Sacrifice x -> tag "Sacrifice" $ Codec.encode SlotName.codec x
      Effect.TurnFaceDown x -> tag "TurnFaceDown" $ Codec.encode SlotName.codec x
      Effect.RemoveFromCombat x -> tag "RemoveFromCombat" $ Codec.encode SlotName.codec x
      Effect.Counter x -> tag "Counter" $ Codec.encode SlotName.codec x
      Effect.MoveToZone x -> tag "MoveToZone" $ Codec.encode MoveToZone.codec x
      Effect.Draw x -> tag "Draw" $ Codec.encode PlayerQuantity.codec x
      Effect.Scry x -> tag "Scry" $ Codec.encode PlayerQuantity.codec x
      Effect.Surveil x -> tag "Surveil" $ Codec.encode PlayerQuantity.codec x
      Effect.Fateseal x -> tag "Fateseal" $ Codec.encode PlayerQuantity.codec x
      Effect.Explore x -> tag "Explore" $ Codec.encode ObjectRef.codec x
      Effect.Mill x -> tag "Mill" $ Codec.encode Mill.codec x
      Effect.Discard x -> tag "Discard" $ Codec.encode Discard.codec x
      Effect.LoseLife x -> tag "LoseLife" $ Codec.encode PlayerQuantity.codec x
      Effect.GainLife x -> tag "GainLife" $ Codec.encode PlayerQuantity.codec x
      Effect.ExchangeLifeTotals x -> tag "ExchangeLifeTotals" $ Codec.encode ExchangeSides.codec x
      Effect.SetLifeTotal x -> tag "SetLifeTotal" $ Codec.encode PlayerQuantity.codec x
      Effect.RedistributeLifeTotals -> Common.nullary "RedistributeLifeTotals"
      Effect.IncreaseSpeed x -> tag "IncreaseSpeed" $ Codec.encode PlayerQuantity.codec x
      Effect.DecreaseSpeed x -> tag "DecreaseSpeed" $ Codec.encode SpeedDecrease.codec x
      Effect.Create x -> tag "Create" $ Codec.encode createCodec x
      Effect.CreateCopy x -> tag "CreateCopy" $ Codec.encode CreateCopy.codec x
      Effect.Replace x -> tag "Replace" $ Codec.encode Replace.codec x
      Effect.SkipNextPhase x -> tag "SkipNextPhase" $ Codec.encode SkipNextPhase.codec x
      Effect.PreventNextDamage x -> tag "PreventNextDamage" $ Codec.encode preventCodec x
      Effect.PreventAllDamage x -> tag "PreventAllDamage" $ Codec.encode DurationRef.codec x
      Effect.RedirectDamage x -> tag "RedirectDamage" $ Codec.encode RedirectDamage.codec x
      Effect.PutCounters x -> tag "PutCounters" $ Codec.encode PutCounters.codec x
      Effect.RemoveCounters x -> tag "RemoveCounters" $ Codec.encode RemoveCounters.codec x
      Effect.GainPlayerCounters x -> tag "GainPlayerCounters" $ Codec.encode PlayerCounters.codec x
      Effect.RemovePlayerCounters x -> tag "RemovePlayerCounters" $ Codec.encode PlayerCounters.codec x
      Effect.Tap x -> tag "Tap" $ Codec.encode ObjectRef.codec x
      Effect.Untap x -> tag "Untap" $ Codec.encode ObjectRef.codec x
      Effect.Transform x -> tag "Transform" $ Codec.encode ObjectRef.codec x
      Effect.AddPhases x -> tag "AddPhases" $ Codec.encode (Common.list ExtraPhase.codec) x
      Effect.GainControl x -> tag "GainControl" $ Codec.encode DurationRef.codec x
      Effect.ArmDelayedTrigger x -> tag "ArmDelayedTrigger" $ Codec.encode ArmDelayedTrigger.codec x
      Effect.AffectPlayers x -> tag "AffectPlayers" $ Codec.encode AffectPlayers.codec x
      Effect.RequireBlock x -> tag "RequireBlock" $ Codec.encode RequireBlock.codec x
      Effect.CreateEmblem x -> tag "CreateEmblem" $ Codec.encode cardCodec x
      Effect.BecomeMonarch x -> tag "BecomeMonarch" $ Codec.encode MonarchTarget.codec x
      Effect.Designate x -> tag "Designate" $ Codec.encode Designate.codec x
      Effect.Unsuspect x -> tag "Unsuspect" $ Codec.encode ObjectRef.codec x
      Effect.Evolve x -> tag "Evolve" $ Codec.encode SlotName.codec x
      Effect.Mentor x -> tag "Mentor" $ Codec.encode SlotName.codec x
      Effect.ItBecomes x -> tag "ItBecomes" $ Codec.encode Daytime.codec x
      Effect.ExileUntilMonarch x -> tag "ExileUntilMonarch" $ Codec.encode SlotName.codec x
      Effect.ExileHaunting x -> tag "ExileHaunting" $ Codec.encode ExileHaunting.codec x
      Effect.Attach x -> tag "Attach" $ Codec.encode SlotName.codec x
      Effect.AttachTarget x -> tag "AttachTarget" $ Codec.encode AttachTarget.codec x
      Effect.PlaySubgame x -> tag "PlaySubgame" $ Codec.encode SlotName.codec x
      Effect.ChooseOpponent x -> tag "ChooseOpponent" $ Codec.encode SlotName.codec x
      Effect.TakeExtraTurn x -> tag "TakeExtraTurn" $ Codec.encode TakeExtraTurn.codec x
      Effect.ShuffleIntoLibrary x -> tag "ShuffleIntoLibrary" $ Codec.encode ShuffleIntoLibrary.codec x
      Effect.OfferCast x -> tag "OfferCast" $ Codec.encode OfferCast.codec x
      Effect.GrantPlayFromExile x -> tag "GrantPlayFromExile" $ Codec.encode DurationRef.codec x
