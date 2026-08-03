module Pawl.Codec.DelayedTrigger where

import qualified Data.Text as Text
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Codec.TurnWindow as TurnWindow
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger

toJson :: DelayedTrigger.DelayedTrigger -> Value.Value
toJson d =
  Common.object
    [ Common.pair "ability" $ TriggeredAbility.toJson Card.toJson (DelayedTrigger.ability d),
      Common.pair "source" $ ObjectId.toJson (DelayedTrigger.source d),
      Common.pair "controller" $ PlayerId.toJson (DelayedTrigger.controller d),
      Common.pair "bindings" $ Binding.toJsonMap (DelayedTrigger.bindings d),
      -- CR 603.7a: TurnWindow.AnyTurn for an ability armed with no onset gate,
      -- which is the rule's default and every entry in the pool but Meandering
      -- Towershell's. Always present, unlike the expiry below: "no restriction"
      -- is one of the windows rather than the absence of one.
      Common.pair "window" $ TurnWindow.toJson (DelayedTrigger.window d),
      -- CR 603.7b: absent for an ability with no stated duration, which is the
      -- rule's default and every entry in the pool but Full Throttle's.
      Common.pair "expiry" . Common.encodeMaybe Expiry.toJson $ DelayedTrigger.expiry d
    ]

fromJson :: Value.Value -> Either Text.Text DelayedTrigger.DelayedTrigger
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "ability" ps >>= TriggeredAbility.fromJson Card.fromJson
  s <- Common.field "source" ps >>= ObjectId.fromJson
  c <- Common.field "controller" ps >>= PlayerId.fromJson
  b <- Common.field "bindings" ps >>= Binding.fromJsonMap
  w <- Common.field "window" ps >>= TurnWindow.fromJson
  e <- Common.decodeMaybe Expiry.fromJson (Common.nullableField "expiry" ps)
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b,
        DelayedTrigger.window = w,
        DelayedTrigger.expiry = e
      }
