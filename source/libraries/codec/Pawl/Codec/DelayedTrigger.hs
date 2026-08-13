module Pawl.Codec.DelayedTrigger where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Binding as Binding
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Codec.TurnWindow as TurnWindow
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger

toJson :: DelayedTrigger.DelayedTrigger -> Value.Value
toJson d =
  Value.object . concat $
    [ Common.requiredPair "ability" (Codec.encode (TriggeredAbility.codec Card.codec)) (DelayedTrigger.ability d),
      Common.requiredPair "source" (Codec.encode ObjectId.codec) (DelayedTrigger.source d),
      Common.requiredPair "controller" (Codec.encode PlayerId.codec) (DelayedTrigger.controller d),
      Common.optionalPair "bindings" Map.empty Binding.toJsonMap (DelayedTrigger.bindings d),
      -- CR 603.7a: TurnWindow.AnyTurn for an ability armed with no onset gate.
      -- Always present, unlike the expiry below, because "no restriction" is
      -- one of the windows rather than the absence of one.
      Common.requiredPair "window" (Codec.encode TurnWindow.codec) (DelayedTrigger.window d),
      -- CR 603.7b: absent for an ability with no stated duration.
      Common.optionalPair "expiry" Nothing (Common.encodeMaybe Expiry.toJson) (DelayedTrigger.expiry d)
    ]

fromJson :: Value.Value -> Either Text.Text DelayedTrigger.DelayedTrigger
fromJson value = do
  ps <- Common.asObject value
  a <- Common.field "ability" ps >>= Codec.decode (TriggeredAbility.codec Card.codec)
  s <- Common.field "source" ps >>= Codec.decode ObjectId.codec
  c <- Common.field "controller" ps >>= Codec.decode PlayerId.codec
  b <- Common.defaultedField "bindings" Map.empty Binding.fromJsonMap ps
  w <- Common.field "window" ps >>= Codec.decode TurnWindow.codec
  e <- Common.defaultedField "expiry" Nothing (Common.decodeMaybe Expiry.fromJson) ps
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b,
        DelayedTrigger.window = w,
        DelayedTrigger.expiry = e
      }
