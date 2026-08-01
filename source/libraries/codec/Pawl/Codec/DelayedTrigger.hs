-- | The @DelayedTrigger ⇆ Json@ codec (#481).
module Pawl.Codec.DelayedTrigger where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Binding (bindingsToJson, jsonToBindings)
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Json.jObject
    [ (Text.pack "ability", TriggeredAbility.toJson Card.toJson (DelayedTrigger.ability d)),
      (Text.pack "source", ObjectId.toJson (DelayedTrigger.source d)),
      (Text.pack "controller", PlayerId.toJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d)),
      -- CR 603.7a: absent for an ability armed with no onset gate, which is the
      -- rule's default and every entry in the pool but Meandering Towershell's.
      (Text.pack "notBefore", Json.maybeTo Json.natTo (DelayedTrigger.notBefore d)),
      -- CR 603.7b: absent for an ability with no stated duration, which is the
      -- rule's default and every entry in the pool but Full Throttle's.
      (Text.pack "expiry", Json.maybeTo Expiry.toJson (DelayedTrigger.expiry d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= TriggeredAbility.fromJson Card.fromJson
  s <- Json.field (Text.pack "source") ps >>= ObjectId.fromJson
  c <- Json.field (Text.pack "controller") ps >>= PlayerId.fromJson
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  n <- Json.maybeFrom Json.natFrom (Json.getOpt (Text.pack "notBefore") ps)
  e <- Json.maybeFrom Expiry.fromJson (Json.getOpt (Text.pack "expiry") ps)
  pure
    DelayedTrigger.MkDelayedTrigger
      { DelayedTrigger.ability = a,
        DelayedTrigger.source = s,
        DelayedTrigger.controller = c,
        DelayedTrigger.bindings = b,
        DelayedTrigger.notBefore = n,
        DelayedTrigger.expiry = e
      }

-- Modal -----------------------------------------------------------------------
