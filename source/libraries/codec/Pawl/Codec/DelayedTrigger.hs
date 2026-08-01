-- | The @DelayedTrigger ⇆ Json@ codec (#481).
module Pawl.Codec.DelayedTrigger where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Binding (bindingsToJson, jsonToBindings)
import Pawl.Codec.Card (cardToJson, jsonToCard)
import Pawl.Codec.Expiry (expiryToJson, jsonToExpiry)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Codec.TriggeredAbility (jsonToTriggeredAbility, triggeredAbilityToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger

delayedTriggerToJson :: DelayedTrigger.DelayedTrigger -> Value
delayedTriggerToJson d =
  Json.jObject
    [ (Text.pack "ability", triggeredAbilityToJson cardToJson (DelayedTrigger.ability d)),
      (Text.pack "source", objectIdToJson (DelayedTrigger.source d)),
      (Text.pack "controller", playerIdToJson (DelayedTrigger.controller d)),
      (Text.pack "bindings", bindingsToJson (DelayedTrigger.bindings d)),
      -- CR 603.7a: absent for an ability armed with no onset gate, which is the
      -- rule's default and every entry in the pool but Meandering Towershell's.
      (Text.pack "notBefore", Json.maybeTo Json.natTo (DelayedTrigger.notBefore d)),
      -- CR 603.7b: absent for an ability with no stated duration, which is the
      -- rule's default and every entry in the pool but Full Throttle's.
      (Text.pack "expiry", Json.maybeTo expiryToJson (DelayedTrigger.expiry d))
    ]

jsonToDelayedTrigger :: Value -> Either Text DelayedTrigger.DelayedTrigger
jsonToDelayedTrigger value = do
  ps <- Json.asObject value
  a <- Json.field (Text.pack "ability") ps >>= jsonToTriggeredAbility jsonToCard
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  b <- Json.field (Text.pack "bindings") ps >>= jsonToBindings
  n <- Json.maybeFrom Json.natFrom (Json.getOpt (Text.pack "notBefore") ps)
  e <- Json.maybeFrom jsonToExpiry (Json.getOpt (Text.pack "expiry") ps)
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
