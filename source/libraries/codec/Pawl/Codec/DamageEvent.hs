-- | The @DamageEvent ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.DamageEvent where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.DamageKind (damageKindToJson, jsonToDamageKind)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.Recipient (jsonToRecipient, recipientToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.DamageEvent as DamageEvent

damageEventToJson :: DamageEvent.DamageEvent -> Value
damageEventToJson ev =
  Json.jObject
    [ (Text.pack "source", objectIdToJson (DamageEvent.source ev)),
      (Text.pack "target", recipientToJson (DamageEvent.target ev)),
      (Text.pack "amount", Json.natTo (DamageEvent.amount ev)),
      (Text.pack "dealtByDeathtouch", Json.jBool (DamageEvent.dealtByDeathtouch ev)),
      (Text.pack "dealtByInfect", Json.jBool (DamageEvent.dealtByInfect ev)),
      (Text.pack "dealtByToxic", Json.natTo (DamageEvent.dealtByToxic ev)),
      (Text.pack "kind", damageKindToJson (DamageEvent.kind ev))
    ]

jsonToDamageEvent :: Value -> Either Text DamageEvent.DamageEvent
jsonToDamageEvent value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  t <- Json.field (Text.pack "target") ps >>= jsonToRecipient
  a <- Json.field (Text.pack "amount") ps >>= Json.natFrom
  d <- Json.field (Text.pack "dealtByDeathtouch") ps >>= Json.jsonToBoolDefault False
  i <- Json.field (Text.pack "dealtByInfect") ps >>= Json.jsonToBoolDefault False
  x <- Json.field (Text.pack "dealtByToxic") ps >>= Json.natFrom
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.dealtByInfect = i,
        DamageEvent.dealtByToxic = x,
        DamageEvent.kind = k
      }
