-- | The @DamageEvent ⇆ Json@ codec (#481).
module Pawl.Codec.DamageEvent where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.DamageKind (damageKindToJson, jsonToDamageKind)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Codec.Recipient (jsonToRecipient, recipientToJson)
import Pawl.Json.Value (Value (Null))
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.PlayerId as PlayerId

damageEventToJson :: DamageEvent.DamageEvent -> Value
damageEventToJson ev =
  Json.jObject
    [ (Text.pack "source", objectIdToJson (DamageEvent.source ev)),
      (Text.pack "target", recipientToJson (DamageEvent.target ev)),
      (Text.pack "amount", Json.natTo (DamageEvent.amount ev)),
      (Text.pack "dealtByDeathtouch", Json.jBool (DamageEvent.dealtByDeathtouch ev)),
      (Text.pack "dealtByInfect", Json.jBool (DamageEvent.dealtByInfect ev)),
      (Text.pack "dealtByToxic", Json.natTo (DamageEvent.dealtByToxic ev)),
      (Text.pack "dealtByLifelink", maybe Json.jNull playerIdToJson (DamageEvent.dealtByLifelink ev)),
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
  l <- Json.field (Text.pack "dealtByLifelink") ps >>= optionalPlayerId
  k <- Json.field (Text.pack "kind") ps >>= jsonToDamageKind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.dealtByInfect = i,
        DamageEvent.dealtByToxic = x,
        DamageEvent.dealtByLifelink = l,
        DamageEvent.kind = k
      }

-- CR 702.15b's answer is a player or nobody, so JSON null is Nothing -- the
-- optionalFilter shape, not a Bool-with-a-default one, because the payload it
-- guards is a value rather than a flag.
optionalPlayerId :: Value -> Either Text (Maybe PlayerId.PlayerId)
optionalPlayerId value = case value of
  Null _ -> Right Nothing
  _ -> fmap Just (jsonToPlayerId value)
