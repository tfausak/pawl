module Pawl.Codec.DamageEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.DamageEvent as DamageEvent

toJson :: DamageEvent.DamageEvent -> Value.Value
toJson ev =
  Value.object
    ( Common.requiredPair "source" ObjectId.toJson (DamageEvent.source ev)
        <> Common.requiredPair "target" Recipient.toJson (DamageEvent.target ev)
        <> Common.requiredPair "amount" Common.encodeNatural (DamageEvent.amount ev)
        <> Common.optionalPair "dealtByDeathtouch" False Value.boolean (DamageEvent.dealtByDeathtouch ev)
        <> Common.optionalPair "dealtByInfect" False Value.boolean (DamageEvent.dealtByInfect ev)
        <> Common.optionalPair "dealtByWither" False Value.boolean (DamageEvent.dealtByWither ev)
        <> Common.optionalPair "dealtByToxic" 0 Common.encodeNatural (DamageEvent.dealtByToxic ev)
        -- CR 702.15b's answer is a player or nobody, so Nothing (no lifelink at
        -- deal time) is what an absent key means.
        <> Common.optionalPair "dealtByLifelink" Nothing (Common.encodeMaybe (Codec.encode PlayerId.codec)) (DamageEvent.dealtByLifelink ev)
        <> Common.requiredPair "kind" DamageKind.toJson (DamageEvent.kind ev)
    )

fromJson :: Value.Value -> Either Text.Text DamageEvent.DamageEvent
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "source" ps >>= ObjectId.fromJson
  t <- Common.field "target" ps >>= Recipient.fromJson
  a <- Common.field "amount" ps >>= Common.decodeNatural
  d <- Common.defaultedField "dealtByDeathtouch" False Common.asBoolean ps
  i <- Common.defaultedField "dealtByInfect" False Common.asBoolean ps
  w <- Common.defaultedField "dealtByWither" False Common.asBoolean ps
  x <- Common.defaultedField "dealtByToxic" 0 Common.decodeNatural ps
  l <- Common.defaultedField "dealtByLifelink" Nothing (Common.decodeMaybe (Codec.decode PlayerId.codec)) ps
  k <- Common.field "kind" ps >>= DamageKind.fromJson
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = s,
        DamageEvent.target = t,
        DamageEvent.amount = a,
        DamageEvent.dealtByDeathtouch = d,
        DamageEvent.dealtByInfect = i,
        DamageEvent.dealtByWither = w,
        DamageEvent.dealtByToxic = x,
        DamageEvent.dealtByLifelink = l,
        DamageEvent.kind = k
      }
