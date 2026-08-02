module Pawl.Codec.DamageEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DamageEvent as DamageEvent

toJson :: DamageEvent.DamageEvent -> Value.Value
toJson ev =
  Common.object
    [ Common.pair "source" $ ObjectId.toJson (DamageEvent.source ev),
      Common.pair "target" $ Recipient.toJson (DamageEvent.target ev),
      Common.pair "amount" $ Common.encodeNatural (DamageEvent.amount ev),
      Common.pair "dealtByDeathtouch" . Common.boolean $ DamageEvent.dealtByDeathtouch ev,
      Common.pair "dealtByInfect" . Common.boolean $ DamageEvent.dealtByInfect ev,
      Common.pair "dealtByToxic" $ Common.encodeNatural (DamageEvent.dealtByToxic ev),
      -- CR 702.15b's answer is a player or nobody, so JSON null is Nothing.
      Common.pair "dealtByLifelink" $ Common.encodeMaybe PlayerId.toJson (DamageEvent.dealtByLifelink ev),
      Common.pair "kind" $ DamageKind.toJson (DamageEvent.kind ev)
    ]

fromJson :: Value.Value -> Either Text.Text DamageEvent.DamageEvent
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "source" ps >>= ObjectId.fromJson
  t <- Common.field "target" ps >>= Recipient.fromJson
  a <- Common.field "amount" ps >>= Common.decodeNatural
  d <- Common.field "dealtByDeathtouch" ps >>= Common.decodeBooleanDefault False
  i <- Common.field "dealtByInfect" ps >>= Common.decodeBooleanDefault False
  x <- Common.field "dealtByToxic" ps >>= Common.decodeNatural
  l <- Common.field "dealtByLifelink" ps >>= Common.decodeMaybe PlayerId.fromJson
  k <- Common.field "kind" ps >>= DamageKind.fromJson
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
