module Pawl.Codec.ProjectedCharacteristics where

import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ProjectedCharacteristics as PC

toJson :: PC.ProjectedCharacteristics -> Value.Value
toJson pc =
  Common.object
    [ Common.pair "name" . CardName.toJson $ PC.name pc,
      Common.pair "supertypes" . Common.encodeSet Supertype.toJson $ PC.supertypes pc,
      Common.pair "keywords" . Common.encodeMultiset Keyword.toJson $ PC.keywords pc,
      Common.pair "colors" . Common.encodeSet Color.toJson $ PC.colors pc,
      Common.pair "power" . Common.encodeMaybe Common.integer $ PC.power pc,
      Common.pair "toughness" . Common.encodeMaybe Common.integer $ PC.toughness pc,
      Common.pair "loyalty" . Common.encodeMaybe Loyalty.toJson $ PC.loyalty pc,
      Common.pair "characteristicPT" . Common.encodeMaybe (\(p, t) -> Common.array [Quantity.toJson p, Quantity.toJson t]) $ PC.characteristicPT pc,
      Common.pair "cardTypes" . Common.encodeSet CardType.toJson $ PC.cardTypes pc,
      Common.pair "subtypes" . Common.encodeSet Subtype.toJson $ PC.subtypes pc,
      Common.pair "activatedAbilities" . Common.encodeList (ActivatedAbility.toJson Card.toJson) $ PC.activatedAbilities pc,
      Common.pair "replacementEffects" . Common.encodeList ReplacementEffect.toJson $ PC.replacementEffects pc,
      Common.pair "triggeredAbilities" . Common.encodeList (TriggeredAbility.toJson Card.toJson) $ PC.triggeredAbilities pc
    ]

fromJson :: Value.Value -> Either Text.Text PC.ProjectedCharacteristics
fromJson value = do
  ps <- Common.asObject value
  nm <- Common.field "name" ps >>= CardName.fromJson
  sups <- Common.field "supertypes" ps >>= Common.decodeSet Supertype.fromJson
  kws <- Common.field "keywords" ps >>= Common.decodeMultiset Keyword.fromJson
  cols <- Common.field "colors" ps >>= Common.decodeSet Color.fromJson
  -- power/toughness/characteristicPT are encoded as required keys (encodeMaybe
  -- writes JSON null for Nothing, never omits the key), so decoding them is
  -- Common.field (required) >>= Common.decodeMaybe (Null -> Nothing), exactly
  -- like every other field here -- not the optional Common.nullableField a
  -- truly-omittable key would need.
  pow <- Common.field "power" ps >>= Common.decodeMaybe Common.asInteger
  tou <- Common.field "toughness" ps >>= Common.decodeMaybe Common.asInteger
  loy <- Common.field "loyalty" ps >>= Common.decodeMaybe Loyalty.fromJson
  cda <- Common.field "characteristicPT" ps >>= Common.decodeMaybe Quantity.fromJsonPair
  cts <- Common.field "cardTypes" ps >>= Common.decodeSet CardType.fromJson
  subs <- Common.field "subtypes" ps >>= Common.decodeSet Subtype.fromJson
  acts <- Common.field "activatedAbilities" ps >>= Common.decodeList (ActivatedAbility.fromJson Card.fromJson)
  reps <- Common.field "replacementEffects" ps >>= Common.decodeList ReplacementEffect.fromJson
  trigs <- Common.field "triggeredAbilities" ps >>= Common.decodeList (TriggeredAbility.fromJson Card.fromJson)
  pure
    PC.MkProjectedCharacteristics
      { PC.name = nm,
        PC.supertypes = sups,
        PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.loyalty = loy,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }
