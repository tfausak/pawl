-- | The @ProjectedCharacteristics ⇆ Json@ codec (#481).
module Pawl.Codec.ProjectedCharacteristics where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ProjectedCharacteristics as PC

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Json.jObject
    [ (Text.pack "name", Json.jText (PC.name pc)),
      (Text.pack "supertypes", Json.setTo Supertype.toJson (PC.supertypes pc)),
      (Text.pack "keywords", Json.multisetTo Keyword.toJson (PC.keywords pc)),
      (Text.pack "colors", Json.setTo Color.toJson (PC.colors pc)),
      (Text.pack "power", Json.maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", Json.maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "loyalty", Json.maybeTo Loyalty.toJson (PC.loyalty pc)),
      (Text.pack "characteristicPT", Json.maybeTo (\(p, t) -> Array (MkArray [Quantity.toJson p, Quantity.toJson t])) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", Json.setTo CardType.toJson (PC.cardTypes pc)),
      (Text.pack "subtypes", Json.setTo Subtype.toJson (PC.subtypes pc)),
      (Text.pack "activatedAbilities", Json.listTo (ActivatedAbility.toJson Card.toJson) (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", Json.listTo ReplacementEffect.toJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", Json.listTo (TriggeredAbility.toJson Card.toJson) (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  nm <- Json.field (Text.pack "name") ps >>= Json.asText
  sups <- Json.field (Text.pack "supertypes") ps >>= Json.setFrom Supertype.fromJson
  kws <- Json.field (Text.pack "keywords") ps >>= Json.multisetFrom Keyword.fromJson
  cols <- Json.field (Text.pack "colors") ps >>= Json.setFrom Color.fromJson
  -- power/toughness/characteristicPT are encoded as required keys (Json.maybeTo
  -- writes JSON null for Nothing, never omits the key), so decoding them is
  -- Json.field (required) >>= Json.maybeFrom (Null -> Nothing), exactly like every
  -- other field here -- not the optional Json.getOpt a truly-omittable key would need.
  pow <- Json.field (Text.pack "power") ps >>= Json.maybeFrom Json.asInteger
  tou <- Json.field (Text.pack "toughness") ps >>= Json.maybeFrom Json.asInteger
  loy <- Json.field (Text.pack "loyalty") ps >>= Json.maybeFrom Loyalty.fromJson
  cda <- Json.field (Text.pack "characteristicPT") ps >>= Json.maybeFrom Quantity.fromJsonPair
  cts <- Json.field (Text.pack "cardTypes") ps >>= Json.setFrom CardType.fromJson
  subs <- Json.field (Text.pack "subtypes") ps >>= Json.setFrom Subtype.fromJson
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (ActivatedAbility.fromJson Card.fromJson)
  reps <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom ReplacementEffect.fromJson
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (TriggeredAbility.fromJson Card.fromJson)
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
