-- | The @ProjectedCharacteristics ⇆ Json@ codec (#481).
module Pawl.Codec.ProjectedCharacteristics where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.ActivatedAbility (activatedAbilityToJson, jsonToActivatedAbility)
import Pawl.Codec.Card (cardToJson, jsonToCard)
import Pawl.Codec.CardType (cardTypeToJson, jsonToCardType)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.Quantity (jsonToQuantityPair, quantityToJson)
import Pawl.Codec.ReplacementEffect (jsonToReplacementEffect, replacementEffectToJson)
import Pawl.Codec.Subtype (jsonToSubtype, subtypeToJson)
import Pawl.Codec.Supertype (jsonToSupertype, supertypeToJson)
import Pawl.Codec.TriggeredAbility (jsonToTriggeredAbility, triggeredAbilityToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.ProjectedCharacteristics as PC

projectedCharacteristicsToJson :: PC.ProjectedCharacteristics -> Value
projectedCharacteristicsToJson pc =
  Json.jObject
    [ (Text.pack "name", Json.jText (PC.name pc)),
      (Text.pack "supertypes", Json.setTo supertypeToJson (PC.supertypes pc)),
      (Text.pack "keywords", Json.multisetTo keywordToJson (PC.keywords pc)),
      (Text.pack "colors", Json.setTo colorToJson (PC.colors pc)),
      (Text.pack "power", Json.maybeTo Json.jInt (PC.power pc)),
      (Text.pack "toughness", Json.maybeTo Json.jInt (PC.toughness pc)),
      (Text.pack "characteristicPT", Json.maybeTo (\(p, t) -> Array (MkArray [quantityToJson p, quantityToJson t])) (PC.characteristicPT pc)),
      (Text.pack "cardTypes", Json.setTo cardTypeToJson (PC.cardTypes pc)),
      (Text.pack "subtypes", Json.setTo subtypeToJson (PC.subtypes pc)),
      (Text.pack "activatedAbilities", Json.listTo (activatedAbilityToJson cardToJson) (PC.activatedAbilities pc)),
      (Text.pack "replacementEffects", Json.listTo replacementEffectToJson (PC.replacementEffects pc)),
      (Text.pack "triggeredAbilities", Json.listTo (triggeredAbilityToJson cardToJson) (PC.triggeredAbilities pc))
    ]

jsonToProjectedCharacteristics :: Value -> Either Text PC.ProjectedCharacteristics
jsonToProjectedCharacteristics value = do
  ps <- Json.asObject value
  nm <- Json.field (Text.pack "name") ps >>= Json.asText
  sups <- Json.field (Text.pack "supertypes") ps >>= Json.setFrom jsonToSupertype
  kws <- Json.field (Text.pack "keywords") ps >>= Json.multisetFrom jsonToKeyword
  cols <- Json.field (Text.pack "colors") ps >>= Json.setFrom jsonToColor
  -- power/toughness/characteristicPT are encoded as required keys (Json.maybeTo
  -- writes JSON null for Nothing, never omits the key), so decoding them is
  -- Json.field (required) >>= Json.maybeFrom (Null -> Nothing), exactly like every
  -- other field here -- not the optional Json.getOpt a truly-omittable key would need.
  pow <- Json.field (Text.pack "power") ps >>= Json.maybeFrom Json.asInteger
  tou <- Json.field (Text.pack "toughness") ps >>= Json.maybeFrom Json.asInteger
  cda <- Json.field (Text.pack "characteristicPT") ps >>= Json.maybeFrom jsonToQuantityPair
  cts <- Json.field (Text.pack "cardTypes") ps >>= Json.setFrom jsonToCardType
  subs <- Json.field (Text.pack "subtypes") ps >>= Json.setFrom jsonToSubtype
  acts <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (jsonToActivatedAbility jsonToCard)
  reps <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom jsonToReplacementEffect
  trigs <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (jsonToTriggeredAbility jsonToCard)
  pure
    PC.MkProjectedCharacteristics
      { PC.name = nm,
        PC.supertypes = sups,
        PC.keywords = kws,
        PC.colors = cols,
        PC.power = pow,
        PC.toughness = tou,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }
