module Pawl.Codec.ProjectedCharacteristics where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  Common.object . concat $
    [ Common.requiredPair "name" CardName.toJson (PC.name pc),
      Common.optionalPair "supertypes" Set.empty (Common.encodeSet Supertype.toJson) (PC.supertypes pc),
      Common.optionalPair "keywords" Map.empty (Common.encodeMultiset Keyword.toJson) (PC.keywords pc),
      Common.optionalPair "colors" Set.empty (Common.encodeSet Color.toJson) (PC.colors pc),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Common.integer) (PC.power pc),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Common.integer) (PC.toughness pc),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe Loyalty.toJson) (PC.loyalty pc),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe (\(p, t) -> Common.array [Quantity.toJson p, Quantity.toJson t])) (PC.characteristicPT pc),
      Common.requiredPair "cardTypes" (Common.encodeSet CardType.toJson) (PC.cardTypes pc),
      Common.optionalPair "subtypes" Set.empty (Common.encodeSet Subtype.toJson) (PC.subtypes pc),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson Card.toJson)) (PC.activatedAbilities pc),
      Common.optionalPair "replacementEffects" [] (Common.encodeList ReplacementEffect.toJson) (PC.replacementEffects pc),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson Card.toJson)) (PC.triggeredAbilities pc)
    ]

fromJson :: Value.Value -> Either Text.Text PC.ProjectedCharacteristics
fromJson value = do
  ps <- Common.asObject value
  nm <- Common.field "name" ps >>= CardName.fromJson
  sups <- Common.defaultedField "supertypes" Set.empty (Common.decodeSet Supertype.fromJson) ps
  kws <- Common.defaultedField "keywords" Map.empty (Common.decodeMultiset Keyword.fromJson) ps
  cols <- Common.defaultedField "colors" Set.empty (Common.decodeSet Color.fromJson) ps
  pow <- Common.defaultedField "power" Nothing (Common.decodeMaybe Common.asInteger) ps
  tou <- Common.defaultedField "toughness" Nothing (Common.decodeMaybe Common.asInteger) ps
  loy <- Common.defaultedField "loyalty" Nothing (Common.decodeMaybe Loyalty.fromJson) ps
  cda <- Common.defaultedField "characteristicPT" Nothing (Common.decodeMaybe Quantity.fromJsonPair) ps
  cts <- Common.field "cardTypes" ps >>= Common.decodeSet CardType.fromJson
  subs <- Common.defaultedField "subtypes" Set.empty (Common.decodeSet Subtype.fromJson) ps
  acts <- Common.defaultedField "activatedAbilities" [] (Common.decodeList (ActivatedAbility.fromJson Card.fromJson)) ps
  reps <- Common.defaultedField "replacementEffects" [] (Common.decodeList ReplacementEffect.fromJson) ps
  trigs <- Common.defaultedField "triggeredAbilities" [] (Common.decodeList (TriggeredAbility.fromJson Card.fromJson)) ps
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
