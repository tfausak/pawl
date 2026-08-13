module Pawl.Codec.ProjectedCharacteristics where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ProjectedCharacteristics as PC

toJson :: PC.ProjectedCharacteristics -> Value.Value
toJson pc =
  Value.object . concat $
    [ Common.requiredPair "names" (Common.encodeSet (Codec.encode CardName.codec)) (PC.names pc),
      Common.optionalPair "supertypes" Set.empty (Common.encodeSet (Codec.encode Supertype.codec)) (PC.supertypes pc),
      Common.optionalPair "keywords" Map.empty (Common.encodeMultiset (Codec.encode Keyword.codec)) (PC.keywords pc),
      Common.optionalPair "colors" Set.empty (Common.encodeSet (Codec.encode Color.codec)) (PC.colors pc),
      Common.optionalPair "manaValue" Nothing (Common.encodeMaybe Value.integer) (PC.manaValue pc),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Value.integer) (PC.power pc),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Value.integer) (PC.toughness pc),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe (Codec.encode Loyalty.codec)) (PC.loyalty pc),
      Common.optionalPair "defense" Nothing (Common.encodeMaybe (Codec.encode Defense.codec)) (PC.defense pc),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe (\(p, t) -> Value.array [Codec.encode Quantity.codec p, Codec.encode Quantity.codec t])) (PC.characteristicPT pc),
      Common.requiredPair "cardTypes" (Common.encodeSet (Codec.encode CardType.codec)) (PC.cardTypes pc),
      Common.optionalPair "subtypes" Set.empty (Common.encodeSet (Codec.encode Subtype.codec)) (PC.subtypes pc),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson Card.toJson)) (PC.activatedAbilities pc),
      Common.optionalPair "replacementEffects" [] (Common.encodeList ReplacementEffect.toJson) (PC.replacementEffects pc),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson Card.toJson)) (PC.triggeredAbilities pc)
    ]

fromJson :: Value.Value -> Either Text.Text PC.ProjectedCharacteristics
fromJson value = do
  ps <- Common.asObject value
  nms <- Common.field "names" ps >>= Common.decodeSet (Codec.decode CardName.codec)
  sups <- Common.defaultedField "supertypes" Set.empty (Common.decodeSet (Codec.decode Supertype.codec)) ps
  kws <- Common.defaultedField "keywords" Map.empty (Common.decodeMultiset (Codec.decode Keyword.codec)) ps
  cols <- Common.defaultedField "colors" Set.empty (Common.decodeSet (Codec.decode Color.codec)) ps
  mv <- Common.defaultedField "manaValue" Nothing (Common.decodeMaybe Common.asInteger) ps
  pow <- Common.defaultedField "power" Nothing (Common.decodeMaybe Common.asInteger) ps
  tou <- Common.defaultedField "toughness" Nothing (Common.decodeMaybe Common.asInteger) ps
  loy <- Common.defaultedField "loyalty" Nothing (Common.decodeMaybe (Codec.decode Loyalty.codec)) ps
  def_ <- Common.defaultedField "defense" Nothing (Common.decodeMaybe (Codec.decode Defense.codec)) ps
  cda <- Common.defaultedField "characteristicPT" Nothing (Common.decodeMaybe (Codec.decode Quantity.pairCodec)) ps
  cts <- Common.field "cardTypes" ps >>= Common.decodeSet (Codec.decode CardType.codec)
  subs <- Common.defaultedField "subtypes" Set.empty (Common.decodeSet (Codec.decode Subtype.codec)) ps
  acts <- Common.defaultedField "activatedAbilities" [] (Common.decodeList (ActivatedAbility.fromJson Card.fromJson)) ps
  reps <- Common.defaultedField "replacementEffects" [] (Common.decodeList ReplacementEffect.fromJson) ps
  trigs <- Common.defaultedField "triggeredAbilities" [] (Common.decodeList (TriggeredAbility.fromJson Card.fromJson)) ps
  pure
    PC.MkProjectedCharacteristics
      { PC.names = nms,
        PC.supertypes = sups,
        PC.keywords = kws,
        PC.colors = cols,
        PC.manaValue = mv,
        PC.power = pow,
        PC.toughness = tou,
        PC.loyalty = loy,
        PC.defense = def_,
        PC.characteristicPT = cda,
        PC.cardTypes = cts,
        PC.subtypes = subs,
        PC.activatedAbilities = acts,
        PC.replacementEffects = reps,
        PC.triggeredAbilities = trigs
      }
