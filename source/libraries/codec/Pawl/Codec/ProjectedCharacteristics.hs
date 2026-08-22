{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ProjectedCharacteristics where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CardType as CardType
import qualified Pawl.Codec.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Codec.CharacteristicPT as CharacteristicPT
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.PrintedReplacement as PrintedReplacement
import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.Codec.Supertype as Supertype
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ProjectedCharacteristics as PC

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec PC.ProjectedCharacteristics
codec = Fields.object $ do
  names <- Fields.required "names" (Common.set CardName.codec) PC.names
  supertypes <- Fields.defaulted "supertypes" Set.empty (Common.set Supertype.codec) PC.supertypes
  keywords <- Fields.defaulted "keywords" Map.empty (Common.multiset Keyword.codec) PC.keywords
  colors <- Fields.defaulted "colors" Set.empty (Common.set Color.codec) PC.colors
  manaValue <- Fields.defaulted "manaValue" Nothing (Common.maybe Common.integer) PC.manaValue
  power <- Fields.defaulted "power" Nothing (Common.maybe Common.integer) PC.power
  toughness <- Fields.defaulted "toughness" Nothing (Common.maybe Common.integer) PC.toughness
  loyalty <- Fields.defaulted "loyalty" Nothing (Common.maybe Loyalty.codec) PC.loyalty
  defense <- Fields.defaulted "defense" Nothing (Common.maybe Defense.codec) PC.defense
  characteristicPT <- Fields.defaulted "characteristicPT" Nothing (Common.maybe CharacteristicPT.codec) PC.characteristicPT
  cardTypes <- Fields.required "cardTypes" (Common.set CardType.codec) PC.cardTypes
  subtypes <- Fields.defaulted "subtypes" Set.empty (Common.set Subtype.codec) PC.subtypes
  activatedAbilities <- Fields.defaulted "activatedAbilities" [] (Common.list (ActivatedAbility.codec Card.codec)) PC.activatedAbilities
  replacementEffects <- Fields.defaulted "replacementEffects" [] (Common.list (PrintedReplacement.codec (Effect.codec Card.codec))) PC.replacementEffects
  triggeredAbilities <- Fields.defaulted "triggeredAbilities" [] (Common.list (TriggeredAbility.codec Card.codec)) PC.triggeredAbilities
  enchant <- Fields.defaulted "enchant" [] (Common.list TargetSlot.codec) PC.enchant
  subtypeWordChanges <- Fields.defaulted "subtypeWordChanges" [] (Common.list ChangeSubtypeWord.codec) PC.subtypeWordChanges
  pure
    PC.MkProjectedCharacteristics
      { PC.names = names,
        PC.supertypes = supertypes,
        PC.keywords = keywords,
        PC.colors = colors,
        PC.manaValue = manaValue,
        PC.power = power,
        PC.toughness = toughness,
        PC.loyalty = loyalty,
        PC.defense = defense,
        PC.characteristicPT = characteristicPT,
        PC.cardTypes = cardTypes,
        PC.subtypes = subtypes,
        PC.activatedAbilities = activatedAbilities,
        PC.replacementEffects = replacementEffects,
        PC.triggeredAbilities = triggeredAbilities,
        PC.enchant = enchant,
        PC.subtypeWordChanges = subtypeWordChanges
      }
