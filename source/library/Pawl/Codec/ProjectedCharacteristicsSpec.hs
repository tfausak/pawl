module Pawl.Codec.ProjectedCharacteristicsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.FaceSpec as FaceSpec
import qualified Pawl.Codec.ProjectedCharacteristics as PC
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | `name` and `cardTypes` are the only required keys; every other field is
-- omitted when it is at its default. One populated fixture, with every
-- collection given at least one element and 'loyalty'/'characteristicPT' left
-- at Nothing, exercises the whole shape at once including those omissions;
-- `minimalCharacteristics` below is its counterpart with everything but the two
-- required keys defaulted.
--
-- No registry here: like Pawl.Codec.CardSpec, Pawl.Codec sits before
-- Pawl.Registry and does not reach a real snapshot. Round trips over an
-- engine-built snapshot stay in Pawl.CodecIntegrationSpec.

-- | A synthetic snapshot, not any real card's projection: its supertype and
-- subtype are chosen to exercise every collection field.
testCharacteristics :: PC.ProjectedCharacteristics
testCharacteristics =
  PC.MkProjectedCharacteristics
    { PC.names = Set.singleton . CardName.MkCardName $ Text.pack "Test Creature",
      PC.supertypes = Set.singleton Supertype.Legendary,
      PC.keywords = Map.singleton Keyword.Flying 1,
      PC.colors = Set.singleton Color.Blue,
      PC.manaValue = Just 3,
      PC.power = Just 1,
      PC.toughness = Just 2,
      PC.loyalty = Nothing,
      PC.defense = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.singleton CardType.Creature,
      PC.subtypes = Set.singleton Subtype.Human,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = [FaceSpec.minimalTriggeredAbility],
      PC.subtypeWordChanges = [ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Spirit Subtype.Elf]
    }

testCharacteristicsJson :: String
testCharacteristicsJson =
  "{\"names\":[\"Test Creature\"],\"supertypes\":[{\"type\":\"Legendary\"}],\"keywords\":[{\"type\":\"Flying\"}],"
    <> "\"colors\":[{\"type\":\"Blue\"}],\"manaValue\":3,\"power\":1,\"toughness\":2,"
    <> "\"cardTypes\":[{\"type\":\"Creature\"}],\"subtypes\":[{\"type\":\"Human\"}],"
    <> "\"triggeredAbilities\":[{\"condition\":{\"type\":\"SelfEnters\"},"
    <> "\"modal\":{\"modes\":[{}]}}],"
    <> "\"subtypeWordChanges\":[{\"from\":{\"type\":\"Spirit\"},\"to\":{\"type\":\"Elf\"}}]}"

-- | Every field but the two required ones at its default.
minimalCharacteristics :: PC.ProjectedCharacteristics
minimalCharacteristics =
  PC.MkProjectedCharacteristics
    { PC.names = Set.singleton (CardName.MkCardName (Text.pack "Mountain")),
      PC.supertypes = Set.empty,
      PC.keywords = Map.empty,
      PC.colors = Set.empty,
      PC.manaValue = Nothing,
      PC.power = Nothing,
      PC.toughness = Nothing,
      PC.loyalty = Nothing,
      PC.defense = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.singleton CardType.Land,
      PC.subtypes = Set.empty,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = [],
      PC.subtypeWordChanges = []
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ProjectedCharacteristics" $ do
  Spec.it s "MkProjectedCharacteristics, every collection populated, loyalty and characteristicPT omitted at Nothing" $
    Common.assertCodec s PC.codec testCharacteristics testCharacteristicsJson
  Spec.it s "an all-default value omits every optional key" $
    Common.assertCodec
      s
      PC.codec
      minimalCharacteristics
      " {\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]} "
