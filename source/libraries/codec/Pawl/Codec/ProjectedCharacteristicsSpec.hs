module Pawl.Codec.ProjectedCharacteristicsSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ProjectedCharacteristics as PC
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- | R7's one case for MkProjectedCharacteristics's single constructor. Every
-- field is a REQUIRED key here (unlike Card's defaulted/elided ones) -- the
-- encoder writes JSON null rather than omitting the key for an absent Maybe --
-- so one populated fixture, with every collection given at least one element
-- and 'power'/'toughness' set but 'loyalty'/'characteristicPT' left at their
-- Nothing (a creature has no loyalty and no printed star), exercises the whole
-- shape at once. `triggeredAbilities` reuses 'CardSpec.minimalTriggeredAbility'
-- rather than building a second one by hand; `activatedAbilities` and
-- `replacementEffects` stay empty because ActivatedAbility's and
-- ReplacementEffect's own per-constructor coverage lives in their own XSpecs.
--
-- No registry here: like Pawl.Codec.CardSpec, this sublibrary sits above
-- Pawl.Registry and cannot reach a real snapshot. The GameEvent.Moved/Revealed
-- round trips over a REAL Typhoid Rats snapshot (multiple keywords, colors,
-- power, toughness, cardTypes and subtypes all populated by the engine) stay in
-- Pawl.CodecIntegrationSpec.

-- | A synthetic snapshot, not any real card's projection (its Legendary
-- supertype and Human subtype are an arbitrary combination chosen to exercise
-- every collection field, not a claim about a printed creature).
testCharacteristics :: PC.ProjectedCharacteristics
testCharacteristics =
  PC.MkProjectedCharacteristics
    { PC.name = CardName.MkCardName $ Text.pack "Test Creature",
      PC.supertypes = Set.singleton Supertype.Legendary,
      PC.keywords = Map.singleton Keyword.Flying 1,
      PC.colors = Set.singleton Color.Blue,
      PC.power = Just 1,
      PC.toughness = Just 2,
      PC.loyalty = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.singleton CardType.Creature,
      PC.subtypes = Set.singleton Subtype.Human,
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = [CardSpec.minimalTriggeredAbility]
    }

testCharacteristicsJson :: String
testCharacteristicsJson =
  "{\"name\":\"Test Creature\",\"supertypes\":[{\"type\":\"Legendary\"}],\"keywords\":[{\"type\":\"Flying\"}],"
    <> "\"colors\":[{\"type\":\"Blue\"}],\"power\":1,\"toughness\":2,\"loyalty\":null,\"characteristicPT\":null,"
    <> "\"cardTypes\":[{\"type\":\"Creature\"}],\"subtypes\":[{\"type\":\"Human\"}],\"activatedAbilities\":[],"
    <> "\"replacementEffects\":[],\"triggeredAbilities\":[{\"condition\":{\"type\":\"SelfEnters\"},"
    <> "\"modal\":{\"modes\":[{}]}}]}"

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ProjectedCharacteristics" . Spec.it s "MkProjectedCharacteristics, every field populated or explicitly Nothing" $
    Common.assertJsonCodec s PC.toJson PC.fromJson testCharacteristics testCharacteristicsJson
