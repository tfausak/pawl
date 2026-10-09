module Pawl.Codec.ProjectedCharacteristicsSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.CardSpec as CardSpec
import qualified Pawl.Codec.FaceSpec as FaceSpec
import qualified Pawl.Codec.ProjectedCharacteristics as PC
import qualified Pawl.Codec.RuleAbilitiesSpec as RuleAbilitiesSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AlternativeCost as AlternativeCost
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostChoice as CostChoice
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CostDirection as CostDirection
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Face as Face.Type
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SpecialAction as SpecialAction
import qualified Pawl.Types.SpendManaAsThough as SpendManaAsThough
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSlot as TargetSlot
import qualified Pawl.Types.Timestamp as Timestamp

-- | `name` and `cardTypes` are the only required keys; every other field is
-- omitted when it is at its default. One populated fixture, with every
-- collection given at least one element and 'loyalty'/'characteristicPT' left
-- at Nothing, exercises the whole shape at once including those omissions;
-- `minimalCharacteristics` below is its counterpart with everything but the two
-- required keys defaulted.
--
-- No registry here: like Pawl.Codec.CardSpec, this sublibrary sits above
-- Pawl.Registry and cannot reach a real snapshot. Round trips over an
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
      PC.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2, ManaSymbol.OfType (ManaType.Colored Color.Blue)]),
      PC.manaValue = Just 3,
      PC.power = Just 1,
      PC.toughness = Just 2,
      PC.loyalty = Nothing,
      PC.defense = Nothing,
      PC.intensity = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.singleton CardType.Creature,
      PC.subtypes = Set.singleton Subtype.Human,
      PC.staticAbilities = [StaticAbility.MkStaticAbility Affected.Attached Nothing Set.empty Nothing Nothing (NonEmpty.singleton (Modification.GainKeyword Keyword.Flying))],
      PC.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility {PlayerStaticAbility.scope = PlayerScope.EachPlayer, PlayerStaticAbility.condition = Nothing, PlayerStaticAbility.name = Nothing, PlayerStaticAbility.effect = PlayerEffect.CantCastMoreThan 1}],
      PC.grantedPlayerAbilities = [(Timestamp.MkTimestamp 3, PlayerStaticAbility.MkPlayerStaticAbility {PlayerStaticAbility.scope = PlayerScope.You, PlayerStaticAbility.condition = Nothing, PlayerStaticAbility.name = Nothing, PlayerStaticAbility.effect = PlayerEffect.NoMaximumHandSize})],
      PC.grantedStaticAbilities = [(Timestamp.MkTimestamp 4, StaticAbility.MkStaticAbility Affected.Attached Nothing Set.empty Nothing Nothing (NonEmpty.singleton (Modification.GainKeyword Keyword.Trample)))],
      PC.grantedRuleAbilities = RuleAbilitiesSpec.testRuleAbilities,
      PC.specialActions = [SpecialAction.DiscardThisAnyTime],
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = [FaceSpec.minimalTriggeredAbility],
      PC.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "later")) FaceSpec.minimalTriggeredAbility,
      PC.enchant = [TargetSlot.required Pool.Creatures Nothing],
      PC.castingPermissions = [CastingPermission.CastFromLibraryWhileSearching],
      -- Pawl.Codec.RuleAbilitiesSpec's own fixture, which is where the twelve
      -- keys inside it are checked; here the case is only that the bundle rides
      -- one key of this record's own object.
      PC.ruleAbilities = RuleAbilitiesSpec.testRuleAbilities,
      -- True rather than the default, so an arm that dropped the field would not
      -- round trip to the same JSON.
      PC.lostAllAbilities = True,
      PC.hasFullText = True,
      PC.subtypeWordChanges = [ChangeSubtypeWord.MkChangeSubtypeWord Subtype.Spirit Subtype.Elf],
      -- A different keyword from `keywords` above, so a codec arm reading the
      -- wrong field would not round trip to the same JSON.
      PC.textChangedKeywords = Map.singleton Keyword.Trample 1,
      PC.assignsCombatDamageWithToughness = True,
      PC.grantsStationToughness = True,
      -- Pawl.Codec.FaceSpec's own values for the four cost fields.
      PC.additionalCosts = [CostComponent.TapThis],
      PC.additionalCostChoices = [CostChoice.MkCostChoice (Cost.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.TapThis] NonEmpty.:| [Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1])) []])],
      PC.alternativeCosts = [AlternativeCost.MkAlternativeCost Nothing (Cost.MkCost (Just (ManaCost.MkManaCost [])) [])],
      PC.costReductions = [CostReduction.MkCostReduction (ManaCost.MkManaCost [ManaSymbol.Generic 3]) (Quantity.Literal 1) Nothing Nothing CostDirection.Less],
      PC.grantedCostReductions = [CostReduction.MkCostReduction (ManaCost.MkManaCost [ManaSymbol.Generic 1]) (Quantity.Literal 1) Nothing Nothing CostDirection.Less],
      PC.grantedAlternativeCosts = [AlternativeCost.MkAlternativeCost Nothing (Cost.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 2])) [])],
      PC.grantedSpendManaAsThough = [SpendManaAsThough.MkSpendManaAsThough ManaFilter.Any (Set.singleton ManaType.Colorless) True],
      -- Synthetic like the rest of this value: a Mountain has no halves, and
      -- what the case is about is that the field carries a whole card through
      -- the wire (CR 709.5).
      PC.halves = Just CardSpec.mountainCard,
      -- CR 707.9b, synthetic for halves' reason.
      PC.exceptions = [CopyException.NoManaCost],
      -- CR 702.140e, synthetic for halves' reason.
      PC.mergedDonors = [minimalCharacteristics],
      -- Synthetic for halves' reason: a Mountain has no inset frame, and what
      -- the case is about is that the field carries a whole FACE through the
      -- wire (CR 722.2b).
      PC.prepare = Just (NonEmpty.head (Card.Type.faces CardSpec.mountainCard)),
      -- Synthetic for prepare's reason (CR 715.2b).
      PC.alternativeSpell = Just (NonEmpty.head (Card.Type.faces CardSpec.mountainCard)),
      -- Two empty modes, off the default of one (CR 707.2).
      PC.spell = Face.Type.defaultSpell {Modal.modes = Modal.modes (Face.Type.defaultSpell :: Modal.Modal Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card)) <> Modal.modes Face.Type.defaultSpell},
      -- The recursive field, carrying the all-default record so the nested
      -- object's own defaults are exercised too (CR 707.3).
      PC.flipped = Just minimalCharacteristics
    }

testCharacteristicsJson :: String
testCharacteristicsJson =
  "{\"names\":[\"Test Creature\"],\"supertypes\":[{\"type\":\"Legendary\"}],\"keywords\":[{\"key\":{\"type\":\"Flying\"},\"value\":1}],"
    <> "\"colors\":[{\"type\":\"Blue\"}],"
    <> "\"manaCost\":[{\"type\":\"Generic\",\"value\":2},{\"type\":\"OfType\",\"value\":{\"type\":\"Colored\",\"value\":{\"type\":\"Blue\"}}}],"
    <> "\"manaValue\":3,\"power\":1,\"toughness\":2,"
    <> "\"cardTypes\":[{\"type\":\"Creature\"}],\"subtypes\":[{\"type\":\"Human\"}],"
    <> "\"staticAbilities\":[{\"affected\":{\"type\":\"Attached\"},\"modifications\":[{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Flying\"}}]}],"
    <> "\"playerAbilities\":[{\"scope\":{\"type\":\"EachPlayer\"},\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":1}}],"
    <> "\"grantedPlayerAbilities\":[{\"key\":3,\"value\":{\"scope\":{\"type\":\"You\"},\"effect\":{\"type\":\"NoMaximumHandSize\"}}}],"
    <> "\"grantedStaticAbilities\":[{\"key\":4,\"value\":{\"affected\":{\"type\":\"Attached\"},\"modifications\":[{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Trample\"}}]}}],"
    <> "\"grantedRuleAbilities\":"
    <> RuleAbilitiesSpec.testRuleAbilitiesJson
    <> ","
    <> "\"specialActions\":[{\"type\":\"DiscardThisAnyTime\"}],"
    <> "\"triggeredAbilities\":[{\"condition\":{\"type\":\"SelfEnters\"},"
    <> "\"modal\":{\"modes\":[{}]}}],"
    <> "\"delayedAbilities\":{\"later\":{\"condition\":{\"type\":\"SelfEnters\"},\"modal\":{\"modes\":[{}]}}},"
    <> "\"enchant\":[{\"pool\":{\"type\":\"Creatures\"}}],"
    <> "\"castingPermissions\":[{\"type\":\"CastFromLibraryWhileSearching\"}],"
    <> "\"ruleAbilities\":"
    <> RuleAbilitiesSpec.testRuleAbilitiesJson
    <> ","
    <> "\"lostAllAbilities\":true,"
    <> "\"hasFullText\":true,"
    <> "\"subtypeWordChanges\":[{\"from\":{\"type\":\"Spirit\"},\"to\":{\"type\":\"Elf\"}}],"
    <> "\"textChangedKeywords\":[{\"key\":{\"type\":\"Trample\"},\"value\":1}],"
    <> "\"assignsCombatDamageWithToughness\":true,"
    <> "\"grantsStationToughness\":true,"
    <> "\"additionalCosts\":[{\"type\":\"TapThis\"}],"
    <> "\"additionalCostChoices\":[[{\"components\":[{\"type\":\"TapThis\"}],\"mana\":[]},{\"mana\":[{\"type\":\"Generic\",\"value\":1}]}]],"
    <> "\"alternativeCosts\":[{\"cost\":{\"mana\":[]}}],"
    <> "\"costReductions\":[{\"amount\":[{\"type\":\"Generic\",\"value\":3}],\"perEach\":{\"type\":\"Literal\",\"value\":1}}],"
    <> "\"grantedCostReductions\":[{\"amount\":[{\"type\":\"Generic\",\"value\":1}],\"perEach\":{\"type\":\"Literal\",\"value\":1}}],"
    <> "\"grantedAlternativeCosts\":[{\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":2}]}}],"
    <> "\"grantedSpendManaAsThough\":[{\"which\":{\"type\":\"Any\"},\"asThough\":[{\"type\":\"Colorless\"}],\"only\":true}],"
    <> "\"halves\":{\"faces\":[{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}}]},"
    <> "\"exceptions\":[{\"type\":\"NoManaCost\"}],"
    <> "\"mergedDonors\":[{\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]}],"
    <> "\"prepare\":{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}},"
    <> "\"alternativeSpell\":{\"name\":\"Mountain\",\"typeLine\":{\"supertypes\":[{\"type\":\"Basic\"}],\"types\":[{\"type\":\"Land\"}],\"subtypes\":[{\"type\":\"Mountain\"}]}},"
    <> "\"spell\":{\"modes\":[{},{}]},"
    <> "\"flipped\":{\"names\":[\"Mountain\"],\"cardTypes\":[{\"type\":\"Land\"}]}}"

-- | Every field but the two required ones at its default.
minimalCharacteristics :: PC.ProjectedCharacteristics
minimalCharacteristics =
  PC.MkProjectedCharacteristics
    { PC.names = Set.singleton (CardName.MkCardName (Text.pack "Mountain")),
      PC.supertypes = Set.empty,
      PC.keywords = Map.empty,
      PC.colors = Set.empty,
      PC.manaCost = Nothing,
      PC.manaValue = Nothing,
      PC.power = Nothing,
      PC.toughness = Nothing,
      PC.loyalty = Nothing,
      PC.defense = Nothing,
      PC.intensity = Nothing,
      PC.characteristicPT = Nothing,
      PC.cardTypes = Set.singleton CardType.Land,
      PC.subtypes = Set.empty,
      PC.staticAbilities = [],
      PC.playerAbilities = [],
      PC.grantedPlayerAbilities = [],
      PC.grantedStaticAbilities = [],
      PC.grantedRuleAbilities = mempty,
      PC.specialActions = [],
      PC.activatedAbilities = [],
      PC.replacementEffects = [],
      PC.triggeredAbilities = [],
      PC.delayedAbilities = Map.empty,
      PC.enchant = [],
      PC.castingPermissions = [],
      PC.ruleAbilities = mempty,
      PC.lostAllAbilities = False,
      PC.hasFullText = False,
      PC.subtypeWordChanges = [],
      PC.textChangedKeywords = Map.empty,
      PC.assignsCombatDamageWithToughness = False,
      PC.grantsStationToughness = False,
      PC.additionalCosts = [],
      PC.additionalCostChoices = [],
      PC.alternativeCosts = [],
      PC.costReductions = [],
      PC.grantedCostReductions = [],
      PC.grantedAlternativeCosts = [],
      PC.grantedSpendManaAsThough = [],
      PC.halves = Nothing,
      PC.exceptions = [],
      PC.mergedDonors = [],
      PC.prepare = Nothing,
      PC.alternativeSpell = Nothing,
      PC.spell = Face.Type.defaultSpell,
      PC.flipped = Nothing
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
