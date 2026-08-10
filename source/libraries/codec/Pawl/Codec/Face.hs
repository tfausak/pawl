-- | @Face@ is the entry point to the rest of @Pawl.Codec@: the transitive
-- closure of @Face@'s fields is one module per type, each exposing free
-- @toJson@\/@fromJson@ functions rather than a type class, and this module is
-- where they are instantiated at @Face@.
--
-- Parametric in the card codec the way 'Pawl.Types.Face.Face' is parametric in
-- @card@: @Pawl.Codec.Card@ passes its own pair in, which is where the knot is
-- tied.
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core.
module Pawl.Codec.Face where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Codec.Power as Power
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Codec.SpecialAction as SpecialAction
import qualified Pawl.Codec.StaticAbility as StaticAbility
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Codec.Toughness as Toughness
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Codec.TypeLine as TypeLine
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face

-- `Eq card` because six of the fields below carry card-shaped payloads and
-- 'Common.optionalPair' omits a key by comparing its value to the default.
toJson :: (Eq card) => (card -> Value.Value) -> Face.Face card -> Value.Value
toJson encodeCard f =
  Common.object . concat $
    [ Common.requiredPair "name" CardName.toJson (Face.name f),
      Common.requiredPair "typeLine" TypeLine.toJson (Face.typeLine f),
      Common.optionalPair "manaCost" Nothing (Common.encodeMaybe ManaCost.toJson) (Face.manaCost f),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Power.toJson) (Face.power f),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Toughness.toJson) (Face.toughness f),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe Loyalty.toJson) (Face.loyalty f),
      Common.optionalPair "defense" Nothing (Common.encodeMaybe Defense.toJson) (Face.defense f),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe Quantity.toJson) (Face.characteristicPT f),
      Common.optionalPair "enchant" [] (Common.encodeList TargetSpec.toJson) (Face.enchant f),
      Common.optionalPair "keywords" Set.empty (Common.encodeSet Keyword.toJson) (Face.keywords f),
      Common.optionalPair "colorIndicator" Set.empty (Common.encodeSet Color.toJson) (Face.colorIndicator f),
      Common.optionalPair "spell" Face.defaultSpell (Modal.toJson encodeCard) (Face.spell f),
      Common.optionalPair "staticAbilities" [] (Common.encodeList StaticAbility.toJson) (Face.staticAbilities f),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson encodeCard)) (Face.activatedAbilities f),
      Common.optionalPair "replacementEffects" [] (Common.encodeList ReplacementEffect.toJson) (Face.replacementEffects f),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson encodeCard)) (Face.triggeredAbilities f),
      Common.optionalPair "delayedAbilities" Map.empty (TriggeredAbility.toJsonDelayed encodeCard) (Face.delayedAbilities f),
      Common.optionalPair "castingPermissions" [] (Common.encodeList CastingPermission.toJson) (Face.castingPermissions f),
      Common.optionalPair "castingRestrictions" [] (Common.encodeList CastingRestriction.toJson) (Face.castingRestrictions f),
      Common.optionalPair "playerAbilities" [] (Common.encodeList PlayerStaticAbility.toJson) (Face.playerAbilities f),
      Common.optionalPair "blockRequirements" [] (Common.encodeList BlockRequirement.toJson) (Face.blockRequirements f),
      Common.optionalPair "blockPermissions" [] (Common.encodeList BlockPermission.toJson) (Face.blockPermissions f),
      Common.optionalPair "attackRequirements" [] (Common.encodeList AttackRequirement.toJson) (Face.attackRequirements f),
      Common.optionalPair "combatRestrictions" [] (Common.encodeList CombatRestriction.toJson) (Face.combatRestrictions f),
      Common.optionalPair "sacrificeRestrictions" [] (Common.encodeList SacrificeRestriction.toJson) (Face.sacrificeRestrictions f),
      Common.optionalPair "attackCosts" [] (Common.encodeList AttackCost.toJson) (Face.attackCosts f),
      Common.optionalPair "additionalCosts" [] (Common.encodeList (CostComponent.toJson Keyword.toJson)) (Face.additionalCosts f),
      Common.optionalPair "alternativeCosts" [] (Common.encodeList (Cost.toJson Keyword.toJson)) (Face.alternativeCosts f),
      -- CR 103.5b / CR 103.6: an array of ACTIONS, each an array of effects, so a
      -- face granting two of them is writable (Pawl.Types.Face).
      Common.optionalPair "mulliganActions" [] (Common.encodeList (Common.encodeList (Effect.toJson encodeCard))) (Face.mulliganActions f),
      Common.optionalPair "openingHandActions" [] (Common.encodeList (Common.encodeList (Effect.toJson encodeCard))) (Face.openingHandActions f),
      -- CR 116.2: the special actions this face grants (Pawl.Types.Face).
      Common.optionalPair "specialActions" [] (Common.encodeList SpecialAction.toJson) (Face.specialActions f),
      -- CR 113.6g: Counterable is the absence of a card stating it can't be
      -- countered.
      Common.optionalPair "counterability" Counterability.Counterable Counterability.toJson (Face.counterability f)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Face.Face card)
fromJson decodeCard value = do
  ps <- Common.asObject value
  name <- Common.field "name" ps >>= CardName.fromJson
  typeLine <- Common.field "typeLine" ps >>= TypeLine.fromJson
  manaCost <- Common.defaultedField "manaCost" Nothing (Common.decodeMaybe ManaCost.fromJson) ps
  power <- Common.defaultedField "power" Nothing (Common.decodeMaybe Power.fromJson) ps
  toughness <- Common.defaultedField "toughness" Nothing (Common.decodeMaybe Toughness.fromJson) ps
  loyalty <- Common.defaultedField "loyalty" Nothing (Common.decodeMaybe Loyalty.fromJson) ps
  defense <- Common.defaultedField "defense" Nothing (Common.decodeMaybe Defense.fromJson) ps
  keywords <- Common.defaultedField "keywords" Set.empty (Common.decodeSet Keyword.fromJson) ps
  statics <- Common.defaultedField "staticAbilities" [] (Common.decodeList StaticAbility.fromJson) ps
  spell <- Common.defaultedField "spell" Face.defaultSpell (Modal.fromJson decodeCard) ps
  activated <- Common.defaultedField "activatedAbilities" [] (Common.decodeList (ActivatedAbility.fromJson decodeCard)) ps
  replacements <- Common.defaultedField "replacementEffects" [] (Common.decodeList ReplacementEffect.fromJson) ps
  triggered <- Common.defaultedField "triggeredAbilities" [] (Common.decodeList (TriggeredAbility.fromJson decodeCard)) ps
  permissions <- Common.defaultedField "castingPermissions" [] (Common.decodeList CastingPermission.fromJson) ps
  restrictions <- Common.defaultedField "castingRestrictions" [] (Common.decodeList CastingRestriction.fromJson) ps
  colorIndicator <- Common.defaultedField "colorIndicator" Set.empty (Common.decodeSet Color.fromJson) ps
  characteristicPT <- Common.defaultedField "characteristicPT" Nothing (Common.decodeMaybe Quantity.fromJson) ps
  delayed <- Common.defaultedField "delayedAbilities" Map.empty (TriggeredAbility.fromJsonDelayed decodeCard) ps
  playerAbilities <- Common.defaultedField "playerAbilities" [] (Common.decodeList PlayerStaticAbility.fromJson) ps
  blockRequirements <- Common.defaultedField "blockRequirements" [] (Common.decodeList BlockRequirement.fromJson) ps
  blockPermissions <- Common.defaultedField "blockPermissions" [] (Common.decodeList BlockPermission.fromJson) ps
  attackRequirements <- Common.defaultedField "attackRequirements" [] (Common.decodeList AttackRequirement.fromJson) ps
  combatRestrictions <- Common.defaultedField "combatRestrictions" [] (Common.decodeList CombatRestriction.fromJson) ps
  sacrificeRestrictions <- Common.defaultedField "sacrificeRestrictions" [] (Common.decodeList SacrificeRestriction.fromJson) ps
  attackCosts <- Common.defaultedField "attackCosts" [] (Common.decodeList AttackCost.fromJson) ps
  additionalCosts <- Common.defaultedField "additionalCosts" [] (Common.decodeList (CostComponent.fromJson Keyword.fromJson)) ps
  alternativeCosts <- Common.defaultedField "alternativeCosts" [] (Common.decodeList (Cost.fromJson Keyword.fromJson)) ps
  mulliganActions <- Common.defaultedField "mulliganActions" [] (Common.decodeList (Common.decodeList (Effect.fromJson decodeCard))) ps
  openingHandActions <- Common.defaultedField "openingHandActions" [] (Common.decodeList (Common.decodeList (Effect.fromJson decodeCard))) ps
  specialActions <- Common.defaultedField "specialActions" [] (Common.decodeList SpecialAction.fromJson) ps
  enchant <- Common.defaultedField "enchant" [] (Common.decodeList TargetSpec.fromJson) ps
  counterability <- Common.defaultedField "counterability" Counterability.Counterable Counterability.fromJson ps
  pure
    Face.MkFace
      { Face.name = name,
        Face.manaCost = manaCost,
        Face.typeLine = typeLine,
        Face.power = power,
        Face.toughness = toughness,
        Face.loyalty = loyalty,
        Face.defense = defense,
        Face.keywords = keywords,
        Face.staticAbilities = statics,
        Face.spell = spell,
        Face.activatedAbilities = activated,
        Face.replacementEffects = replacements,
        Face.triggeredAbilities = triggered,
        Face.castingPermissions = permissions,
        Face.castingRestrictions = restrictions,
        Face.colorIndicator = colorIndicator,
        Face.characteristicPT = characteristicPT,
        Face.delayedAbilities = delayed,
        Face.playerAbilities = playerAbilities,
        Face.blockRequirements = blockRequirements,
        Face.blockPermissions = blockPermissions,
        Face.attackRequirements = attackRequirements,
        Face.combatRestrictions = combatRestrictions,
        Face.sacrificeRestrictions = sacrificeRestrictions,
        Face.attackCosts = attackCosts,
        Face.additionalCosts = additionalCosts,
        Face.alternativeCosts = alternativeCosts,
        Face.mulliganActions = mulliganActions,
        Face.openingHandActions = openingHandActions,
        Face.specialActions = specialActions,
        Face.enchant = enchant,
        Face.counterability = counterability
      }
