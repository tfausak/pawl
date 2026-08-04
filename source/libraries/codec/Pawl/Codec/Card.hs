-- | @Card@ is the entry point to the rest of @Pawl.Codec@: the transitive
-- closure of @Card@'s fields is one module per type, each exposing free
-- @toJson@\/@fromJson@ functions rather than a type class, and this module is
-- where they are instantiated at @Card@.
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core.
module Pawl.Codec.Card where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
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
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Codec.Power as Power
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.StaticAbility as StaticAbility
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Codec.Toughness as Toughness
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Codec.TypeLine as TypeLine
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality

-- | What a card that says nothing about its spell means: one mode with no
-- effects and no targets, chosen. Every land and vanilla creature has exactly
-- this, which is why it is the default rather than a required key.
defaultSpell :: Modal.Modal Card.Card
defaultSpell =
  Modal.MkModal
    (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory))
    Modal.defaultSelection

toJson :: Card.Card -> Value.Value
toJson c =
  Common.object . concat $
    [ Common.requiredPair "name" CardName.toJson (Card.name c),
      Common.requiredPair "typeLine" TypeLine.toJson (Card.typeLine c),
      Common.optionalPair "manaCost" Nothing (Common.encodeMaybe ManaCost.toJson) (Card.manaCost c),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Power.toJson) (Card.power c),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Toughness.toJson) (Card.toughness c),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe Loyalty.toJson) (Card.loyalty c),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe Quantity.toJson) (Card.characteristicPT c),
      Common.optionalPair "enchant" Nothing (Common.encodeMaybe TargetSpec.toJson) (Card.enchant c),
      Common.optionalPair "keywords" Set.empty (Common.encodeSet Keyword.toJson) (Card.keywords c),
      Common.optionalPair "colorIndicator" Set.empty (Common.encodeSet Color.toJson) (Card.colorIndicator c),
      Common.optionalPair "spell" defaultSpell (Modal.toJson toJson) (Card.spell c),
      Common.optionalPair "staticAbilities" [] (Common.encodeList StaticAbility.toJson) (Card.staticAbilities c),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson toJson)) (Card.activatedAbilities c),
      Common.optionalPair "replacementEffects" [] (Common.encodeList ReplacementEffect.toJson) (Card.replacementEffects c),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson toJson)) (Card.triggeredAbilities c),
      Common.optionalPair "delayedAbilities" Map.empty (TriggeredAbility.toJsonDelayed toJson) (Card.delayedAbilities c),
      Common.optionalPair "castingPermissions" [] (Common.encodeList CastingPermission.toJson) (Card.castingPermissions c),
      Common.optionalPair "castingRestrictions" [] (Common.encodeList CastingRestriction.toJson) (Card.castingRestrictions c),
      Common.optionalPair "playerAbilities" [] (Common.encodeList PlayerStaticAbility.toJson) (Card.playerAbilities c),
      Common.optionalPair "blockRequirements" [] (Common.encodeList BlockRequirement.toJson) (Card.blockRequirements c),
      Common.optionalPair "attackRequirements" [] (Common.encodeList AttackRequirement.toJson) (Card.attackRequirements c),
      Common.optionalPair "combatRestrictions" [] (Common.encodeList CombatRestriction.toJson) (Card.combatRestrictions c),
      Common.optionalPair "attackCosts" [] (Common.encodeList AttackCost.toJson) (Card.attackCosts c),
      Common.optionalPair "additionalCosts" [] (Common.encodeList (CostComponent.toJson Keyword.toJson)) (Card.additionalCosts c),
      Common.optionalPair "alternativeCosts" [] (Common.encodeList (Cost.toJson Keyword.toJson)) (Card.alternativeCosts c),
      Common.optionalPair "mulliganAction" [] (Common.encodeList (Effect.toJson toJson)) (Card.mulliganAction c),
      Common.optionalPair "openingHandAction" [] (Common.encodeList (Effect.toJson toJson)) (Card.openingHandAction c),
      -- CR 113.6g: Counterable is the absence of a card stating it can't be
      -- countered.
      Common.optionalPair "counterability" Counterability.Counterable Counterability.toJson (Card.counterability c)
    ]

fromJson :: Value.Value -> Either Text.Text Card.Card
fromJson value = do
  ps <- Common.asObject value
  name <- Common.field "name" ps >>= CardName.fromJson
  typeLine <- Common.field "typeLine" ps >>= TypeLine.fromJson
  manaCost <- Common.defaultedField "manaCost" Nothing (Common.decodeMaybe ManaCost.fromJson) ps
  power <- Common.defaultedField "power" Nothing (Common.decodeMaybe Power.fromJson) ps
  toughness <- Common.defaultedField "toughness" Nothing (Common.decodeMaybe Toughness.fromJson) ps
  loyalty <- Common.defaultedField "loyalty" Nothing (Common.decodeMaybe Loyalty.fromJson) ps
  keywords <- Common.defaultedField "keywords" Set.empty (Common.decodeSet Keyword.fromJson) ps
  statics <- Common.defaultedField "staticAbilities" [] (Common.decodeList StaticAbility.fromJson) ps
  spell <- Common.defaultedField "spell" defaultSpell (Modal.fromJson fromJson) ps
  activated <- Common.defaultedField "activatedAbilities" [] (Common.decodeList (ActivatedAbility.fromJson fromJson)) ps
  replacements <- Common.defaultedField "replacementEffects" [] (Common.decodeList ReplacementEffect.fromJson) ps
  triggered <- Common.defaultedField "triggeredAbilities" [] (Common.decodeList (TriggeredAbility.fromJson fromJson)) ps
  permissions <- Common.defaultedField "castingPermissions" [] (Common.decodeList CastingPermission.fromJson) ps
  restrictions <- Common.defaultedField "castingRestrictions" [] (Common.decodeList CastingRestriction.fromJson) ps
  colorIndicator <- Common.defaultedField "colorIndicator" Set.empty (Common.decodeSet Color.fromJson) ps
  characteristicPT <- Common.defaultedField "characteristicPT" Nothing (Common.decodeMaybe Quantity.fromJson) ps
  delayed <- Common.defaultedField "delayedAbilities" Map.empty (TriggeredAbility.fromJsonDelayed fromJson) ps
  playerAbilities <- Common.defaultedField "playerAbilities" [] (Common.decodeList PlayerStaticAbility.fromJson) ps
  blockRequirements <- Common.defaultedField "blockRequirements" [] (Common.decodeList BlockRequirement.fromJson) ps
  attackRequirements <- Common.defaultedField "attackRequirements" [] (Common.decodeList AttackRequirement.fromJson) ps
  combatRestrictions <- Common.defaultedField "combatRestrictions" [] (Common.decodeList CombatRestriction.fromJson) ps
  attackCosts <- Common.defaultedField "attackCosts" [] (Common.decodeList AttackCost.fromJson) ps
  additionalCosts <- Common.defaultedField "additionalCosts" [] (Common.decodeList (CostComponent.fromJson Keyword.fromJson)) ps
  alternativeCosts <- Common.defaultedField "alternativeCosts" [] (Common.decodeList (Cost.fromJson Keyword.fromJson)) ps
  mulliganAction <- Common.defaultedField "mulliganAction" [] (Common.decodeList (Effect.fromJson fromJson)) ps
  openingHandAction <- Common.defaultedField "openingHandAction" [] (Common.decodeList (Effect.fromJson fromJson)) ps
  enchant <- Common.defaultedField "enchant" Nothing (Common.decodeMaybe TargetSpec.fromJson) ps
  counterability <- Common.defaultedField "counterability" Counterability.Counterable Counterability.fromJson ps
  pure
    Card.MkCard
      { Card.name = name,
        Card.manaCost = manaCost,
        Card.typeLine = typeLine,
        Card.power = power,
        Card.toughness = toughness,
        Card.loyalty = loyalty,
        Card.keywords = keywords,
        Card.staticAbilities = statics,
        Card.spell = spell,
        Card.activatedAbilities = activated,
        Card.replacementEffects = replacements,
        Card.triggeredAbilities = triggered,
        Card.castingPermissions = permissions,
        Card.castingRestrictions = restrictions,
        Card.colorIndicator = colorIndicator,
        Card.characteristicPT = characteristicPT,
        Card.delayedAbilities = delayed,
        Card.playerAbilities = playerAbilities,
        Card.blockRequirements = blockRequirements,
        Card.attackRequirements = attackRequirements,
        Card.combatRestrictions = combatRestrictions,
        Card.attackCosts = attackCosts,
        Card.additionalCosts = additionalCosts,
        Card.alternativeCosts = alternativeCosts,
        Card.mulliganAction = mulliganAction,
        Card.openingHandAction = openingHandAction,
        Card.enchant = enchant,
        Card.counterability = counterability
      }
