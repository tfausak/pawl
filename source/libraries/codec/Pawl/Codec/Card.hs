-- | @Card@ is the entry point to the rest of @Pawl.Codec@: the transitive
-- closure of @Card@'s fields is one module per type, each exposing free
-- @toJson@\/@fromJson@ functions rather than a type class, and this module is
-- where they are instantiated at @Card@ (§2 of the M3.5 spec).
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core --
-- mirroring 'Pawl.Engine.Resolve', which executes every @Effect@ and is the only
-- module allowed to case on one to decide WHAT IT DOES. The one other
-- @case@-on-@Effect@ in the engine is 'Pawl.Engine.ManaAbility', which answers CR
-- 605.1a's classification and executes nothing; its header says why it is not a
-- breach.
module Pawl.Codec.Card where

import qualified Data.Map.Strict as Map
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

toJson :: Card.Card -> Value.Value
toJson c =
  Common.object
    ( [ Common.pair "name" . CardName.toJson $ Card.name c,
        Common.pair "manaCost" . Common.encodeMaybe ManaCost.toJson $ Card.manaCost c,
        Common.pair "typeLine" . TypeLine.toJson $ Card.typeLine c,
        Common.pair "power" . Common.encodeMaybe Power.toJson $ Card.power c,
        Common.pair "toughness" . Common.encodeMaybe Toughness.toJson $ Card.toughness c,
        Common.pair "keywords" . Common.encodeSet Keyword.toJson $ Card.keywords c,
        Common.pair "staticAbilities" . Common.encodeList StaticAbility.toJson $ Card.staticAbilities c,
        Common.pair "spell" $ Modal.toJson toJson (Card.spell c),
        Common.pair "activatedAbilities" . Common.encodeList (ActivatedAbility.toJson toJson) $ Card.activatedAbilities c,
        Common.pair "replacementEffects" . Common.encodeList ReplacementEffect.toJson $ Card.replacementEffects c,
        Common.pair "triggeredAbilities" . Common.encodeList (TriggeredAbility.toJson toJson) $ Card.triggeredAbilities c,
        Common.pair "castingPermissions" . Common.encodeList CastingPermission.toJson $ Card.castingPermissions c
      ]
        -- CR 306.5: omitted for every card that is not a planeswalker, the
        -- counterability posture below rather than the power/toughness one. Those
        -- two are required keys spelled `null` on every noncreature because they
        -- predate the pool; a required loyalty key would have meant editing every
        -- other card file to say nothing.
        <> ( case Card.loyalty c of
               Nothing -> []
               Just l -> [Common.pair "loyalty" (Loyalty.toJson l)]
           )
        <> ( if Set.null (Card.colorIndicator c)
               then []
               else [Common.pair "colorIndicator" (Common.encodeSet Color.toJson (Card.colorIndicator c))]
           )
        <> ( case Card.characteristicPT c of
               Nothing -> []
               Just q -> [Common.pair "characteristicPT" (Quantity.toJson q)]
           )
        <> ( if Map.null (Card.delayedAbilities c)
               then []
               else [Common.pair "delayedAbilities" (TriggeredAbility.toJsonDelayed toJson (Card.delayedAbilities c))]
           )
        <> ( if null (Card.playerAbilities c)
               then []
               else [Common.pair "playerAbilities" (Common.encodeList PlayerStaticAbility.toJson (Card.playerAbilities c))]
           )
        <> ( if null (Card.blockRequirements c)
               then []
               else [Common.pair "blockRequirements" (Common.encodeList BlockRequirement.toJson (Card.blockRequirements c))]
           )
        <> ( if null (Card.attackRequirements c)
               then []
               else [Common.pair "attackRequirements" (Common.encodeList AttackRequirement.toJson (Card.attackRequirements c))]
           )
        <> ( if null (Card.combatRestrictions c)
               then []
               else [Common.pair "combatRestrictions" (Common.encodeList CombatRestriction.toJson (Card.combatRestrictions c))]
           )
        <> ( if null (Card.attackCosts c)
               then []
               else [Common.pair "attackCosts" (Common.encodeList AttackCost.toJson (Card.attackCosts c))]
           )
        <> ( if null (Card.additionalCosts c)
               then []
               else [Common.pair "additionalCosts" (Common.encodeList (CostComponent.toJson Keyword.toJson) (Card.additionalCosts c))]
           )
        <> ( if null (Card.alternativeCosts c)
               then []
               else [Common.pair "alternativeCosts" (Common.encodeList (Cost.toJson Keyword.toJson) (Card.alternativeCosts c))]
           )
        -- Omitted when Counterable, the posture every other defaulted key here
        -- takes: one card in the pool prints "this spell can't be countered", and
        -- a required key would have meant editing every other card file to say
        -- nothing.
        <> ( case Card.counterability c of
               Counterability.Counterable -> []
               Counterability.CantBeCountered -> [Common.pair "counterability" (Counterability.toJson (Card.counterability c))]
           )
        <> ( if null (Card.mulliganAction c)
               then []
               else [Common.pair "mulliganAction" (Common.encodeList (Effect.toJson toJson) (Card.mulliganAction c))]
           )
        <> ( if null (Card.openingHandAction c)
               then []
               else [Common.pair "openingHandAction" (Common.encodeList (Effect.toJson toJson) (Card.openingHandAction c))]
           )
        <> ( case Card.enchant c of
               Nothing -> []
               Just spec -> [Common.pair "enchant" (TargetSpec.toJson spec)]
           )
        -- Omitted when empty, unlike the required `castingPermissions` key it
        -- mirrors: one card in the pool prints a casting restriction, and a
        -- required key would have meant editing every other card file to say
        -- nothing.
        <> ( if null (Card.castingRestrictions c)
               then []
               else [Common.pair "castingRestrictions" (Common.encodeList CastingRestriction.toJson (Card.castingRestrictions c))]
           )
    )

fromJson :: Value.Value -> Either Text.Text Card.Card
fromJson value = do
  ps <- Common.asObject value
  name <- Common.field "name" ps >>= CardName.fromJson
  manaCost <- Common.decodeMaybe ManaCost.fromJson (Common.nullableField "manaCost" ps)
  typeLine <- Common.field "typeLine" ps >>= TypeLine.fromJson
  power <- Common.decodeMaybe Power.fromJson (Common.nullableField "power" ps)
  toughness <- Common.decodeMaybe Toughness.fromJson (Common.nullableField "toughness" ps)
  loyalty <- Common.decodeMaybe Loyalty.fromJson (Common.nullableField "loyalty" ps)
  keywords <- Common.field "keywords" ps >>= Common.decodeSet Keyword.fromJson
  statics <- Common.field "staticAbilities" ps >>= Common.decodeList StaticAbility.fromJson
  spell <- Common.field "spell" ps >>= Modal.fromJson fromJson
  activated <- Common.field "activatedAbilities" ps >>= Common.decodeList (ActivatedAbility.fromJson fromJson)
  replacements <- Common.field "replacementEffects" ps >>= Common.decodeList ReplacementEffect.fromJson
  triggered <- Common.field "triggeredAbilities" ps >>= Common.decodeList (TriggeredAbility.fromJson fromJson)
  permissions <- Common.field "castingPermissions" ps >>= Common.decodeList CastingPermission.fromJson
  restrictions <- Common.decodeListDefault CastingRestriction.fromJson (Common.nullableField "castingRestrictions" ps)
  colorIndicator <- Common.decodeSetDefault Color.fromJson (Common.nullableField "colorIndicator" ps)
  characteristicPT <- Common.decodeMaybe Quantity.fromJson (Common.nullableField "characteristicPT" ps)
  delayed <- Common.decodeMapDefault (TriggeredAbility.fromJsonDelayed fromJson) (Common.nullableField "delayedAbilities" ps)
  playerAbilities <- Common.decodeListDefault PlayerStaticAbility.fromJson (Common.nullableField "playerAbilities" ps)
  blockRequirements <- Common.decodeListDefault BlockRequirement.fromJson (Common.nullableField "blockRequirements" ps)
  attackRequirements <- Common.decodeListDefault AttackRequirement.fromJson (Common.nullableField "attackRequirements" ps)
  combatRestrictions <- Common.decodeListDefault CombatRestriction.fromJson (Common.nullableField "combatRestrictions" ps)
  attackCosts <- Common.decodeListDefault AttackCost.fromJson (Common.nullableField "attackCosts" ps)
  additionalCosts <- Common.decodeListDefault (CostComponent.fromJson Keyword.fromJson) (Common.nullableField "additionalCosts" ps)
  alternativeCosts <- Common.decodeListDefault (Cost.fromJson Keyword.fromJson) (Common.nullableField "alternativeCosts" ps)
  mulliganAction <- Common.decodeListDefault (Effect.fromJson fromJson) (Common.nullableField "mulliganAction" ps)
  openingHandAction <- Common.decodeListDefault (Effect.fromJson fromJson) (Common.nullableField "openingHandAction" ps)
  enchant <- Common.decodeMaybe TargetSpec.fromJson (Common.nullableField "enchant" ps)
  counterability <- Counterability.fromJsonDefault (Common.nullableField "counterability" ps)
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
