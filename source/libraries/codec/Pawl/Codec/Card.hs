-- | The @Card ⇆ Json@ codec (§2 of the M3.5 spec), and the entry point to the
-- rest of @Pawl.Codec@: the transitive closure of @Card@'s fields is one
-- module per type, each exposing free @xToJson@\/@jsonToX@ functions rather
-- than a type class, and this module is where they are instantiated at
-- @Card@ (#481).
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core --
-- mirroring 'Pawl.Engine.Resolve', the sole @case@-on-@Effect@ home.
module Pawl.Codec.Card where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.ActivatedAbility (activatedAbilityToJson, jsonToActivatedAbility)
import Pawl.Codec.AttackRequirement (attackRequirementToJson, jsonToAttackRequirement)
import Pawl.Codec.BlockRequirement (blockRequirementToJson, jsonToBlockRequirement)
import Pawl.Codec.CastingPermission (castingPermissionToJson, jsonToCastingPermission)
import Pawl.Codec.CastingRestriction (castingRestrictionToJson, jsonToCastingRestriction)
import Pawl.Codec.Color (colorToJson, jsonToColor)
import Pawl.Codec.CombatRestriction (combatRestrictionToJson, jsonToCombatRestriction)
import Pawl.Codec.Cost (costToJson, jsonToCost)
import Pawl.Codec.CostComponent (costComponentToJson, jsonToCostComponent)
import Pawl.Codec.Counterability (counterabilityToJson, jsonToCounterabilityDefault)
import Pawl.Codec.Effect (effectToJson, jsonToEffect)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Codec.Loyalty (jsonToLoyalty, loyaltyToJson)
import Pawl.Codec.ManaCost (jsonToManaCost, manaCostToJson)
import Pawl.Codec.Modal (jsonToModal, modalToJson)
import Pawl.Codec.PlayerStaticAbility (jsonToPlayerStaticAbility, playerStaticAbilityToJson)
import Pawl.Codec.Power (jsonToPower, powerToJson)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Codec.ReplacementEffect (jsonToReplacementEffect, replacementEffectToJson)
import Pawl.Codec.StaticAbility (jsonToStaticAbility, staticAbilityToJson)
import Pawl.Codec.TargetSpec (jsonToTargetSpec, targetSpecToJson)
import Pawl.Codec.Toughness (jsonToToughness, toughnessToJson)
import Pawl.Codec.TriggeredAbility (delayedAbilitiesToJson, jsonToDelayedAbilities, jsonToTriggeredAbility, triggeredAbilityToJson)
import Pawl.Codec.TypeLine (jsonToTypeLine, typeLineToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.Counterability as Counterability

cardToJson :: CardT.Card -> Value
cardToJson c =
  Json.jObject
    ( [ (Text.pack "name", Json.jText (CardT.name c)),
        (Text.pack "manaCost", Json.maybeTo manaCostToJson (CardT.manaCost c)),
        (Text.pack "typeLine", typeLineToJson (CardT.typeLine c)),
        (Text.pack "power", Json.maybeTo powerToJson (CardT.power c)),
        (Text.pack "toughness", Json.maybeTo toughnessToJson (CardT.toughness c)),
        (Text.pack "keywords", Json.setTo keywordToJson (CardT.keywords c)),
        (Text.pack "staticAbilities", Json.listTo staticAbilityToJson (CardT.staticAbilities c)),
        (Text.pack "spell", modalToJson cardToJson (CardT.spell c)),
        (Text.pack "activatedAbilities", Json.listTo (activatedAbilityToJson cardToJson) (CardT.activatedAbilities c)),
        (Text.pack "replacementEffects", Json.listTo replacementEffectToJson (CardT.replacementEffects c)),
        (Text.pack "triggeredAbilities", Json.listTo (triggeredAbilityToJson cardToJson) (CardT.triggeredAbilities c)),
        (Text.pack "castingPermissions", Json.listTo castingPermissionToJson (CardT.castingPermissions c))
      ]
        -- CR 306.5: omitted for every card that is not a planeswalker, the
        -- counterability posture below rather than the power/toughness one. Those
        -- two are required keys spelled `null` on every noncreature because they
        -- predate the pool; a required loyalty key would have meant editing every
        -- other card file to say nothing.
        <> ( case CardT.loyalty c of
               Nothing -> []
               Just l -> [(Text.pack "loyalty", loyaltyToJson l)]
           )
        <> ( if Set.null (CardT.colorIndicator c)
               then []
               else [(Text.pack "colorIndicator", Json.setTo colorToJson (CardT.colorIndicator c))]
           )
        <> ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", quantityToJson q)]
           )
        <> ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", delayedAbilitiesToJson cardToJson (CardT.delayedAbilities c))]
           )
        <> ( if null (CardT.playerAbilities c)
               then []
               else [(Text.pack "playerAbilities", Json.listTo playerStaticAbilityToJson (CardT.playerAbilities c))]
           )
        <> ( if null (CardT.blockRequirements c)
               then []
               else [(Text.pack "blockRequirements", Json.listTo blockRequirementToJson (CardT.blockRequirements c))]
           )
        <> ( if null (CardT.attackRequirements c)
               then []
               else [(Text.pack "attackRequirements", Json.listTo attackRequirementToJson (CardT.attackRequirements c))]
           )
        <> ( if null (CardT.combatRestrictions c)
               then []
               else [(Text.pack "combatRestrictions", Json.listTo combatRestrictionToJson (CardT.combatRestrictions c))]
           )
        <> ( if null (CardT.additionalCosts c)
               then []
               else [(Text.pack "additionalCosts", Json.listTo costComponentToJson (CardT.additionalCosts c))]
           )
        <> ( if null (CardT.alternativeCosts c)
               then []
               else [(Text.pack "alternativeCosts", Json.listTo costToJson (CardT.alternativeCosts c))]
           )
        -- Omitted when Counterable, the posture every other defaulted key here
        -- takes: one card in the pool prints "this spell can't be countered", and
        -- a required key would have meant editing every other card file to say
        -- nothing.
        <> ( case CardT.counterability c of
               Counterability.Counterable -> []
               Counterability.CantBeCountered -> [(Text.pack "counterability", counterabilityToJson (CardT.counterability c))]
           )
        <> ( if null (CardT.mulliganAction c)
               then []
               else [(Text.pack "mulliganAction", Json.listTo (effectToJson cardToJson) (CardT.mulliganAction c))]
           )
        <> ( if null (CardT.openingHandAction c)
               then []
               else [(Text.pack "openingHandAction", Json.listTo (effectToJson cardToJson) (CardT.openingHandAction c))]
           )
        <> ( case CardT.enchant c of
               Nothing -> []
               Just spec -> [(Text.pack "enchant", targetSpecToJson spec)]
           )
        -- Omitted when empty, unlike the required `castingPermissions` key it
        -- mirrors: one card in the pool prints a casting restriction, and a
        -- required key would have meant editing every other card file to say
        -- nothing.
        <> ( if null (CardT.castingRestrictions c)
               then []
               else [(Text.pack "castingRestrictions", Json.listTo castingRestrictionToJson (CardT.castingRestrictions c))]
           )
    )

jsonToCard :: Value -> Either Text CardT.Card
jsonToCard value = do
  ps <- Json.asObject value
  name <- Json.field (Text.pack "name") ps >>= Json.asText
  manaCost <- Json.maybeFrom jsonToManaCost (Json.getOpt (Text.pack "manaCost") ps)
  typeLine <- Json.field (Text.pack "typeLine") ps >>= jsonToTypeLine
  power <- Json.maybeFrom jsonToPower (Json.getOpt (Text.pack "power") ps)
  toughness <- Json.maybeFrom jsonToToughness (Json.getOpt (Text.pack "toughness") ps)
  loyalty <- Json.maybeFrom jsonToLoyalty (Json.getOpt (Text.pack "loyalty") ps)
  keywords <- Json.field (Text.pack "keywords") ps >>= Json.setFrom jsonToKeyword
  statics <- Json.field (Text.pack "staticAbilities") ps >>= Json.listFrom jsonToStaticAbility
  spell <- Json.field (Text.pack "spell") ps >>= jsonToModal jsonToCard
  activated <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (jsonToActivatedAbility jsonToCard)
  replacements <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom jsonToReplacementEffect
  triggered <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (jsonToTriggeredAbility jsonToCard)
  permissions <- Json.field (Text.pack "castingPermissions") ps >>= Json.listFrom jsonToCastingPermission
  restrictions <- Json.listFromDefault jsonToCastingRestriction (Json.getOpt (Text.pack "castingRestrictions") ps)
  colorIndicator <- Json.setFromDefault jsonToColor (Json.getOpt (Text.pack "colorIndicator") ps)
  characteristicPT <- Json.maybeFrom jsonToQuantity (Json.getOpt (Text.pack "characteristicPT") ps)
  delayed <- Json.mapFromDefault (jsonToDelayedAbilities jsonToCard) (Json.getOpt (Text.pack "delayedAbilities") ps)
  playerAbilities <- Json.listFromDefault jsonToPlayerStaticAbility (Json.getOpt (Text.pack "playerAbilities") ps)
  blockRequirements <- Json.listFromDefault jsonToBlockRequirement (Json.getOpt (Text.pack "blockRequirements") ps)
  attackRequirements <- Json.listFromDefault jsonToAttackRequirement (Json.getOpt (Text.pack "attackRequirements") ps)
  combatRestrictions <- Json.listFromDefault jsonToCombatRestriction (Json.getOpt (Text.pack "combatRestrictions") ps)
  additionalCosts <- Json.listFromDefault jsonToCostComponent (Json.getOpt (Text.pack "additionalCosts") ps)
  alternativeCosts <- Json.listFromDefault jsonToCost (Json.getOpt (Text.pack "alternativeCosts") ps)
  mulliganAction <- Json.listFromDefault (jsonToEffect jsonToCard) (Json.getOpt (Text.pack "mulliganAction") ps)
  openingHandAction <- Json.listFromDefault (jsonToEffect jsonToCard) (Json.getOpt (Text.pack "openingHandAction") ps)
  enchant <- Json.maybeFrom jsonToTargetSpec (Json.getOpt (Text.pack "enchant") ps)
  counterability <- jsonToCounterabilityDefault (Json.getOpt (Text.pack "counterability") ps)
  pure
    CardT.MkCard
      { CardT.name = name,
        CardT.manaCost = manaCost,
        CardT.typeLine = typeLine,
        CardT.power = power,
        CardT.toughness = toughness,
        CardT.loyalty = loyalty,
        CardT.keywords = keywords,
        CardT.staticAbilities = statics,
        CardT.spell = spell,
        CardT.activatedAbilities = activated,
        CardT.replacementEffects = replacements,
        CardT.triggeredAbilities = triggered,
        CardT.castingPermissions = permissions,
        CardT.castingRestrictions = restrictions,
        CardT.colorIndicator = colorIndicator,
        CardT.characteristicPT = characteristicPT,
        CardT.delayedAbilities = delayed,
        CardT.playerAbilities = playerAbilities,
        CardT.blockRequirements = blockRequirements,
        CardT.attackRequirements = attackRequirements,
        CardT.combatRestrictions = combatRestrictions,
        CardT.additionalCosts = additionalCosts,
        CardT.alternativeCosts = alternativeCosts,
        CardT.mulliganAction = mulliganAction,
        CardT.openingHandAction = openingHandAction,
        CardT.enchant = enchant,
        CardT.counterability = counterability
      }
