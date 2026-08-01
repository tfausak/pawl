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
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Json as Json
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
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Card as CardT
import qualified Pawl.Types.Counterability as Counterability

cardToJson :: CardT.Card -> Value
cardToJson c =
  Json.jObject
    ( [ (Text.pack "name", Json.jText (CardT.name c)),
        (Text.pack "manaCost", Json.maybeTo ManaCost.toJson (CardT.manaCost c)),
        (Text.pack "typeLine", TypeLine.toJson (CardT.typeLine c)),
        (Text.pack "power", Json.maybeTo Power.toJson (CardT.power c)),
        (Text.pack "toughness", Json.maybeTo Toughness.toJson (CardT.toughness c)),
        (Text.pack "keywords", Json.setTo Keyword.toJson (CardT.keywords c)),
        (Text.pack "staticAbilities", Json.listTo StaticAbility.toJson (CardT.staticAbilities c)),
        (Text.pack "spell", Modal.toJson cardToJson (CardT.spell c)),
        (Text.pack "activatedAbilities", Json.listTo (ActivatedAbility.toJson cardToJson) (CardT.activatedAbilities c)),
        (Text.pack "replacementEffects", Json.listTo ReplacementEffect.toJson (CardT.replacementEffects c)),
        (Text.pack "triggeredAbilities", Json.listTo (TriggeredAbility.toJson cardToJson) (CardT.triggeredAbilities c)),
        (Text.pack "castingPermissions", Json.listTo CastingPermission.toJson (CardT.castingPermissions c))
      ]
        -- CR 306.5: omitted for every card that is not a planeswalker, the
        -- counterability posture below rather than the power/toughness one. Those
        -- two are required keys spelled `null` on every noncreature because they
        -- predate the pool; a required loyalty key would have meant editing every
        -- other card file to say nothing.
        <> ( case CardT.loyalty c of
               Nothing -> []
               Just l -> [(Text.pack "loyalty", Loyalty.toJson l)]
           )
        <> ( if Set.null (CardT.colorIndicator c)
               then []
               else [(Text.pack "colorIndicator", Json.setTo Color.toJson (CardT.colorIndicator c))]
           )
        <> ( case CardT.characteristicPT c of
               Nothing -> []
               Just q -> [(Text.pack "characteristicPT", Quantity.toJson q)]
           )
        <> ( if Map.null (CardT.delayedAbilities c)
               then []
               else [(Text.pack "delayedAbilities", TriggeredAbility.toJsonDelayed cardToJson (CardT.delayedAbilities c))]
           )
        <> ( if null (CardT.playerAbilities c)
               then []
               else [(Text.pack "playerAbilities", Json.listTo PlayerStaticAbility.toJson (CardT.playerAbilities c))]
           )
        <> ( if null (CardT.blockRequirements c)
               then []
               else [(Text.pack "blockRequirements", Json.listTo BlockRequirement.toJson (CardT.blockRequirements c))]
           )
        <> ( if null (CardT.attackRequirements c)
               then []
               else [(Text.pack "attackRequirements", Json.listTo AttackRequirement.toJson (CardT.attackRequirements c))]
           )
        <> ( if null (CardT.additionalCosts c)
               then []
               else [(Text.pack "additionalCosts", Json.listTo CostComponent.toJson (CardT.additionalCosts c))]
           )
        <> ( if null (CardT.alternativeCosts c)
               then []
               else [(Text.pack "alternativeCosts", Json.listTo Cost.toJson (CardT.alternativeCosts c))]
           )
        -- Omitted when Counterable, the posture every other defaulted key here
        -- takes: one card in the pool prints "this spell can't be countered", and
        -- a required key would have meant editing every other card file to say
        -- nothing.
        <> ( case CardT.counterability c of
               Counterability.Counterable -> []
               Counterability.CantBeCountered -> [(Text.pack "counterability", Counterability.toJson (CardT.counterability c))]
           )
        <> ( if null (CardT.mulliganAction c)
               then []
               else [(Text.pack "mulliganAction", Json.listTo (Effect.toJson cardToJson) (CardT.mulliganAction c))]
           )
        <> ( if null (CardT.openingHandAction c)
               then []
               else [(Text.pack "openingHandAction", Json.listTo (Effect.toJson cardToJson) (CardT.openingHandAction c))]
           )
        <> ( case CardT.enchant c of
               Nothing -> []
               Just spec -> [(Text.pack "enchant", TargetSpec.toJson spec)]
           )
        -- Omitted when empty, unlike the required `castingPermissions` key it
        -- mirrors: one card in the pool prints a casting restriction, and a
        -- required key would have meant editing every other card file to say
        -- nothing.
        <> ( if null (CardT.castingRestrictions c)
               then []
               else [(Text.pack "castingRestrictions", Json.listTo CastingRestriction.toJson (CardT.castingRestrictions c))]
           )
    )

jsonToCard :: Value -> Either Text CardT.Card
jsonToCard value = do
  ps <- Json.asObject value
  name <- Json.field (Text.pack "name") ps >>= Json.asText
  manaCost <- Json.maybeFrom ManaCost.fromJson (Json.getOpt (Text.pack "manaCost") ps)
  typeLine <- Json.field (Text.pack "typeLine") ps >>= TypeLine.fromJson
  power <- Json.maybeFrom Power.fromJson (Json.getOpt (Text.pack "power") ps)
  toughness <- Json.maybeFrom Toughness.fromJson (Json.getOpt (Text.pack "toughness") ps)
  loyalty <- Json.maybeFrom Loyalty.fromJson (Json.getOpt (Text.pack "loyalty") ps)
  keywords <- Json.field (Text.pack "keywords") ps >>= Json.setFrom Keyword.fromJson
  statics <- Json.field (Text.pack "staticAbilities") ps >>= Json.listFrom StaticAbility.fromJson
  spell <- Json.field (Text.pack "spell") ps >>= Modal.fromJson jsonToCard
  activated <- Json.field (Text.pack "activatedAbilities") ps >>= Json.listFrom (ActivatedAbility.fromJson jsonToCard)
  replacements <- Json.field (Text.pack "replacementEffects") ps >>= Json.listFrom ReplacementEffect.fromJson
  triggered <- Json.field (Text.pack "triggeredAbilities") ps >>= Json.listFrom (TriggeredAbility.fromJson jsonToCard)
  permissions <- Json.field (Text.pack "castingPermissions") ps >>= Json.listFrom CastingPermission.fromJson
  restrictions <- Json.listFromDefault CastingRestriction.fromJson (Json.getOpt (Text.pack "castingRestrictions") ps)
  colorIndicator <- Json.setFromDefault Color.fromJson (Json.getOpt (Text.pack "colorIndicator") ps)
  characteristicPT <- Json.maybeFrom Quantity.fromJson (Json.getOpt (Text.pack "characteristicPT") ps)
  delayed <- Json.mapFromDefault (TriggeredAbility.fromJsonDelayed jsonToCard) (Json.getOpt (Text.pack "delayedAbilities") ps)
  playerAbilities <- Json.listFromDefault PlayerStaticAbility.fromJson (Json.getOpt (Text.pack "playerAbilities") ps)
  blockRequirements <- Json.listFromDefault BlockRequirement.fromJson (Json.getOpt (Text.pack "blockRequirements") ps)
  attackRequirements <- Json.listFromDefault AttackRequirement.fromJson (Json.getOpt (Text.pack "attackRequirements") ps)
  additionalCosts <- Json.listFromDefault CostComponent.fromJson (Json.getOpt (Text.pack "additionalCosts") ps)
  alternativeCosts <- Json.listFromDefault Cost.fromJson (Json.getOpt (Text.pack "alternativeCosts") ps)
  mulliganAction <- Json.listFromDefault (Effect.fromJson jsonToCard) (Json.getOpt (Text.pack "mulliganAction") ps)
  openingHandAction <- Json.listFromDefault (Effect.fromJson jsonToCard) (Json.getOpt (Text.pack "openingHandAction") ps)
  enchant <- Json.maybeFrom TargetSpec.fromJson (Json.getOpt (Text.pack "enchant") ps)
  counterability <- Counterability.fromJsonDefault (Json.getOpt (Text.pack "counterability") ps)
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
        CardT.additionalCosts = additionalCosts,
        CardT.alternativeCosts = alternativeCosts,
        CardT.mulliganAction = mulliganAction,
        CardT.openingHandAction = openingHandAction,
        CardT.enchant = enchant,
        CardT.counterability = counterability
      }
