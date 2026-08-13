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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.AlternativeCost as AlternativeCost
import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.Codec.DungeonRoom as DungeonRoom
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
import qualified Pawl.Codec.UntapRestriction as UntapRestriction
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.TypeLine as TypeLine

-- `Eq card` because six of the fields below carry card-shaped payloads and
-- 'Common.optionalPair' omits a key by comparing its value to the default.
toJson :: (Eq card) => (card -> Value.Value) -> Face.Face card -> Value.Value
toJson encodeCard f =
  Value.object . concat $
    [ Common.requiredPair "name" (Codec.encode CardName.codec) (Face.name f),
      -- CR 205.1 puts a type line on every card and CR 114.3 gives an emblem no
      -- types at all, so the key is optional and its absence means the latter --
      -- see fromJson.
      Common.optionalPair "typeLine" TypeLine.empty (Codec.encode TypeLine.codec) (Face.typeLine f),
      Common.optionalPair "manaCost" Nothing (Common.encodeMaybe (Codec.encode ManaCost.codec)) (Face.manaCost f),
      Common.optionalPair "power" Nothing (Common.encodeMaybe Power.toJson) (Face.power f),
      Common.optionalPair "toughness" Nothing (Common.encodeMaybe Toughness.toJson) (Face.toughness f),
      Common.optionalPair "loyalty" Nothing (Common.encodeMaybe (Codec.encode Loyalty.codec)) (Face.loyalty f),
      Common.optionalPair "defense" Nothing (Common.encodeMaybe (Codec.encode Defense.codec)) (Face.defense f),
      Common.optionalPair "characteristicPT" Nothing (Common.encodeMaybe (Codec.encode Quantity.codec)) (Face.characteristicPT f),
      Common.optionalPair "enchant" [] (Common.encodeList (Codec.encode TargetSpec.codec)) (Face.enchant f),
      Common.optionalPair "keywords" Set.empty (Common.encodeSet (Codec.encode Keyword.codec)) (Face.keywords f),
      Common.optionalPair "colorIndicator" Set.empty (Common.encodeSet (Codec.encode Color.codec)) (Face.colorIndicator f),
      Common.optionalPair "spell" Face.defaultSpell (Modal.toJson encodeCard) (Face.spell f),
      Common.optionalPair "staticAbilities" [] (Common.encodeList StaticAbility.toJson) (Face.staticAbilities f),
      Common.optionalPair "activatedAbilities" [] (Common.encodeList (ActivatedAbility.toJson encodeCard)) (Face.activatedAbilities f),
      Common.optionalPair "replacementEffects" [] (Common.encodeList (Codec.encode ReplacementEffect.codec)) (Face.replacementEffects f),
      Common.optionalPair "triggeredAbilities" [] (Common.encodeList (TriggeredAbility.toJson encodeCard)) (Face.triggeredAbilities f),
      Common.optionalPair "delayedAbilities" Map.empty (TriggeredAbility.toJsonDelayed encodeCard) (Face.delayedAbilities f),
      -- CR 309.4: the rooms of a dungeon card, topmost first.
      Common.optionalPair "rooms" Seq.empty (Common.encodeSeq (DungeonRoom.toJson encodeCard)) (Face.rooms f),
      Common.optionalPair "castingPermissions" [] (Common.encodeList (Codec.encode CastingPermission.codec)) (Face.castingPermissions f),
      Common.optionalPair "castingRestrictions" [] (Common.encodeList (Codec.encode CastingRestriction.codec)) (Face.castingRestrictions f),
      Common.optionalPair "playerAbilities" [] (Common.encodeList PlayerStaticAbility.toJson) (Face.playerAbilities f),
      Common.optionalPair "blockRequirements" [] (Common.encodeList BlockRequirement.toJson) (Face.blockRequirements f),
      Common.optionalPair "blockPermissions" [] (Common.encodeList BlockPermission.toJson) (Face.blockPermissions f),
      Common.optionalPair "attackRequirements" [] (Common.encodeList AttackRequirement.toJson) (Face.attackRequirements f),
      Common.optionalPair "combatRestrictions" [] (Common.encodeList CombatRestriction.toJson) (Face.combatRestrictions f),
      Common.optionalPair "sacrificeRestrictions" [] (Common.encodeList SacrificeRestriction.toJson) (Face.sacrificeRestrictions f),
      Common.optionalPair "untapRestrictions" [] (Common.encodeList UntapRestriction.toJson) (Face.untapRestrictions f),
      Common.optionalPair "attackCosts" [] (Common.encodeList AttackCost.toJson) (Face.attackCosts f),
      Common.optionalPair "additionalCosts" [] (Common.encodeList (Codec.encode (CostComponent.codec Keyword.codec))) (Face.additionalCosts f),
      Common.optionalPair "alternativeCosts" [] (Common.encodeList AlternativeCost.toJson) (Face.alternativeCosts f),
      -- CR 103.5b / CR 103.6: an array of ACTIONS, each an array of effects, so a
      -- face granting two of them is writable (Pawl.Types.Face).
      Common.optionalPair "mulliganActions" [] (Common.encodeList (Common.encodeList (Effect.toJson encodeCard))) (Face.mulliganActions f),
      Common.optionalPair "openingHandActions" [] (Common.encodeList (Common.encodeList (Effect.toJson encodeCard))) (Face.openingHandActions f),
      -- CR 116.2: the special actions this face grants (Pawl.Types.Face).
      Common.optionalPair "specialActions" [] (Common.encodeList (Codec.encode SpecialAction.codec)) (Face.specialActions f),
      -- CR 113.6g: Counterable is the absence of a card stating it can't be
      -- countered.
      Common.optionalPair "counterability" Counterability.Counterable (Codec.encode Counterability.codec) (Face.counterability f)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Face.Face card)
fromJson decodeCard value = do
  ps <- Common.asObject value
  name <- Common.field "name" ps >>= Codec.decode CardName.codec
  -- CR 114.3: an emblem "has no characteristics other than the abilities defined
  -- by the effect that created it. In particular, an emblem has no types" -- and
  -- an emblem's face is authored as the payload of Effect.CreateEmblem, decoded
  -- through this same codec. So an ABSENT key is CR 114.3's answer, while a key
  -- that is present must still name a card type, which is Pawl.Codec.TypeLine's
  -- own reading of CR 205.1. Which faces may leave it out is a question
  -- about the corpus rather than about the wire, and Pawl.CardSpec's lint --
  -- "only an emblem's face has no card type" -- is what asks it.
  typeLine <- Common.defaultedField "typeLine" TypeLine.empty (Codec.decode TypeLine.codec) ps
  manaCost <- Common.defaultedField "manaCost" Nothing (Common.decodeMaybe (Codec.decode ManaCost.codec)) ps
  power <- Common.defaultedField "power" Nothing (Common.decodeMaybe Power.fromJson) ps
  toughness <- Common.defaultedField "toughness" Nothing (Common.decodeMaybe Toughness.fromJson) ps
  loyalty <- Common.defaultedField "loyalty" Nothing (Common.decodeMaybe (Codec.decode Loyalty.codec)) ps
  defense <- Common.defaultedField "defense" Nothing (Common.decodeMaybe (Codec.decode Defense.codec)) ps
  keywords <- Common.defaultedField "keywords" Set.empty (Common.decodeSet (Codec.decode Keyword.codec)) ps
  statics <- Common.defaultedField "staticAbilities" [] (Common.decodeList StaticAbility.fromJson) ps
  spell <- Common.defaultedField "spell" Face.defaultSpell (Modal.fromJson decodeCard) ps
  activated <- Common.defaultedField "activatedAbilities" [] (Common.decodeList (ActivatedAbility.fromJson decodeCard)) ps
  replacements <- Common.defaultedField "replacementEffects" [] (Common.decodeList (Codec.decode ReplacementEffect.codec)) ps
  triggered <- Common.defaultedField "triggeredAbilities" [] (Common.decodeList (TriggeredAbility.fromJson decodeCard)) ps
  permissions <- Common.defaultedField "castingPermissions" [] (Common.decodeList (Codec.decode CastingPermission.codec)) ps
  restrictions <- Common.defaultedField "castingRestrictions" [] (Common.decodeList (Codec.decode CastingRestriction.codec)) ps
  colorIndicator <- Common.defaultedField "colorIndicator" Set.empty (Common.decodeSet (Codec.decode Color.codec)) ps
  characteristicPT <- Common.defaultedField "characteristicPT" Nothing (Common.decodeMaybe (Codec.decode Quantity.codec)) ps
  delayed <- Common.defaultedField "delayedAbilities" Map.empty (TriggeredAbility.fromJsonDelayed decodeCard) ps
  rooms <- Common.defaultedField "rooms" Seq.empty (Common.decodeSeq (DungeonRoom.fromJson decodeCard)) ps
  playerAbilities <- Common.defaultedField "playerAbilities" [] (Common.decodeList PlayerStaticAbility.fromJson) ps
  blockRequirements <- Common.defaultedField "blockRequirements" [] (Common.decodeList BlockRequirement.fromJson) ps
  blockPermissions <- Common.defaultedField "blockPermissions" [] (Common.decodeList BlockPermission.fromJson) ps
  attackRequirements <- Common.defaultedField "attackRequirements" [] (Common.decodeList AttackRequirement.fromJson) ps
  combatRestrictions <- Common.defaultedField "combatRestrictions" [] (Common.decodeList CombatRestriction.fromJson) ps
  sacrificeRestrictions <- Common.defaultedField "sacrificeRestrictions" [] (Common.decodeList SacrificeRestriction.fromJson) ps
  untapRestrictions <- Common.defaultedField "untapRestrictions" [] (Common.decodeList UntapRestriction.fromJson) ps
  attackCosts <- Common.defaultedField "attackCosts" [] (Common.decodeList AttackCost.fromJson) ps
  additionalCosts <- Common.defaultedField "additionalCosts" [] (Common.decodeList (Codec.decode (CostComponent.codec Keyword.codec))) ps
  alternativeCosts <- Common.defaultedField "alternativeCosts" [] (Common.decodeList AlternativeCost.fromJson) ps
  mulliganActions <- Common.defaultedField "mulliganActions" [] (Common.decodeList (Common.decodeList (Effect.fromJson decodeCard))) ps
  openingHandActions <- Common.defaultedField "openingHandActions" [] (Common.decodeList (Common.decodeList (Effect.fromJson decodeCard))) ps
  specialActions <- Common.defaultedField "specialActions" [] (Common.decodeList (Codec.decode SpecialAction.codec)) ps
  enchant <- Common.defaultedField "enchant" [] (Common.decodeList (Codec.decode TargetSpec.codec)) ps
  counterability <- Common.defaultedField "counterability" Counterability.Counterable (Codec.decode Counterability.codec) ps
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
        Face.rooms = rooms,
        Face.playerAbilities = playerAbilities,
        Face.blockRequirements = blockRequirements,
        Face.blockPermissions = blockPermissions,
        Face.attackRequirements = attackRequirements,
        Face.combatRestrictions = combatRestrictions,
        Face.sacrificeRestrictions = sacrificeRestrictions,
        Face.untapRestrictions = untapRestrictions,
        Face.attackCosts = attackCosts,
        Face.additionalCosts = additionalCosts,
        Face.alternativeCosts = alternativeCosts,
        Face.mulliganActions = mulliganActions,
        Face.openingHandActions = openingHandActions,
        Face.specialActions = specialActions,
        Face.enchant = enchant,
        Face.counterability = counterability
      }
