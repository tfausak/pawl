{-# LANGUAGE ApplicativeDo #-}

-- | @Face@ is the entry point to the rest of @Pawl.Codec@: the transitive
-- closure of @Face@'s fields is one module per type, each exposing a 'Codec.Codec'
-- bundle, and this module is where they are instantiated at @Face@.
--
-- Parametric in the card codec the way 'Pawl.Types.Face.Face' is parametric in
-- @card@: @Pawl.Codec.Card@ passes its own in, which is where the knot is tied.
--
-- Every @Pawl.Types.*@ module stays JSON-free. Casing on an effect's identity
-- anywhere under @Pawl.Codec@ is open-half machinery, not the rules core.
module Pawl.Codec.Face where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.ActivatedAbility as ActivatedAbility
import qualified Pawl.Codec.AlternativeCost as AlternativeCost
import qualified Pawl.Codec.AttackCost as AttackCost
import qualified Pawl.Codec.AttackRequirement as AttackRequirement
import qualified Pawl.Codec.BlockCost as BlockCost
import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.BlockRequirement as BlockRequirement
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.Codec.CastingRestriction as CastingRestriction
import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.CombatRestriction as CombatRestriction
import qualified Pawl.Codec.CostComponent as CostComponent
import qualified Pawl.Codec.CostReduction as CostReduction
import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.Codec.DungeonRoom as DungeonRoom
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.HandAction as HandAction
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Modal as Modal
import qualified Pawl.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Codec.Power as Power
import qualified Pawl.Codec.PrintedReplacement as PrintedReplacement
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SacrificeRestriction as SacrificeRestriction
import qualified Pawl.Codec.SpecialAction as SpecialAction
import qualified Pawl.Codec.StaticAbility as StaticAbility
import qualified Pawl.Codec.TargetSlot as TargetSlot
import qualified Pawl.Codec.Toughness as Toughness
import qualified Pawl.Codec.TriggeredAbility as TriggeredAbility
import qualified Pawl.Codec.TypeLine as TypeLine
import qualified Pawl.Codec.UntapRestriction as UntapRestriction
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Counterability as Counterability.Type
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.TypeLine as TypeLine

-- | @Eq card@ because several fields below carry card-shaped payloads and
-- 'Fields.defaulted' omits a key by comparing its value to the default.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: (Typeable.Typeable card, Eq card) => Codec.Codec card -> Codec.Codec (Face.Face card)
codec cardCodec = Fields.object $ do
  name <- Fields.required "name" CardName.codec Face.name
  -- CR 205.1 puts a type line on every card and CR 114.3 gives an emblem no
  -- types at all, so the key is optional and its absence means the latter: an
  -- emblem's face is authored as the payload of Effect.CreateEmblem and decoded
  -- through this same codec. A key that IS present must still name a card type,
  -- which is Pawl.Codec.TypeLine's own reading of CR 205.1. Which faces may
  -- leave it out is a question about the corpus rather than about the wire, and
  -- Pawl.CardSpec's lint -- "only an emblem's face has no card type" -- asks it.
  typeLine <- Fields.defaulted "typeLine" TypeLine.empty TypeLine.codec Face.typeLine
  manaCost <- Fields.defaulted "manaCost" Nothing (Common.maybe ManaCost.codec) Face.manaCost
  power <- Fields.defaulted "power" Nothing (Common.maybe Power.codec) Face.power
  toughness <- Fields.defaulted "toughness" Nothing (Common.maybe Toughness.codec) Face.toughness
  loyalty <- Fields.defaulted "loyalty" Nothing (Common.maybe Loyalty.codec) Face.loyalty
  defense <- Fields.defaulted "defense" Nothing (Common.maybe Defense.codec) Face.defense
  characteristicPT <- Fields.defaulted "characteristicPT" Nothing (Common.maybe Quantity.codec) Face.characteristicPT
  enchant <- Fields.defaulted "enchant" [] (Common.list TargetSlot.codec) Face.enchant
  keywords <- Fields.defaulted "keywords" Set.empty (Common.set Keyword.codec) Face.keywords
  colorIndicator <- Fields.defaulted "colorIndicator" Set.empty (Common.set Color.codec) Face.colorIndicator
  spell <- Fields.defaulted "spell" Face.defaultSpell (Modal.codec cardCodec) Face.spell
  staticAbilities <- Fields.defaulted "staticAbilities" [] (Common.list (StaticAbility.codec cardCodec)) Face.staticAbilities
  activatedAbilities <- Fields.defaulted "activatedAbilities" [] (Common.list (ActivatedAbility.codec cardCodec)) Face.activatedAbilities
  replacementEffects <- Fields.defaulted "replacementEffects" [] (Common.list (PrintedReplacement.codec (Effect.codec cardCodec))) Face.replacementEffects
  triggeredAbilities <- Fields.defaulted "triggeredAbilities" [] (Common.list (TriggeredAbility.codec cardCodec)) Face.triggeredAbilities
  delayedAbilities <- Fields.defaulted "delayedAbilities" Map.empty (TriggeredAbility.codecDelayed cardCodec) Face.delayedAbilities
  -- CR 309.4: the rooms of a dungeon card, topmost first.
  rooms <- Fields.defaulted "rooms" Seq.empty (Common.seq (DungeonRoom.codec cardCodec)) Face.rooms
  castingPermissions <- Fields.defaulted "castingPermissions" [] (Common.list CastingPermission.codec) Face.castingPermissions
  castingRestrictions <- Fields.defaulted "castingRestrictions" [] (Common.list CastingRestriction.codec) Face.castingRestrictions
  playerAbilities <- Fields.defaulted "playerAbilities" [] (Common.list PlayerStaticAbility.codec) Face.playerAbilities
  blockRequirements <- Fields.defaulted "blockRequirements" [] (Common.list BlockRequirement.codec) Face.blockRequirements
  blockPermissions <- Fields.defaulted "blockPermissions" [] (Common.list BlockPermission.codec) Face.blockPermissions
  attackRequirements <- Fields.defaulted "attackRequirements" [] (Common.list AttackRequirement.codec) Face.attackRequirements
  combatRestrictions <- Fields.defaulted "combatRestrictions" [] (Common.list CombatRestriction.codec) Face.combatRestrictions
  sacrificeRestrictions <- Fields.defaulted "sacrificeRestrictions" [] (Common.list SacrificeRestriction.codec) Face.sacrificeRestrictions
  untapRestrictions <- Fields.defaulted "untapRestrictions" [] (Common.list UntapRestriction.codec) Face.untapRestrictions
  attackCosts <- Fields.defaulted "attackCosts" [] (Common.list AttackCost.codec) Face.attackCosts
  blockCosts <- Fields.defaulted "blockCosts" [] (Common.list BlockCost.codec) Face.blockCosts
  additionalCosts <- Fields.defaulted "additionalCosts" [] (Common.list (CostComponent.codec Keyword.codec)) Face.additionalCosts
  -- CR 101.1: the ceiling this face's own words put on CR 601.2b's announced X
  -- (Pawl.Types.Face).
  maximumX <- Fields.defaulted "maximumX" Nothing (Common.maybe Quantity.codec) Face.maximumX
  alternativeCosts <- Fields.defaulted "alternativeCosts" [] (Common.list AlternativeCost.codec) Face.alternativeCosts
  -- CR 601.2f: the reductions this face applies to its own cost to cast
  -- (Pawl.Types.CostReduction).
  costReductions <- Fields.defaulted "costReductions" [] (Common.list CostReduction.codec) Face.costReductions
  -- CR 103.5b / CR 103.6: an array of ACTIONS, each an array of effects, so a
  -- face granting two of them is writable (Pawl.Types.Face).
  mulliganActions <- Fields.defaulted "mulliganActions" [] (Common.list (HandAction.codec cardCodec)) Face.mulliganActions
  openingHandActions <- Fields.defaulted "openingHandActions" [] (Common.list (HandAction.codec cardCodec)) Face.openingHandActions
  -- CR 116.2: the special actions this face grants (Pawl.Types.Face).
  specialActions <- Fields.defaulted "specialActions" [] (Common.list SpecialAction.codec) Face.specialActions
  -- CR 113.6g: Counterable is the absence of a card stating it can't be
  -- countered.
  counterability <- Fields.defaulted "counterability" Counterability.Type.Counterable Counterability.codec Face.counterability
  pure
    Face.MkFace
      { Face.name = name,
        Face.typeLine = typeLine,
        Face.manaCost = manaCost,
        Face.power = power,
        Face.toughness = toughness,
        Face.loyalty = loyalty,
        Face.defense = defense,
        Face.characteristicPT = characteristicPT,
        Face.enchant = enchant,
        Face.keywords = keywords,
        Face.colorIndicator = colorIndicator,
        Face.spell = spell,
        Face.staticAbilities = staticAbilities,
        Face.activatedAbilities = activatedAbilities,
        Face.replacementEffects = replacementEffects,
        Face.triggeredAbilities = triggeredAbilities,
        Face.delayedAbilities = delayedAbilities,
        Face.rooms = rooms,
        Face.castingPermissions = castingPermissions,
        Face.castingRestrictions = castingRestrictions,
        Face.playerAbilities = playerAbilities,
        Face.blockRequirements = blockRequirements,
        Face.blockPermissions = blockPermissions,
        Face.attackRequirements = attackRequirements,
        Face.combatRestrictions = combatRestrictions,
        Face.sacrificeRestrictions = sacrificeRestrictions,
        Face.untapRestrictions = untapRestrictions,
        Face.attackCosts = attackCosts,
        Face.blockCosts = blockCosts,
        Face.additionalCosts = additionalCosts,
        Face.maximumX = maximumX,
        Face.alternativeCosts = alternativeCosts,
        Face.costReductions = costReductions,
        Face.mulliganActions = mulliganActions,
        Face.openingHandActions = openingHandActions,
        Face.specialActions = specialActions,
        Face.counterability = counterability
      }
