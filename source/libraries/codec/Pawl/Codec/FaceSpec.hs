{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.FaceSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Face as Face
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AttackCost as AttackCost
import qualified Pawl.Types.AttackRequirement as AttackRequirement
import qualified Pawl.Types.BlockRequirement as BlockRequirement
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastingPermission as CastingPermission
import qualified Pawl.Types.CastingRestriction as CastingRestriction
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CombatRestriction as CombatRestriction
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Counterability as Counterability
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Loyalty as Loyalty
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Pool as Pool
import qualified Pawl.Types.Power as Power
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.StaticAbility as StaticAbility
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine

-- Fixtures --------------------------------------------------------------------
--
-- No registry here: the codec sublibrary sits above the test suite in the
-- dependency table, so it cannot reach Pawl.Registry or a real Printing. Every
-- fixture below is a synthetic Face built by hand.

-- | The face codec at the card knot Pawl.Codec.Card ties, which is the only
-- instantiation of it that exists: the six card-shaped fields need a card codec
-- passed in, and Card's is the one.
encodeFace :: Face.Face Card.Card -> Value.Value
encodeFace = Face.toJson Card.toJson

decodeFace :: Value.Value -> Either Text.Text (Face.Face Card.Card)
decodeFace = Face.fromJson Card.fromJson

-- | CR 700.2's non-modal shape, which is what a land or vanilla creature's
-- spell payload is: one mode, no effects, forced.
minimalModal :: Modal.Modal Card.Card
minimalModal =
  Modal.MkModal
    (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory Nothing))
    (ModeSelection.ChooseExactly 1)

-- | CR 603.6a's simplest trigger. The shape does not matter here, only that it
-- is a well-typed TriggeredAbility Card.
minimalTriggeredAbility :: TriggeredAbility.TriggeredAbility Card.Card
minimalTriggeredAbility =
  TriggeredAbility.MkTriggeredAbility TriggerCondition.SelfEnters minimalModal Nothing

-- | Every required field set to a simple value, 'manaCost', 'power' and
-- 'toughness' set to non-default values, and every other defaulted field left at
-- its Haskell default. 'baseFaceJson' therefore carries no key for any of those
-- defaulted fields, which is what lets one round-trip assertion prove both halves
-- of every field's elision at once: an encoder that emitted a default, or a
-- decoder that mis-defaulted an absent key, breaks the equality.
baseFace :: Face.Face Card.Card
baseFace =
  Face.MkFace
    { Face.name = CardName.MkCardName $ Text.pack "Test Card",
      Face.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
      Face.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) Set.empty,
      Face.power = Just (Power.MkPower (Quantity.Literal 1)),
      Face.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.keywords = Set.empty,
      Face.staticAbilities = [],
      Face.spell = minimalModal,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.colorIndicator = Set.empty,
      Face.characteristicPT = Nothing,
      Face.delayedAbilities = Map.empty,
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.attackCosts = [],
      Face.additionalCosts = [],
      Face.alternativeCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable
    }

-- | Every field at the default an omitted key means, which is what the minimal
-- JSON above has to decode to.
minimalFace :: Face.Face Card.Card
minimalFace =
  Face.MkFace
    { Face.name = CardName.MkCardName (Text.pack "Mountain"),
      Face.manaCost = Nothing,
      Face.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Land) Set.empty,
      Face.power = Nothing,
      Face.toughness = Nothing,
      Face.loyalty = Nothing,
      Face.defense = Nothing,
      Face.keywords = Set.empty,
      Face.colorIndicator = Set.empty,
      Face.characteristicPT = Nothing,
      Face.staticAbilities = [],
      Face.spell = Face.defaultSpell,
      Face.activatedAbilities = [],
      Face.replacementEffects = [],
      Face.triggeredAbilities = [],
      Face.delayedAbilities = Map.empty,
      Face.castingPermissions = [],
      Face.castingRestrictions = [],
      Face.enchant = [],
      Face.counterability = Counterability.Counterable,
      Face.additionalCosts = [],
      Face.alternativeCosts = [],
      Face.playerAbilities = [],
      Face.blockRequirements = [],
      Face.attackRequirements = [],
      Face.combatRestrictions = [],
      Face.attackCosts = [],
      Face.mulliganActions = [],
      Face.openingHandActions = []
    }

-- The same face the verbose literal below spells out: minimalFace's fields,
-- with the type line a real basic land carries.
mountainFace :: Face.Face Card.Card
mountainFace =
  minimalFace
    { Face.typeLine =
        TypeLine.MkTypeLine
          (Set.singleton Supertype.Basic)
          (Set.singleton CardType.Land)
          (Set.singleton Subtype.Mountain)
    }

baseFaceJson :: String
baseFaceJson =
  "{\"name\":\"Test Card\",\"manaCost\":[{\"type\":\"Generic\",\"value\":1}],"
    <> "\"typeLine\":{\"types\":[{\"type\":\"Creature\"}]},"
    <> "\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":1}}"

-- | 'baseFace' with every field populated at once, except 'Face.spell', which
-- nothing here overrides -- so 'populatedFaceJson' has no @"spell"@ key. That
-- literal was produced by running 'encodeFace' on this exact value rather than
-- transcribed by hand.
populatedFace :: Face.Face Card.Card
populatedFace =
  baseFace
    { Face.keywords = Set.singleton Keyword.Deathtouch,
      Face.staticAbilities = [StaticAbility.MkStaticAbility Affected.Attached Nothing Nothing (NonEmpty.singleton Modification.LoseAllAbilities)],
      Face.activatedAbilities = [ActivatedAbility.MkActivatedAbility (Cost.MkCost (Just (ManaCost.MkManaCost [])) []) minimalModal [] Nothing],
      Face.replacementEffects = [ReplacementEffect.EntryR Filter.IsSource EntryRewrite.AsCopy],
      Face.triggeredAbilities = [minimalTriggeredAbility],
      Face.castingPermissions = [CastingPermission.CastFromLibraryWhileSearching],
      Face.loyalty = Just (Loyalty.MkLoyalty 3),
      Face.colorIndicator = Set.singleton Color.White,
      Face.characteristicPT = Just Quantity.ManaValue,
      Face.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "trigger")) minimalTriggeredAbility,
      Face.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.CantCastSpells],
      Face.blockRequirements = [BlockRequirement.MkBlockRequirement Affected.Attached],
      Face.attackRequirements = [AttackRequirement.MkAttackRequirement Affected.Attached],
      Face.combatRestrictions = [CombatRestriction.CantAttack Affected.Attached Nothing],
      Face.attackCosts = [AttackCost.MkAttackCost Affected.Attached (ManaCost.MkManaCost [ManaSymbol.Generic 2])],
      Face.additionalCosts = [CostComponent.TapThis],
      Face.alternativeCosts = [Cost.MkCost (Just (ManaCost.MkManaCost [])) []],
      Face.counterability = Counterability.CantBeCountered,
      Face.mulliganActions = [[Effect.ExileHandThenDraw]],
      Face.openingHandActions = [[Effect.ExileHandThenDraw]],
      Face.enchant = [TargetSpec.MkTargetSpec Pool.Creatures Nothing],
      Face.castingRestrictions = [CastingRestriction.AttackedThisStep]
    }

populatedFaceJson :: String
populatedFaceJson =
  "{\"name\":\"Test Card\",\"manaCost\":[{\"type\":\"Generic\",\"value\":1}],"
    <> "\"typeLine\":{\"types\":[{\"type\":\"Creature\"}]},"
    <> "\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":1},"
    <> "\"keywords\":[{\"type\":\"Deathtouch\"}],"
    <> "\"staticAbilities\":[{\"affected\":{\"type\":\"Attached\"},\"modifications\":[{\"type\":\"LoseAllAbilities\"}]}],"
    <> "\"activatedAbilities\":[{\"cost\":{\"mana\":[]},"
    <> "\"modal\":{\"modes\":[{}]}}],"
    <> "\"replacementEffects\":[{\"type\":\"EntryR\",\"value\":[{\"type\":\"IsSource\"},{\"type\":\"AsCopy\"}]}],"
    <> "\"triggeredAbilities\":[{\"condition\":{\"type\":\"SelfEnters\"},"
    <> "\"modal\":{\"modes\":[{}]}}],"
    <> "\"castingPermissions\":[{\"type\":\"CastFromLibraryWhileSearching\"}],"
    <> "\"loyalty\":3,\"colorIndicator\":[{\"type\":\"White\"}],\"characteristicPT\":{\"type\":\"ManaValue\"},"
    <> "\"delayedAbilities\":[{\"name\":\"trigger\",\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},"
    <> "\"modal\":{\"modes\":[{}]}}}],"
    <> "\"playerAbilities\":[{\"scope\":{\"type\":\"You\"},\"effect\":{\"type\":\"CantCastSpells\"}}],"
    <> "\"blockRequirements\":[{\"attacker\":{\"type\":\"Attached\"}}],"
    <> "\"attackRequirements\":[{\"subject\":{\"type\":\"Attached\"}}],"
    <> "\"combatRestrictions\":[{\"type\":\"CantAttack\",\"value\":{\"affected\":{\"type\":\"Attached\"}}}],"
    <> "\"attackCosts\":[{\"subject\":{\"type\":\"Attached\"},\"perAttacker\":[{\"type\":\"Generic\",\"value\":2}]}],"
    <> "\"additionalCosts\":[{\"type\":\"TapThis\"}],"
    <> "\"alternativeCosts\":[{\"mana\":[]}],"
    <> "\"counterability\":{\"type\":\"CantBeCountered\"},"
    <> "\"mulliganActions\":[[{\"type\":\"ExileHandThenDraw\"}]],"
    <> "\"openingHandActions\":[[{\"type\":\"ExileHandThenDraw\"}]],"
    <> "\"enchant\":[{\"pool\":{\"type\":\"Creatures\"}}],"
    <> "\"castingRestrictions\":[{\"type\":\"AttackedThisStep\"}]}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Face" $ do
  -- name and typeLine are the only required keys: a face that says nothing else
  -- is a vanilla card's face rather than a malformed file.
  Spec.it s "a minimal face carries only name and typeLine" $
    Common.assertJsonCodec
      s
      encodeFace
      decodeFace
      minimalFace
      """ {"name":"Mountain","typeLine":{"types":[{"type":"Land"}]}} """
  Spec.it s "MkFace, every required field present and every optional field absent" $
    Common.assertJsonCodec s encodeFace decodeFace baseFace baseFaceJson
  -- Most defaulted fields also get their own absent-key assertion below, not
  -- just the aggregate round trip above: a decoder that defaulted the WRONG
  -- field could still pass the aggregate equality if two defaults happened to
  -- collide, and reading each field back out individually rules that out.
  Spec.describe s "each defaulted field takes its default when its key is absent from the JSON" $ do
    Spec.it s "loyalty (CR 306.5) defaults to Nothing" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.loyalty <$> decodeFace v) (Right Nothing)
    Spec.it s "colorIndicator (CR 204.1/204.2) defaults to the empty set" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.colorIndicator <$> decodeFace v) (Right Set.empty)
    Spec.it s "characteristicPT (CR 604.3/208.2a) defaults to Nothing" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.characteristicPT <$> decodeFace v) (Right Nothing)
    Spec.it s "delayedAbilities (CR 603.7) defaults to the empty map" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.delayedAbilities <$> decodeFace v) (Right Map.empty)
    Spec.it s "playerAbilities (CR 604.1/604.2/611.1) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.playerAbilities <$> decodeFace v) (Right [])
    Spec.it s "blockRequirements (CR 509.1c) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.blockRequirements <$> decodeFace v) (Right [])
    Spec.it s "attackRequirements (CR 508.1d) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.attackRequirements <$> decodeFace v) (Right [])
    Spec.it s "combatRestrictions (CR 508.1c/509.1b) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.combatRestrictions <$> decodeFace v) (Right [])
    Spec.it s "attackCosts (CR 508.1c/508.1h) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.attackCosts <$> decodeFace v) (Right [])
    Spec.it s "additionalCosts (CR 118.8) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.additionalCosts <$> decodeFace v) (Right [])
    Spec.it s "alternativeCosts (CR 118.9) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.alternativeCosts <$> decodeFace v) (Right [])
    -- The registry-backed pair lives in Pawl.CodecIntegrationSpec, which can
    -- reach real Printings; this sublibrary cannot.
    Spec.it s "counterability (CR 113.6g) defaults to Counterable" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.counterability <$> decodeFace v) (Right Counterability.Counterable)
    Spec.it s "mulliganActions (CR 103.5b) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.mulliganActions <$> decodeFace v) (Right [])
    Spec.it s "openingHandActions (CR 103.6) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.openingHandActions <$> decodeFace v) (Right [])
    Spec.it s "enchant (CR 702.5a) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.enchant <$> decodeFace v) (Right [])
    Spec.it s "castingRestrictions (CR 601.3) defaults to the empty list" $ do
      v <- Common.assertJson s baseFaceJson
      Spec.assertEq s (Face.castingRestrictions <$> decodeFace v) (Right [])
  -- The other half of every defaulted field's story: populated, it appears
  -- under its own key and round-trips. Each case is 'baseFace' with exactly one
  -- field changed, so its JSON is 'baseFaceJson' plus exactly one extra key.
  Spec.describe s "each defaulted field round-trips when present" $ do
    Spec.it s "loyalty" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.loyalty = Just (Loyalty.MkLoyalty 3)}
        (init baseFaceJson <> ",\"loyalty\":3}")
    Spec.it s "colorIndicator" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.colorIndicator = Set.singleton Color.White}
        (init baseFaceJson <> ",\"colorIndicator\":[{\"type\":\"White\"}]}")
    Spec.it s "characteristicPT" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.characteristicPT = Just Quantity.ManaValue}
        (init baseFaceJson <> ",\"characteristicPT\":{\"type\":\"ManaValue\"}}")
    Spec.it s "delayedAbilities" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "trigger")) minimalTriggeredAbility}
        ( init baseFaceJson
            <> ",\"delayedAbilities\":[{\"name\":\"trigger\",\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},"
            <> "\"modal\":{\"modes\":[{}]}}}]}"
        )
    Spec.it s "playerAbilities" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.CantCastSpells]}
        (init baseFaceJson <> ",\"playerAbilities\":[{\"scope\":{\"type\":\"You\"},\"effect\":{\"type\":\"CantCastSpells\"}}]}")
    Spec.it s "blockRequirements" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.blockRequirements = [BlockRequirement.MkBlockRequirement Affected.Attached]}
        (init baseFaceJson <> ",\"blockRequirements\":[{\"attacker\":{\"type\":\"Attached\"}}]}")
    Spec.it s "attackRequirements" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.attackRequirements = [AttackRequirement.MkAttackRequirement Affected.Attached]}
        (init baseFaceJson <> ",\"attackRequirements\":[{\"subject\":{\"type\":\"Attached\"}}]}")
    Spec.it s "combatRestrictions" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.combatRestrictions = [CombatRestriction.CantAttack Affected.Attached Nothing]}
        (init baseFaceJson <> ",\"combatRestrictions\":[{\"type\":\"CantAttack\",\"value\":{\"affected\":{\"type\":\"Attached\"}}}]}")
    Spec.it s "attackCosts" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.attackCosts = [AttackCost.MkAttackCost Affected.Attached (ManaCost.MkManaCost [ManaSymbol.Generic 2])]}
        (init baseFaceJson <> ",\"attackCosts\":[{\"subject\":{\"type\":\"Attached\"},\"perAttacker\":[{\"type\":\"Generic\",\"value\":2}]}]}")
    Spec.it s "additionalCosts" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.additionalCosts = [CostComponent.TapThis]}
        (init baseFaceJson <> ",\"additionalCosts\":[{\"type\":\"TapThis\"}]}")
    Spec.it s "alternativeCosts" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.alternativeCosts = [Cost.MkCost (Just (ManaCost.MkManaCost [])) []]}
        (init baseFaceJson <> ",\"alternativeCosts\":[{\"mana\":[]}]}")
    Spec.it s "counterability" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.counterability = Counterability.CantBeCountered}
        (init baseFaceJson <> ",\"counterability\":{\"type\":\"CantBeCountered\"}}")
    Spec.it s "mulliganActions" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.mulliganActions = [[Effect.ExileHandThenDraw]]}
        (init baseFaceJson <> ",\"mulliganActions\":[[{\"type\":\"ExileHandThenDraw\"}]]}")
    -- CR 103.5b caps nothing, so two actions must survive the round trip AS TWO.
    -- The arms differ in length on purpose: a codec that flattened them would
    -- read back one action of three effects, which is a different face.
    Spec.it s "mulliganActions keeps two actions apart" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.mulliganActions = [[Effect.ExileHandThenDraw], [Effect.ExileHandThenDraw, Effect.ExileHandThenDraw]]}
        ( init baseFaceJson
            <> ",\"mulliganActions\":[[{\"type\":\"ExileHandThenDraw\"}],"
            <> "[{\"type\":\"ExileHandThenDraw\"},{\"type\":\"ExileHandThenDraw\"}]]}"
        )
    Spec.it s "openingHandActions" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.openingHandActions = [[Effect.ExileHandThenDraw]]}
        (init baseFaceJson <> ",\"openingHandActions\":[[{\"type\":\"ExileHandThenDraw\"}]]}")
    Spec.it s "enchant" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.enchant = [TargetSpec.MkTargetSpec Pool.Creatures Nothing]}
        (init baseFaceJson <> ",\"enchant\":[{\"pool\":{\"type\":\"Creatures\"}}]}")
    Spec.it s "castingRestrictions" $
      Common.assertJsonCodec
        s
        encodeFace
        decodeFace
        baseFace {Face.castingRestrictions = [CastingRestriction.AttackedThisStep]}
        (init baseFaceJson <> ",\"castingRestrictions\":[{\"type\":\"AttackedThisStep\"}]}")
  -- Every field at once, including the recursive card-in-card ones that only
  -- Card itself ties the knot on.
  Spec.it s "MkFace, every field populated at once" $
    Common.assertJsonCodec s encodeFace decodeFace populatedFace populatedFaceJson
  -- Omission is permitted on input, never required: a file that spells out
  -- every default must still load.
  Spec.it s "a pre-migration card file still decodes" $
    Common.assertFromJson
      s
      decodeFace
      """ {"name":"Mountain","typeLine":{"supertypes":[{"type":"Basic"}],"types":[{"type":"Land"}],"subtypes":[{"type":"Mountain"}]},"manaCost":null,"power":null,"toughness":null,"keywords":[],"staticAbilities":[],"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}} """
      mountainFace
