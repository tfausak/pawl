module Pawl.Codec.CardSpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationTiming as ActivationTiming
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
import qualified Pawl.Types.TargetSpec as TargetSpec
import qualified Pawl.Types.Toughness as Toughness
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TypeLine as TypeLine

-- Fixtures --------------------------------------------------------------------
--
-- No registry here: Pawl.Codec.CardSpec sits in the codec sublibrary, which is
-- ABOVE the test suite in CLAUDE.md's dependency table, so it cannot reach
-- Pawl.Registry or a real Printing. Every fixture below is a synthetic Card
-- built by hand -- CR 700.2's non-modal shape, a single empty Mode with
-- ChooseExactly 1, is what a land or vanilla creature's `spell` field is.

-- | A land or vanilla creature's spell payload: one mode, no effects, forced.
minimalModal :: Modal.Modal Card.Card
minimalModal =
  Modal.MkModal
    (Seq.singleton (Mode.MkMode Seq.empty Map.empty Optionality.Mandatory))
    (ModeSelection.ChooseExactly 1)

-- | CR 603.6a's simplest trigger, reused for both triggeredAbilities and
-- delayedAbilities below -- the shape does not matter to this module, only that
-- it is a well-typed TriggeredAbility Card.
minimalTriggeredAbility :: TriggeredAbility.TriggeredAbility Card.Card
minimalTriggeredAbility =
  TriggeredAbility.MkTriggeredAbility TriggerCondition.SelfEnters minimalModal Nothing

-- | Every required field set to a simple value, and every defaulted/elided
-- field at its Haskell default (Nothing, empty, or Counterable). Its JSON has
-- none of the sixteen optional keys -- 'baseCardJson' below -- which is what
-- lets the single round-trip assertion in the first test prove BOTH halves of
-- every one of those fields' elision at once: 'Card.toJson' would emit an
-- extra key if any field's encoder mis-omitted its default, and
-- 'Card.fromJson' would land on the wrong value if any field's decoder
-- mis-defaulted an absent key, and either failure would break the equality
-- check.
baseCard :: Card.Card
baseCard =
  Card.MkCard
    { Card.name = CardName.MkCardName $ Text.pack "Test Card",
      Card.manaCost = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
      Card.typeLine = TypeLine.MkTypeLine Set.empty (Set.singleton CardType.Creature) Set.empty,
      Card.power = Just (Power.MkPower (Quantity.Literal 1)),
      Card.toughness = Just (Toughness.MkToughness (Quantity.Literal 1)),
      Card.loyalty = Nothing,
      Card.keywords = Set.empty,
      Card.staticAbilities = [],
      Card.spell = minimalModal,
      Card.activatedAbilities = [],
      Card.replacementEffects = [],
      Card.triggeredAbilities = [],
      Card.castingPermissions = [],
      Card.castingRestrictions = [],
      Card.colorIndicator = Set.empty,
      Card.characteristicPT = Nothing,
      Card.delayedAbilities = Map.empty,
      Card.playerAbilities = [],
      Card.blockRequirements = [],
      Card.attackRequirements = [],
      Card.combatRestrictions = [],
      Card.attackCosts = [],
      Card.additionalCosts = [],
      Card.alternativeCosts = [],
      Card.mulliganAction = [],
      Card.openingHandAction = [],
      Card.enchant = Nothing,
      Card.counterability = Counterability.Counterable
    }

baseCardJson :: String
baseCardJson =
  "{\"name\":\"Test Card\",\"manaCost\":[{\"type\":\"Generic\",\"value\":1}],"
    <> "\"typeLine\":{\"types\":[{\"type\":\"Creature\"}]},"
    <> "\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":1},"
    <> "\"keywords\":[],\"staticAbilities\":[],"
    <> "\"spell\":{\"modes\":[{}]},"
    <> "\"activatedAbilities\":[],\"replacementEffects\":[],\"triggeredAbilities\":[],\"castingPermissions\":[]}"

-- | 'baseCard' with every field populated at once, including the sixteen
-- defaulted/elided ones and the recursive card-in-card fields (spell,
-- activatedAbilities, triggeredAbilities, delayedAbilities) -- the shape a
-- reviewer would otherwise have to piece together from sixteen separate single-
-- field cases. Its JSON, 'populatedCardJson', was produced by running
-- 'Card.toJson' on this exact value (not transcribed by hand) and pasted back in,
-- per the recipe's "derive every JSON literal by reading the encoder" rule.
populatedCard :: Card.Card
populatedCard =
  baseCard
    { Card.keywords = Set.singleton Keyword.Deathtouch,
      Card.staticAbilities = [StaticAbility.MkStaticAbility Affected.Attached (NonEmpty.singleton Modification.LoseAllAbilities)],
      Card.activatedAbilities = [ActivatedAbility.MkActivatedAbility (Cost.MkCost (Just (ManaCost.MkManaCost [])) []) minimalModal ActivationTiming.AnyTime],
      Card.replacementEffects = [ReplacementEffect.EntryR Filter.IsSource EntryRewrite.AsCopy],
      Card.triggeredAbilities = [minimalTriggeredAbility],
      Card.castingPermissions = [CastingPermission.CastFromLibraryWhileSearching],
      Card.loyalty = Just (Loyalty.MkLoyalty 3),
      Card.colorIndicator = Set.singleton Color.White,
      Card.characteristicPT = Just Quantity.ManaValue,
      Card.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "trigger")) minimalTriggeredAbility,
      Card.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.CantCastSpells],
      Card.blockRequirements = [BlockRequirement.MkBlockRequirement Affected.Attached],
      Card.attackRequirements = [AttackRequirement.MkAttackRequirement Affected.Attached],
      Card.combatRestrictions = [CombatRestriction.CantAttack Affected.Attached Nothing],
      Card.attackCosts = [AttackCost.MkAttackCost Affected.Attached (ManaCost.MkManaCost [ManaSymbol.Generic 2])],
      Card.additionalCosts = [CostComponent.TapThis],
      Card.alternativeCosts = [Cost.MkCost (Just (ManaCost.MkManaCost [])) []],
      Card.counterability = Counterability.CantBeCountered,
      Card.mulliganAction = [Effect.ExileHandThenDraw],
      Card.openingHandAction = [Effect.ExileHandThenDraw],
      Card.enchant = Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing),
      Card.castingRestrictions = [CastingRestriction.AttackedThisStep]
    }

populatedCardJson :: String
populatedCardJson =
  "{\"name\":\"Test Card\",\"manaCost\":[{\"type\":\"Generic\",\"value\":1}],"
    <> "\"typeLine\":{\"types\":[{\"type\":\"Creature\"}]},"
    <> "\"power\":{\"type\":\"Literal\",\"value\":1},\"toughness\":{\"type\":\"Literal\",\"value\":1},"
    <> "\"keywords\":[{\"type\":\"Deathtouch\"}],"
    <> "\"staticAbilities\":[{\"affected\":{\"type\":\"Attached\"},\"modifications\":[{\"type\":\"LoseAllAbilities\"}]}],"
    <> "\"spell\":{\"modes\":[{}]},"
    <> "\"activatedAbilities\":[{\"cost\":{\"mana\":[],\"components\":[]},"
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
    <> "\"alternativeCosts\":[{\"mana\":[],\"components\":[]}],"
    <> "\"counterability\":{\"type\":\"CantBeCountered\"},"
    <> "\"mulliganAction\":[{\"type\":\"ExileHandThenDraw\"}],"
    <> "\"openingHandAction\":[{\"type\":\"ExileHandThenDraw\"}],"
    <> "\"enchant\":{\"pool\":{\"type\":\"Creatures\"}},"
    <> "\"castingRestrictions\":[{\"type\":\"AttackedThisStep\"}]}"

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Card" $ do
  -- R7's one case for MkCard's single constructor, and simultaneously the
  -- absent-key proof for all sixteen defaulted/elided fields at once: see
  -- 'baseCard's haddock for why one round trip suffices for both.
  Spec.it s "MkCard, every required field present and every optional field absent" $
    Common.assertJsonCodec s Card.toJson Card.fromJson baseCard baseCardJson
  -- Each defaulted field gets its own explicit absent-key assertion too, not
  -- just the aggregate proof above: a decoder that defaulted the WRONG field
  -- (swapping two Maybe fields of the same underlying type, say) could still
  -- pass the aggregate 'baseCard' equality by accident if the two defaults
  -- happened to collide; reading each field back out individually rules that
  -- out.
  Spec.describe s "each defaulted field takes its default when its key is absent from the JSON" $ do
    Spec.it s "loyalty (CR 306.5) defaults to Nothing" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.loyalty <$> Card.fromJson v) (Right Nothing)
    Spec.it s "colorIndicator (CR 204.1/204.2) defaults to the empty set" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.colorIndicator <$> Card.fromJson v) (Right Set.empty)
    Spec.it s "characteristicPT (CR 604.3/208.2a) defaults to Nothing" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.characteristicPT <$> Card.fromJson v) (Right Nothing)
    Spec.it s "delayedAbilities (CR 603.7) defaults to the empty map" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.delayedAbilities <$> Card.fromJson v) (Right Map.empty)
    Spec.it s "playerAbilities (CR 604.1/604.2/611.1) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.playerAbilities <$> Card.fromJson v) (Right [])
    Spec.it s "blockRequirements (CR 509.1c) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.blockRequirements <$> Card.fromJson v) (Right [])
    Spec.it s "attackRequirements (CR 508.1d) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.attackRequirements <$> Card.fromJson v) (Right [])
    Spec.it s "combatRestrictions (CR 508.1c/509.1b) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.combatRestrictions <$> Card.fromJson v) (Right [])
    Spec.it s "attackCosts (CR 508.1c/508.1h) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.attackCosts <$> Card.fromJson v) (Right [])
    Spec.it s "additionalCosts (CR 118.8) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.additionalCosts <$> Card.fromJson v) (Right [])
    Spec.it s "alternativeCosts (CR 118.9) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.alternativeCosts <$> Card.fromJson v) (Right [])
    -- CR 113.6g's default, the stand-in for the registry-backed Rending
    -- Volley/Cancel pair kept in Pawl.CodecIntegrationSpec (that test needs
    -- real Printings, which this sublibrary cannot reach).
    Spec.it s "counterability (CR 113.6g) defaults to Counterable" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.counterability <$> Card.fromJson v) (Right Counterability.Counterable)
    Spec.it s "mulliganAction (CR 103.5b) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.mulliganAction <$> Card.fromJson v) (Right [])
    Spec.it s "openingHandAction (CR 103.6) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.openingHandAction <$> Card.fromJson v) (Right [])
    Spec.it s "enchant (CR 702.5a) defaults to Nothing" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.enchant <$> Card.fromJson v) (Right Nothing)
    Spec.it s "castingRestrictions (CR 601.3) defaults to the empty list" $ do
      v <- Common.assertJson s baseCardJson
      Spec.assertEq s (Card.castingRestrictions <$> Card.fromJson v) (Right [])
  -- The other half of every defaulted field's story: populated, it appears
  -- under its own key (proving the encoder's non-default arm) and round-trips
  -- (proving the decoder's present-key arm). Each case here is 'baseCard' with
  -- exactly one field changed, so its JSON is 'baseCardJson' plus exactly one
  -- extra key -- moved from the registry-backed "a Card carrying X round-trips"
  -- /"an empty X list is omitted" pairs formerly in Pawl.CodecSpec, which
  -- needed no registry fixture to make the same point.
  Spec.describe s "each defaulted field round-trips when present" $ do
    Spec.it s "loyalty" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.loyalty = Just (Loyalty.MkLoyalty 3)}
        (init baseCardJson <> ",\"loyalty\":3}")
    Spec.it s "colorIndicator" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.colorIndicator = Set.singleton Color.White}
        (init baseCardJson <> ",\"colorIndicator\":[{\"type\":\"White\"}]}")
    Spec.it s "characteristicPT" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.characteristicPT = Just Quantity.ManaValue}
        (init baseCardJson <> ",\"characteristicPT\":{\"type\":\"ManaValue\"}}")
    Spec.it s "delayedAbilities" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.delayedAbilities = Map.singleton (AbilityName.MkAbilityName (Text.pack "trigger")) minimalTriggeredAbility}
        ( init baseCardJson
            <> ",\"delayedAbilities\":[{\"name\":\"trigger\",\"ability\":{\"condition\":{\"type\":\"SelfEnters\"},"
            <> "\"modal\":{\"modes\":[{}]}}}]}"
        )
    Spec.it s "playerAbilities" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.playerAbilities = [PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.You PlayerEffect.CantCastSpells]}
        (init baseCardJson <> ",\"playerAbilities\":[{\"scope\":{\"type\":\"You\"},\"effect\":{\"type\":\"CantCastSpells\"}}]}")
    Spec.it s "blockRequirements" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.blockRequirements = [BlockRequirement.MkBlockRequirement Affected.Attached]}
        (init baseCardJson <> ",\"blockRequirements\":[{\"attacker\":{\"type\":\"Attached\"}}]}")
    Spec.it s "attackRequirements" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.attackRequirements = [AttackRequirement.MkAttackRequirement Affected.Attached]}
        (init baseCardJson <> ",\"attackRequirements\":[{\"subject\":{\"type\":\"Attached\"}}]}")
    Spec.it s "combatRestrictions" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.combatRestrictions = [CombatRestriction.CantAttack Affected.Attached Nothing]}
        (init baseCardJson <> ",\"combatRestrictions\":[{\"type\":\"CantAttack\",\"value\":{\"affected\":{\"type\":\"Attached\"}}}]}")
    Spec.it s "attackCosts" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.attackCosts = [AttackCost.MkAttackCost Affected.Attached (ManaCost.MkManaCost [ManaSymbol.Generic 2])]}
        (init baseCardJson <> ",\"attackCosts\":[{\"subject\":{\"type\":\"Attached\"},\"perAttacker\":[{\"type\":\"Generic\",\"value\":2}]}]}")
    Spec.it s "additionalCosts" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.additionalCosts = [CostComponent.TapThis]}
        (init baseCardJson <> ",\"additionalCosts\":[{\"type\":\"TapThis\"}]}")
    Spec.it s "alternativeCosts" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.alternativeCosts = [Cost.MkCost (Just (ManaCost.MkManaCost [])) []]}
        (init baseCardJson <> ",\"alternativeCosts\":[{\"mana\":[],\"components\":[]}]}")
    Spec.it s "counterability" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.counterability = Counterability.CantBeCountered}
        (init baseCardJson <> ",\"counterability\":{\"type\":\"CantBeCountered\"}}")
    Spec.it s "mulliganAction" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.mulliganAction = [Effect.ExileHandThenDraw]}
        (init baseCardJson <> ",\"mulliganAction\":[{\"type\":\"ExileHandThenDraw\"}]}")
    Spec.it s "openingHandAction" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.openingHandAction = [Effect.ExileHandThenDraw]}
        (init baseCardJson <> ",\"openingHandAction\":[{\"type\":\"ExileHandThenDraw\"}]}")
    Spec.it s "enchant" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.enchant = Just (TargetSpec.MkTargetSpec Pool.Creatures Nothing)}
        (init baseCardJson <> ",\"enchant\":{\"pool\":{\"type\":\"Creatures\"}}}")
    Spec.it s "castingRestrictions" $
      Common.assertJsonCodec
        s
        Card.toJson
        Card.fromJson
        baseCard {Card.castingRestrictions = [CastingRestriction.AttackedThisStep]}
        (init baseCardJson <> ",\"castingRestrictions\":[{\"type\":\"AttackedThisStep\"}]}")
  -- Every field at once, including the recursive card-in-card ones (spell,
  -- activatedAbilities, triggeredAbilities, delayedAbilities) that only Card
  -- itself ties the knot on -- 'populatedCard's haddock explains the fixture.
  Spec.it s "MkCard, every field populated at once" $
    Common.assertJsonCodec s Card.toJson Card.fromJson populatedCard populatedCardJson
