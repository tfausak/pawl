import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Pawl.ActivateSpec
import qualified Pawl.AuraSpec
import qualified Pawl.BindingSpec
import qualified Pawl.CardSpec
import qualified Pawl.CardsSpec
import qualified Pawl.CastSpec
import qualified Pawl.Codec.AbilityNameSpec
import qualified Pawl.Codec.ActivatedAbilitySpec
import qualified Pawl.Codec.ActivationTimingSpec
import qualified Pawl.Codec.AffectedSpec
import qualified Pawl.Codec.AggregationSpec
import qualified Pawl.Codec.AttackCostSpec
import qualified Pawl.Codec.AttackRequirementSpec
import qualified Pawl.Codec.BeginningStepSpec
import qualified Pawl.Codec.BindingSpec
import qualified Pawl.Codec.BlockRequirementSpec
import qualified Pawl.Codec.CardSpec
import qualified Pawl.Codec.CardTypeSpec
import qualified Pawl.Codec.CastingPermissionSpec
import qualified Pawl.Codec.CastingRestrictionSpec
import qualified Pawl.Codec.ColorSpec
import qualified Pawl.Codec.CombatRestrictionSpec
import qualified Pawl.Codec.CombatStepSpec
import qualified Pawl.Codec.CommonSpec
import qualified Pawl.Codec.ComparisonSpec
import qualified Pawl.Codec.ConditionSpec
import qualified Pawl.Codec.ControllerRelationSpec
import qualified Pawl.Codec.CostComponentSpec
import qualified Pawl.Codec.CostSpec
import qualified Pawl.Codec.CountSpec
import qualified Pawl.Codec.CounterKindSpec
import qualified Pawl.Codec.CounterPatternSpec
import qualified Pawl.Codec.CounterabilitySpec
import qualified Pawl.Codec.CounteringSpec
import qualified Pawl.Codec.DamageEventSpec
import qualified Pawl.Codec.DamageKindSpec
import qualified Pawl.Codec.DamagePatternSpec
import qualified Pawl.Codec.DamageRewriteSpec
import qualified Pawl.Codec.DelayedTriggerSpec
import qualified Pawl.Codec.DestructionRewriteSpec
import qualified Pawl.Codec.DiscardCauseSpec
import qualified Pawl.Codec.DurationSpec
import qualified Pawl.Codec.EffectSpec
import qualified Pawl.Codec.EndingStepSpec
import qualified Pawl.Codec.EntryOptionSpec
import qualified Pawl.Codec.EntryRewriteSpec
import qualified Pawl.Codec.EntryRidersSpec
import qualified Pawl.Codec.EventShapeSpec
import qualified Pawl.Codec.ExpirySpec
import qualified Pawl.Codec.ExtraPhaseSpec
import qualified Pawl.Codec.FilterSpec
import qualified Pawl.Codec.GameEventSpec
import qualified Pawl.Codec.KeywordSpec
import qualified Pawl.Codec.LoyaltySpec
import qualified Pawl.Codec.ManaCostSpec
import qualified Pawl.Codec.ManaCountSpec
import qualified Pawl.Codec.ManaFilterSpec
import qualified Pawl.Codec.ManaProductionSpec
import qualified Pawl.Codec.ManaSymbolSpec
import qualified Pawl.Codec.ManaTypeSpec
import qualified Pawl.Codec.ModalSpec
import qualified Pawl.Codec.ModeIndexSpec
import qualified Pawl.Codec.ModeSelectionSpec
import qualified Pawl.Codec.ModeSpec
import qualified Pawl.Codec.ModificationSpec
import qualified Pawl.Codec.MonarchTargetSpec
import qualified Pawl.Codec.ObjectIdSpec
import qualified Pawl.Codec.ObjectRefSpec
import qualified Pawl.Codec.OnsetSpec
import qualified Pawl.Codec.OptionalitySpec
import qualified Pawl.Codec.PhasePatternSpec
import qualified Pawl.Codec.PhaseSelectorSpec
import qualified Pawl.Codec.PhaseSpec
import qualified Pawl.Codec.PlayerCounterKindSpec
import qualified Pawl.Codec.PlayerEffectSpec
import qualified Pawl.Codec.PlayerIdSpec
import qualified Pawl.Codec.PlayerRefSpec
import qualified Pawl.Codec.PlayerRelationSpec
import qualified Pawl.Codec.PlayerScopeSpec
import qualified Pawl.Codec.PlayerStaticAbilitySpec
import qualified Pawl.Codec.PoolSpec
import qualified Pawl.Codec.PowerSpec
import qualified Pawl.Codec.PrintingSpec
import qualified Pawl.Codec.ProjectedCharacteristicsSpec
import qualified Pawl.Codec.QuantitySpec
import qualified Pawl.Codec.RecipientSpec
import qualified Pawl.Codec.RegenerabilitySpec
import qualified Pawl.Codec.ReplacementEffectSpec
import qualified Pawl.Codec.ReplacementOriginSpec
import qualified Pawl.Codec.ScalingSpec
import qualified Pawl.Codec.ScopeSpec
import qualified Pawl.Codec.SearchDestinationSpec
import qualified Pawl.Codec.SlotNameSpec
import qualified Pawl.Codec.SourceRelationSpec
import qualified Pawl.Codec.StaticAbilitySpec
import qualified Pawl.Codec.SubtypeFamilySpec
import qualified Pawl.Codec.SubtypeSpec
import qualified Pawl.Codec.SupertypeSpec
import qualified Pawl.Codec.TapStateSpec
import qualified Pawl.Codec.TargetSpecSpec
import qualified Pawl.Codec.TokenPatternSpec
import qualified Pawl.Codec.ToughnessSpec
import qualified Pawl.Codec.TriggerConditionSpec
import qualified Pawl.Codec.TriggerFrequencySpec
import qualified Pawl.Codec.TriggeredAbilitySpec
import qualified Pawl.Codec.TurnScopeSpec
import qualified Pawl.Codec.TurnWindowSpec
import qualified Pawl.Codec.TypeLineSpec
import qualified Pawl.Codec.UsesSpec
import qualified Pawl.Codec.ZoneChangePatternSpec
import qualified Pawl.Codec.ZoneChangeSpec
import qualified Pawl.Codec.ZoneChangeSubjectSpec
import qualified Pawl.Codec.ZoneSpec
import qualified Pawl.CodecIntegrationSpec
import qualified Pawl.ColorSpec
import qualified Pawl.CombatSpec
import qualified Pawl.ConditionSpec
import qualified Pawl.CopySpec
import qualified Pawl.CoreSpec
import qualified Pawl.CorpusSpec
import qualified Pawl.CostSpec
import qualified Pawl.CountSpec
import qualified Pawl.DamageSpec
import qualified Pawl.DecideSpec
import qualified Pawl.DecimalSpec
import qualified Pawl.DepartureSpec
import qualified Pawl.EngineSpec
import qualified Pawl.EventSpec
import qualified Pawl.ExpirySpec
import qualified Pawl.Extra.BuilderSpec
import qualified Pawl.Extra.EitherSpec
import qualified Pawl.Extra.IntSpec
import qualified Pawl.Extra.IntegerSpec
import qualified Pawl.Extra.MonoidSpec
import qualified Pawl.Extra.NaturalSpec
import qualified Pawl.Extra.OrdSpec
import qualified Pawl.Extra.ParsecSpec
import qualified Pawl.Extra.SemigroupSpec
import qualified Pawl.FilterSpec
import qualified Pawl.GameSpec
import qualified Pawl.Json.ArraySpec
import qualified Pawl.Json.BooleanSpec
import qualified Pawl.Json.NullSpec
import qualified Pawl.Json.NumberSpec
import qualified Pawl.Json.ObjectSpec
import qualified Pawl.Json.PairSpec
import qualified Pawl.Json.StringSpec
import qualified Pawl.Json.ValueSpec
import qualified Pawl.ManaSpec
import qualified Pawl.ModalSpec
import qualified Pawl.MulliganSpec
import qualified Pawl.PlaneswalkerSpec
import qualified Pawl.PlayerEffectSpec
import qualified Pawl.PowerToughnessSpec
import qualified Pawl.ProjectionSpec
import qualified Pawl.Registry as Registry
import qualified Pawl.RegistrySpec
import qualified Pawl.ReplacementSpec
import qualified Pawl.ReplaySpec
import qualified Pawl.ResolveSpec
import qualified Pawl.SetupSpec
import qualified Pawl.SlugSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.TargetSpec
import qualified Pawl.TriggerSpec
import qualified Pawl.TurnSpec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

main :: IO ()
main = do
  root <- Registry.defaultRoot
  registry <- Registry.fileRegistry root
  Tasty.defaultMain (testTree registry)

tasty :: Spec.Spec IO (Writer.Writer [Tasty.TestTree])
tasty =
  Spec.MkSpec
    { Spec.assertFailure = HU.assertFailure,
      Spec.describe = \s -> Writer.tell . List.singleton . Tasty.testGroup s . Writer.execWriter,
      Spec.it = \s -> Writer.tell . List.singleton . HU.testCase s
    }

testTree :: Registry.Registry IO -> Tasty.TestTree
testTree registry =
  Tasty.testGroup
    "pawl"
    ( [Tasty.testGroup "spec" . Writer.execWriter $ spec tasty registry]
        -- Pawl.ReplacementSpec is wired separately because its timeout is a
        -- tasty option and Pawl.Spec cannot express one. The rationale for the
        -- timeout is at that module's `spec`; it guards CR 616.1's termination,
        -- where a regression hangs rather than fails.
        <> fmap
          (Tasty.localOption (Tasty.mkTimeout 5000000))
          (Writer.execWriter (Pawl.ReplacementSpec.spec tasty registry))
        -- Pawl.EngineSpec is wired separately for the same reason, and guards
        -- the same failure mode one level up: it asserts that a whole game
        -- terminates, which without a timeout hangs rather than fails. Its
        -- budget is the looser one because each of its cases plays out one or
        -- two complete games (the longest runs 161 turns) rather than resolving
        -- a single event.
        <> fmap
          (Tasty.localOption (Tasty.mkTimeout 30000000))
          (Writer.execWriter (Pawl.EngineSpec.spec tasty registry))
    )

-- Specs that reach for card data take a registry over the same monad they
-- assert in. The rest need none at all.
spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = do
  Pawl.ActivateSpec.spec s registry
  Pawl.AuraSpec.spec s registry
  Pawl.BindingSpec.spec s
  Pawl.CardSpec.spec s registry
  Pawl.CardsSpec.spec s
  Pawl.CastSpec.spec s registry
  Pawl.Codec.AbilityNameSpec.spec s
  Pawl.Codec.ActivatedAbilitySpec.spec s
  Pawl.Codec.ActivationTimingSpec.spec s
  Pawl.Codec.AffectedSpec.spec s
  Pawl.Codec.AggregationSpec.spec s
  Pawl.Codec.AttackCostSpec.spec s
  Pawl.Codec.AttackRequirementSpec.spec s
  Pawl.Codec.BeginningStepSpec.spec s
  Pawl.Codec.BindingSpec.spec s
  Pawl.Codec.BlockRequirementSpec.spec s
  Pawl.Codec.CardSpec.spec s
  Pawl.Codec.CardTypeSpec.spec s
  Pawl.Codec.CastingPermissionSpec.spec s
  Pawl.Codec.CastingRestrictionSpec.spec s
  Pawl.Codec.ColorSpec.spec s
  Pawl.Codec.CombatRestrictionSpec.spec s
  Pawl.Codec.CombatStepSpec.spec s
  Pawl.Codec.CommonSpec.spec s
  Pawl.Codec.ComparisonSpec.spec s
  Pawl.Codec.ConditionSpec.spec s
  Pawl.Codec.ControllerRelationSpec.spec s
  Pawl.Codec.CostComponentSpec.spec s
  Pawl.Codec.CostSpec.spec s
  Pawl.Codec.CounterKindSpec.spec s
  Pawl.Codec.CounterPatternSpec.spec s
  Pawl.Codec.CounterabilitySpec.spec s
  Pawl.Codec.CounteringSpec.spec s
  Pawl.Codec.CountSpec.spec s
  Pawl.Codec.DamageEventSpec.spec s
  Pawl.Codec.DamageKindSpec.spec s
  Pawl.Codec.DamagePatternSpec.spec s
  Pawl.Codec.DamageRewriteSpec.spec s
  Pawl.Codec.DelayedTriggerSpec.spec s
  Pawl.Codec.DestructionRewriteSpec.spec s
  Pawl.Codec.DiscardCauseSpec.spec s
  Pawl.Codec.DurationSpec.spec s
  Pawl.Codec.EffectSpec.spec s
  Pawl.Codec.EndingStepSpec.spec s
  Pawl.Codec.EntryOptionSpec.spec s
  Pawl.Codec.EntryRewriteSpec.spec s
  Pawl.Codec.EntryRidersSpec.spec s
  Pawl.Codec.EventShapeSpec.spec s
  Pawl.Codec.ExpirySpec.spec s
  Pawl.Codec.ExtraPhaseSpec.spec s
  Pawl.Codec.FilterSpec.spec s
  Pawl.Codec.GameEventSpec.spec s
  Pawl.Codec.KeywordSpec.spec s
  Pawl.Codec.LoyaltySpec.spec s
  Pawl.Codec.ManaCostSpec.spec s
  Pawl.Codec.ManaCountSpec.spec s
  Pawl.Codec.ManaFilterSpec.spec s
  Pawl.Codec.ManaProductionSpec.spec s
  Pawl.Codec.ManaSymbolSpec.spec s
  Pawl.Codec.ManaTypeSpec.spec s
  Pawl.Codec.ModalSpec.spec s
  Pawl.Codec.ModeIndexSpec.spec s
  Pawl.Codec.ModeSelectionSpec.spec s
  Pawl.Codec.ModeSpec.spec s
  Pawl.Codec.ModificationSpec.spec s
  Pawl.Codec.MonarchTargetSpec.spec s
  Pawl.Codec.ObjectIdSpec.spec s
  Pawl.Codec.ObjectRefSpec.spec s
  Pawl.Codec.OnsetSpec.spec s
  Pawl.Codec.OptionalitySpec.spec s
  Pawl.Codec.PhasePatternSpec.spec s
  Pawl.Codec.PhaseSelectorSpec.spec s
  Pawl.Codec.PhaseSpec.spec s
  Pawl.Codec.PlayerCounterKindSpec.spec s
  Pawl.Codec.PlayerEffectSpec.spec s
  Pawl.Codec.PlayerIdSpec.spec s
  Pawl.Codec.PlayerRefSpec.spec s
  Pawl.Codec.PlayerRelationSpec.spec s
  Pawl.Codec.PlayerScopeSpec.spec s
  Pawl.Codec.PlayerStaticAbilitySpec.spec s
  Pawl.Codec.PoolSpec.spec s
  Pawl.Codec.PowerSpec.spec s
  Pawl.Codec.PrintingSpec.spec s
  Pawl.Codec.ProjectedCharacteristicsSpec.spec s
  Pawl.Codec.QuantitySpec.spec s
  Pawl.Codec.RecipientSpec.spec s
  Pawl.Codec.RegenerabilitySpec.spec s
  Pawl.Codec.ReplacementEffectSpec.spec s
  Pawl.Codec.ReplacementOriginSpec.spec s
  Pawl.Codec.ScalingSpec.spec s
  Pawl.Codec.ScopeSpec.spec s
  Pawl.Codec.SearchDestinationSpec.spec s
  Pawl.Codec.SlotNameSpec.spec s
  Pawl.Codec.SourceRelationSpec.spec s
  Pawl.Codec.StaticAbilitySpec.spec s
  Pawl.Codec.SubtypeFamilySpec.spec s
  Pawl.Codec.SubtypeSpec.spec s
  Pawl.Codec.SupertypeSpec.spec s
  Pawl.Codec.TapStateSpec.spec s
  Pawl.Codec.TargetSpecSpec.spec s
  Pawl.Codec.TokenPatternSpec.spec s
  Pawl.Codec.ToughnessSpec.spec s
  Pawl.Codec.TriggerConditionSpec.spec s
  Pawl.Codec.TriggerFrequencySpec.spec s
  Pawl.Codec.TriggeredAbilitySpec.spec s
  Pawl.Codec.TurnScopeSpec.spec s
  Pawl.Codec.TurnWindowSpec.spec s
  Pawl.Codec.TypeLineSpec.spec s
  Pawl.Codec.UsesSpec.spec s
  Pawl.Codec.ZoneChangePatternSpec.spec s
  Pawl.Codec.ZoneChangeSpec.spec s
  Pawl.Codec.ZoneChangeSubjectSpec.spec s
  Pawl.Codec.ZoneSpec.spec s
  Pawl.CodecIntegrationSpec.spec s registry
  Pawl.ColorSpec.spec s registry
  Pawl.CombatSpec.spec s registry
  Pawl.ConditionSpec.spec s registry
  Pawl.CopySpec.spec s registry
  Pawl.CorpusSpec.spec s
  Pawl.CoreSpec.spec s registry
  Pawl.CostSpec.spec s registry
  Pawl.CountSpec.spec s registry
  Pawl.DamageSpec.spec s registry
  Pawl.DecideSpec.spec s
  Pawl.DecimalSpec.spec s
  Pawl.DepartureSpec.spec s registry
  Pawl.EventSpec.spec s registry
  Pawl.ExpirySpec.spec s registry
  Pawl.Extra.BuilderSpec.spec s
  Pawl.Extra.EitherSpec.spec s
  Pawl.Extra.IntSpec.spec s
  Pawl.Extra.IntegerSpec.spec s
  Pawl.Extra.MonoidSpec.spec s
  Pawl.Extra.NaturalSpec.spec s
  Pawl.Extra.OrdSpec.spec s
  Pawl.Extra.ParsecSpec.spec s
  Pawl.Extra.SemigroupSpec.spec s
  Pawl.FilterSpec.spec s
  Pawl.GameSpec.spec s registry
  Pawl.Json.ArraySpec.spec s
  Pawl.Json.BooleanSpec.spec s
  Pawl.Json.NullSpec.spec s
  Pawl.Json.NumberSpec.spec s
  Pawl.Json.ObjectSpec.spec s
  Pawl.Json.PairSpec.spec s
  Pawl.Json.StringSpec.spec s
  Pawl.Json.ValueSpec.spec s
  Pawl.ManaSpec.spec s registry
  Pawl.ModalSpec.spec s registry
  Pawl.MulliganSpec.spec s registry
  Pawl.PlaneswalkerSpec.spec s registry
  Pawl.PlayerEffectSpec.spec s registry
  Pawl.PowerToughnessSpec.spec s registry
  Pawl.ProjectionSpec.spec s registry
  Pawl.RegistrySpec.spec s
  Pawl.ReplaySpec.spec s registry
  Pawl.ResolveSpec.spec s registry
  Pawl.SetupSpec.spec s registry
  Pawl.SlugSpec.spec s
  Pawl.TargetSpec.spec s registry
  Pawl.TriggerSpec.spec s registry
  Pawl.TurnSpec.spec s registry
