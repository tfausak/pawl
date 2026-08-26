module Pawl.Test where

import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Pawl.ActivateSpec
import qualified Pawl.AdventureSpec
import qualified Pawl.AuraSpec
import qualified Pawl.BattleSpec
import qualified Pawl.BindingSpec
import qualified Pawl.CardSpec
import qualified Pawl.CardsSpec
import qualified Pawl.CaseSpec
import qualified Pawl.CastSpec
import qualified Pawl.ClassSpec
import qualified Pawl.Codec.AbilityNameSpec
import qualified Pawl.Codec.AbilityTriggeredSpec
import qualified Pawl.Codec.ActivatedAbilitySourceSpec
import qualified Pawl.Codec.ActivatedAbilitySpec
import qualified Pawl.Codec.ActivationRestrictionSpec
import qualified Pawl.Codec.ActiveAttackRequirementSpec
import qualified Pawl.Codec.ActiveBlockRequirementSpec
import qualified Pawl.Codec.ActivePlayerEffectSpec
import qualified Pawl.Codec.ActiveReplacementSpec
import qualified Pawl.Codec.ActiveUnregeneratableSpec
import qualified Pawl.Codec.AddActivationCostSpec
import qualified Pawl.Codec.AddSpellCostSpec
import qualified Pawl.Codec.AffectPlayersSpec
import qualified Pawl.Codec.AffectedPlayersSpec
import qualified Pawl.Codec.AffectedSpec
import qualified Pawl.Codec.AffectedUnlessSpec
import qualified Pawl.Codec.AfterTurnSpec
import qualified Pawl.Codec.AgainstSlotSpec
import qualified Pawl.Codec.AggregationSpec
import qualified Pawl.Codec.AlternativeCostSpec
import qualified Pawl.Codec.ArmDelayedTriggerSpec
import qualified Pawl.Codec.AsCopySpec
import qualified Pawl.Codec.AttachRestrictionSpec
import qualified Pawl.Codec.AttachTargetSpec
import qualified Pawl.Codec.AttackCostSpec
import qualified Pawl.Codec.AttackRequirementSpec
import qualified Pawl.Codec.AttackTargetSpec
import qualified Pawl.Codec.AttackerBlockedSpec
import qualified Pawl.Codec.AttackerDeclaredSpec
import qualified Pawl.Codec.AttackingPlayersSpec
import qualified Pawl.Codec.BattlefieldCandidateSpec
import qualified Pawl.Codec.BecameAttackedSpec
import qualified Pawl.Codec.BecameBlockingSpec
import qualified Pawl.Codec.BecameDesignatedSpec
import qualified Pawl.Codec.BecomeCopySpec
import qualified Pawl.Codec.BeginningStepSpec
import qualified Pawl.Codec.BindingSpec
import qualified Pawl.Codec.BlockCostSpec
import qualified Pawl.Codec.BlockPermissionSpec
import qualified Pawl.Codec.BlockRequirementSpec
import qualified Pawl.Codec.BlocksDeclaredSpec
import qualified Pawl.Codec.CandidateIdSpec
import qualified Pawl.Codec.CantBeBlockedBySpec
import qualified Pawl.Codec.CantBeRegeneratedSpec
import qualified Pawl.Codec.CardNameSpec
import qualified Pawl.Codec.CardSpec
import qualified Pawl.Codec.CardTypeSpec
import qualified Pawl.Codec.CastObligationSpec
import qualified Pawl.Codec.CastOfferSpec
import qualified Pawl.Codec.CastingPermissionSpec
import qualified Pawl.Codec.CastingRestrictionSpec
import qualified Pawl.Codec.ChangeSubtypeWordSpec
import qualified Pawl.Codec.ChangeTextSpec
import qualified Pawl.Codec.CharacteristicPTSpec
import qualified Pawl.Codec.ChooseBetweenSpec
import qualified Pawl.Codec.ChooserSpec
import qualified Pawl.Codec.ChosenCardFromAmongSpec
import qualified Pawl.Codec.ChosenCardInGraveyardSpec
import qualified Pawl.Codec.ChosenCardInHandSpec
import qualified Pawl.Codec.ClassLevelChangeSpec
import qualified Pawl.Codec.ClassLevelSpec
import qualified Pawl.Codec.ClauseIndexSpec
import qualified Pawl.Codec.ClauseSpec
import qualified Pawl.Codec.CoinFlippedSpec
import qualified Pawl.Codec.ColorSpec
import qualified Pawl.Codec.CombatRestrictionSpec
import qualified Pawl.Codec.CombatSpec
import qualified Pawl.Codec.CombatStepSpec
import qualified Pawl.Codec.ComparesSpec
import qualified Pawl.Codec.ComparisonSpec
import qualified Pawl.Codec.ConditionSpec
import qualified Pawl.Codec.ContinuousEffectSpec
import qualified Pawl.Codec.ControlChangedSpec
import qualified Pawl.Codec.ControllerBecomesTargetSpec
import qualified Pawl.Codec.ControllerRelationSpec
import qualified Pawl.Codec.CopyExceptionSpec
import qualified Pawl.Codec.CopySpellSpec
import qualified Pawl.Codec.CostComponentSpec
import qualified Pawl.Codec.CostReductionSpec
import qualified Pawl.Codec.CostScaleSpec
import qualified Pawl.Codec.CostSpec
import qualified Pawl.Codec.CountSpec
import qualified Pawl.Codec.CountedDiscardSpec
import qualified Pawl.Codec.CounterChangeSpec
import qualified Pawl.Codec.CounterKindSpec
import qualified Pawl.Codec.CounterNameSpec
import qualified Pawl.Codec.CounterPatternSpec
import qualified Pawl.Codec.CounterPlacementSpec
import qualified Pawl.Codec.CounterRSpec
import qualified Pawl.Codec.CounterRestrictionSpec
import qualified Pawl.Codec.CounterSpec
import qualified Pawl.Codec.CounterSubjectSpec
import qualified Pawl.Codec.CounterabilitySpec
import qualified Pawl.Codec.CounteringSpec
import qualified Pawl.Codec.CreateCopySpec
import qualified Pawl.Codec.CreateSpec
import qualified Pawl.Codec.CreatureBecomesBlockedByAtLeastSpec
import qualified Pawl.Codec.CyclingSpec
import qualified Pawl.Codec.DamageDirectionSpec
import qualified Pawl.Codec.DamageEventSpec
import qualified Pawl.Codec.DamageKindSpec
import qualified Pawl.Codec.DamagePatternSpec
import qualified Pawl.Codec.DamagePreventedSpec
import qualified Pawl.Codec.DamageRSpec
import qualified Pawl.Codec.DamageRewriteSpec
import qualified Pawl.Codec.DaytimeSpec
import qualified Pawl.Codec.DealDamageSpec
import qualified Pawl.Codec.DeciderSpec
import qualified Pawl.Codec.DefenseSpec
import qualified Pawl.Codec.DelayedTriggerSpec
import qualified Pawl.Codec.DepartureSpec
import qualified Pawl.Codec.DesignateSpec
import qualified Pawl.Codec.DesignationSpec
import qualified Pawl.Codec.DestroySpec
import qualified Pawl.Codec.DestructionRewriteSpec
import qualified Pawl.Codec.DiscardCardsSpec
import qualified Pawl.Codec.DiscardCauseSpec
import qualified Pawl.Codec.DiscardSpec
import qualified Pawl.Codec.DiscardedSpec
import qualified Pawl.Codec.DrawSpec
import qualified Pawl.Codec.DrewSpec
import qualified Pawl.Codec.DungeonRoomSpec
import qualified Pawl.Codec.DurationRefSpec
import qualified Pawl.Codec.DurationSpec
import qualified Pawl.Codec.DuringPhaseSpec
import qualified Pawl.Codec.EachCardFromAmongSpec
import qualified Pawl.Codec.EachCardInGraveyardSpec
import qualified Pawl.Codec.EachCardInHandSpec
import qualified Pawl.Codec.EffectSpec
import qualified Pawl.Codec.EndTurnSignalSpec
import qualified Pawl.Codec.EndingStepSpec
import qualified Pawl.Codec.EntryFlipSpec
import qualified Pawl.Codec.EntryOptionSpec
import qualified Pawl.Codec.EntryRSpec
import qualified Pawl.Codec.EntryRestrictionSpec
import qualified Pawl.Codec.EntryRewriteSpec
import qualified Pawl.Codec.EntryRidersSpec
import qualified Pawl.Codec.EventGroupSpec
import qualified Pawl.Codec.EventShapeSpec
import qualified Pawl.Codec.ExcessDestinationSpec
import qualified Pawl.Codec.ExchangeSidesSpec
import qualified Pawl.Codec.ExileCardsFromGraveyardSpec
import qualified Pawl.Codec.ExileHauntingSpec
import qualified Pawl.Codec.ExilePlayPermissionSpec
import qualified Pawl.Codec.ExpirySpec
import qualified Pawl.Codec.ExtraPhaseSpec
import qualified Pawl.Codec.ExtraTurnSpec
import qualified Pawl.Codec.FaceDownCharacteristicsSpec
import qualified Pawl.Codec.FaceDownReasonSpec
import qualified Pawl.Codec.FaceDownStateSpec
import qualified Pawl.Codec.FaceSpec
import qualified Pawl.Codec.FacingSpec
import qualified Pawl.Codec.FightSpec
import qualified Pawl.Codec.FilterSpec
import qualified Pawl.Codec.FlipCoinSpec
import qualified Pawl.Codec.FloatingCandidateSpec
import qualified Pawl.Codec.ForEachSpec
import qualified Pawl.Codec.GameEventSpec
import qualified Pawl.Codec.GrantPlayFromExileSpec
import qualified Pawl.Codec.GrantedAbilitySpec
import qualified Pawl.Codec.GraveyardScopeSpec
import qualified Pawl.Codec.HalfUnlockedSpec
import qualified Pawl.Codec.HalvedSpec
import qualified Pawl.Codec.HandActionSpec
import qualified Pawl.Codec.HybridSpec
import qualified Pawl.Codec.IgnoredAbilitySpec
import qualified Pawl.Codec.InZoneSpec
import qualified Pawl.Codec.IncreaseActivationCostSpec
import qualified Pawl.Codec.IncreaseSpellCostSpec
import qualified Pawl.Codec.InherentTriggerSourceSpec
import qualified Pawl.Codec.InstanceOrdinalSpec
import qualified Pawl.Codec.KeywordFamilySpec
import qualified Pawl.Codec.KeywordSpec
import qualified Pawl.Codec.LastKnownSpec
import qualified Pawl.Codec.LayoutSpec
import qualified Pawl.Codec.LibraryPlacementSpec
import qualified Pawl.Codec.LibraryPositionSpec
import qualified Pawl.Codec.LifeChangeSpec
import qualified Pawl.Codec.LimitUnlessSpec
import qualified Pawl.Codec.LoggedEventSpec
import qualified Pawl.Codec.LookAtSpec
import qualified Pawl.Codec.LoyaltySpec
import qualified Pawl.Codec.ManaAdditionSpec
import qualified Pawl.Codec.ManaCostSpec
import qualified Pawl.Codec.ManaCountSpec
import qualified Pawl.Codec.ManaFilterSpec
import qualified Pawl.Codec.ManaProductionSpec
import qualified Pawl.Codec.ManaRestrictionSpec
import qualified Pawl.Codec.ManaRetentionSpec
import qualified Pawl.Codec.ManaRiderEffectSpec
import qualified Pawl.Codec.ManaRiderSpec
import qualified Pawl.Codec.ManaSpec
import qualified Pawl.Codec.ManaSpendingSpec
import qualified Pawl.Codec.ManaSymbolSpec
import qualified Pawl.Codec.ManaTypeSpec
import qualified Pawl.Codec.ManaUnitSpec
import qualified Pawl.Codec.MentoredSpec
import qualified Pawl.Codec.MillSpec
import qualified Pawl.Codec.MillTallySpec
import qualified Pawl.Codec.MilledSpec
import qualified Pawl.Codec.ModalSpec
import qualified Pawl.Codec.ModeIndexSpec
import qualified Pawl.Codec.ModeSelectionSpec
import qualified Pawl.Codec.ModeSpec
import qualified Pawl.Codec.ModificationSpec
import qualified Pawl.Codec.ModifyPowerToughnessSpec
import qualified Pawl.Codec.ModifyTargetSpec
import qualified Pawl.Codec.MonarchTargetSpec
import qualified Pawl.Codec.MonarchWatchSpec
import qualified Pawl.Codec.MorphSpec
import qualified Pawl.Codec.MorphVariantSpec
import qualified Pawl.Codec.MoveCountersSpec
import qualified Pawl.Codec.MoveToZoneSpec
import qualified Pawl.Codec.MovedBetweenSpec
import qualified Pawl.Codec.MovedSpec
import qualified Pawl.Codec.ObjectIdSpec
import qualified Pawl.Codec.ObjectRefSpec
import qualified Pawl.Codec.ObjectSpec
import qualified Pawl.Codec.OfferCastSpec
import qualified Pawl.Codec.OnsetSpec
import qualified Pawl.Codec.OptionalitySpec
import qualified Pawl.Codec.PaidExpirySpec
import qualified Pawl.Codec.PayBranchSpec
import qualified Pawl.Codec.PayGateSpec
import qualified Pawl.Codec.PayObligationSpec
import qualified Pawl.Codec.PendingEntryEffectSpec
import qualified Pawl.Codec.PermanentBecomesDesignatedSpec
import qualified Pawl.Codec.PermanentCandidateSpec
import qualified Pawl.Codec.PermanentSacrificedSpec
import qualified Pawl.Codec.PhasePatternSpec
import qualified Pawl.Codec.PhaseSelectorSpec
import qualified Pawl.Codec.PhaseSpec
import qualified Pawl.Codec.PhasedOutSpec
import qualified Pawl.Codec.PlayPermissionOriginSpec
import qualified Pawl.Codec.PlayerAttacksWithSpec
import qualified Pawl.Codec.PlayerCounterKindSpec
import qualified Pawl.Codec.PlayerCounterTallySpec
import qualified Pawl.Codec.PlayerCountersSpec
import qualified Pawl.Codec.PlayerDrawsNthCardSpec
import qualified Pawl.Codec.PlayerEffectSpec
import qualified Pawl.Codec.PlayerIdSpec
import qualified Pawl.Codec.PlayerQuantitySpec
import qualified Pawl.Codec.PlayerRefSpec
import qualified Pawl.Codec.PlayerRelationSpec
import qualified Pawl.Codec.PlayerSacrificesSpec
import qualified Pawl.Codec.PlayerScopeSpec
import qualified Pawl.Codec.PlayerSpec
import qualified Pawl.Codec.PlayerStaticAbilitySpec
import qualified Pawl.Codec.PlusSpec
import qualified Pawl.Codec.PoolSpec
import qualified Pawl.Codec.PowerSpec
import qualified Pawl.Codec.PreventAllDamageSpec
import qualified Pawl.Codec.PreventNextDamageSpec
import qualified Pawl.Codec.PreventionRiderSpec
import qualified Pawl.Codec.PreventionSpec
import qualified Pawl.Codec.PrintedReplacementSpec
import qualified Pawl.Codec.PrintingIdSpec
import qualified Pawl.Codec.PrintingSpec
import qualified Pawl.Codec.ProductionTagSpec
import qualified Pawl.Codec.ProjectedCharacteristicsSpec
import qualified Pawl.Codec.PutCountersSpec
import qualified Pawl.Codec.QuantitySpec
import qualified Pawl.Codec.RecipientSpec
import qualified Pawl.Codec.RedirectDamageSpec
import qualified Pawl.Codec.ReduceActivationCostSpec
import qualified Pawl.Codec.ReduceSpellCostSpec
import qualified Pawl.Codec.RegenerabilitySpec
import qualified Pawl.Codec.ReinforceSpec
import qualified Pawl.Codec.RemoveCountersSpec
import qualified Pawl.Codec.ReplaceSpec
import qualified Pawl.Codec.ReplacementEffectSpec
import qualified Pawl.Codec.ReplacementOriginSpec
import qualified Pawl.Codec.RequireAttackSpec
import qualified Pawl.Codec.RequireBlockSpec
import qualified Pawl.Codec.RestartSignalSpec
import qualified Pawl.Codec.ResultSpec
import qualified Pawl.Codec.RevealCauseSpec
import qualified Pawl.Codec.RevealSpec
import qualified Pawl.Codec.RevealedSpec
import qualified Pawl.Codec.RollDieSpec
import qualified Pawl.Codec.RoomIndexSpec
import qualified Pawl.Codec.RoundingSpec
import qualified Pawl.Codec.SacrificeAnyNumberSpec
import qualified Pawl.Codec.SacrificeRestrictionSpec
import qualified Pawl.Codec.SacrificeSpec
import qualified Pawl.Codec.ScalingSpec
import qualified Pawl.Codec.ScopeSpec
import qualified Pawl.Codec.SearchDestinationSpec
import qualified Pawl.Codec.SearchSpec
import qualified Pawl.Codec.SelfCountersReachedSpec
import qualified Pawl.Codec.SetBasePowerToughnessSpec
import qualified Pawl.Codec.SetClassLevelSpec
import qualified Pawl.Codec.SetPowerToughnessSpec
import qualified Pawl.Codec.ShuffleIntoLibrarySpec
import qualified Pawl.Codec.SicknessSpec
import qualified Pawl.Codec.SkipNextPhaseSpec
import qualified Pawl.Codec.SlotNameSpec
import qualified Pawl.Codec.SourceSpec
import qualified Pawl.Codec.SpecialActionSpec
import qualified Pawl.Codec.SpeedDecreaseSpec
import qualified Pawl.Codec.SpellCastSpec
import qualified Pawl.Codec.SpellWasCastSpec
import qualified Pawl.Codec.SpendManaAsThoughSpec
import qualified Pawl.Codec.StackObjectKindSpec
import qualified Pawl.Codec.StaticAbilitySpec
import qualified Pawl.Codec.StatusSpec
import qualified Pawl.Codec.StepBeganSpec
import qualified Pawl.Codec.StepBeginsSpec
import qualified Pawl.Codec.SubtypeFamilySpec
import qualified Pawl.Codec.SubtypeSpec
import qualified Pawl.Codec.SupertypeSpec
import qualified Pawl.Codec.TakeExtraTurnSpec
import qualified Pawl.Codec.TapForTotalPowerSpec
import qualified Pawl.Codec.TapPermanentsSpec
import qualified Pawl.Codec.TapStateSpec
import qualified Pawl.Codec.TargetCountSpec
import qualified Pawl.Codec.TargetSlotSpec
import qualified Pawl.Codec.TimestampSpec
import qualified Pawl.Codec.TokenPatternSpec
import qualified Pawl.Codec.TokenRSpec
import qualified Pawl.Codec.TopOfLibrarySpec
import qualified Pawl.Codec.TopOfLibraryUntilSpec
import qualified Pawl.Codec.ToughnessSpec
import qualified Pawl.Codec.TransformedSpec
import qualified Pawl.Codec.TriggerConditionSpec
import qualified Pawl.Codec.TriggerFrequencySpec
import qualified Pawl.Codec.TriggerLimitSpec
import qualified Pawl.Codec.TriggeredAbilitySourceSpec
import qualified Pawl.Codec.TriggeredAbilitySpec
import qualified Pawl.Codec.TurnFaceDownSpec
import qualified Pawl.Codec.TurnScopeSpec
import qualified Pawl.Codec.TurnUpProcedureSpec
import qualified Pawl.Codec.TurnUpRSpec
import qualified Pawl.Codec.TurnUpRewriteSpec
import qualified Pawl.Codec.TurnWindowSpec
import qualified Pawl.Codec.TypeLineSpec
import qualified Pawl.Codec.UntapRestrictionSpec
import qualified Pawl.Codec.UntapRewriteSpec
import qualified Pawl.Codec.UsesSpec
import qualified Pawl.Codec.VentureMarkerEnteredSpec
import qualified Pawl.Codec.WhileSpec
import qualified Pawl.Codec.WithCountersSpec
import qualified Pawl.Codec.ZoneChangePatternSpec
import qualified Pawl.Codec.ZoneChangeRSpec
import qualified Pawl.Codec.ZoneChangeSpec
import qualified Pawl.Codec.ZoneSpec
import qualified Pawl.CodecIntegrationSpec
import qualified Pawl.CoinSpec
import qualified Pawl.ColorSpec
import qualified Pawl.CombatEffectSpec
import qualified Pawl.CombatSpec
import qualified Pawl.CommanderSpec
import qualified Pawl.ConditionSpec
import qualified Pawl.CopySpec
import qualified Pawl.CoreSpec
import qualified Pawl.CostSpec
import qualified Pawl.CountSpec
import qualified Pawl.CounterRestrictionSpec
import qualified Pawl.CounterspellSpec
import qualified Pawl.CrewSpec
import qualified Pawl.DamageSpec
import qualified Pawl.DaytimeSpec
import qualified Pawl.DecideSpec
import qualified Pawl.DecimalSpec
import qualified Pawl.DepartureSpec
import qualified Pawl.DetainSpec
import qualified Pawl.DiceSpec
import qualified Pawl.DungeonSpec
import qualified Pawl.EngineSpec
import qualified Pawl.EntryRestrictionSpec
import qualified Pawl.EventSpec
import qualified Pawl.EventTriggerSpec
import qualified Pawl.ExileSpec
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
import qualified Pawl.Extra.Word8Spec
import qualified Pawl.FaceDownSpec
import qualified Pawl.FilterSpec
import qualified Pawl.GameSpec
import qualified Pawl.GoadSpec
import qualified Pawl.InvestigateSpec
import qualified Pawl.Json.ArraySpec
import qualified Pawl.Json.BooleanSpec
import qualified Pawl.Json.NullSpec
import qualified Pawl.Json.NumberSpec
import qualified Pawl.Json.ObjectSpec
import qualified Pawl.Json.PairSpec
import qualified Pawl.Json.StringSpec
import qualified Pawl.Json.ValueSpec
import qualified Pawl.JsonCodec.ArmSpec
import qualified Pawl.JsonCodec.CommonSpec
import qualified Pawl.JsonCodec.FieldsSpec
import qualified Pawl.JsonPointer.EvaluateSpec
import qualified Pawl.JsonPointer.PointerSpec
import qualified Pawl.JsonPointer.TokenSpec
import qualified Pawl.JsonSchema.DefineSpec
import qualified Pawl.JsonSchema.NameSpec
import qualified Pawl.JsonSchema.PatternSpec
import qualified Pawl.JsonSchema.SchemaSpec
import qualified Pawl.JsonSchema.ValidateSpec
import qualified Pawl.KeywordTriggerSpec
import qualified Pawl.LibraryOrderSpec
import qualified Pawl.ManaSpec
import qualified Pawl.MassEffectSpec
import qualified Pawl.ModalDoubleFacedSpec
import qualified Pawl.ModalSpec
import qualified Pawl.MoveCounterSpec
import qualified Pawl.MulliganSpec
import qualified Pawl.PhasingSpec
import qualified Pawl.PlaneswalkerSpec
import qualified Pawl.PlayerEffectSpec
import qualified Pawl.PowerToughnessSpec
import qualified Pawl.ProjectionSpec
import qualified Pawl.RadSpec
import qualified Pawl.Registry as Registry
import qualified Pawl.RegistrySpec
import qualified Pawl.ReplacementSpec
import qualified Pawl.ReplaySpec
import qualified Pawl.ResolveSpec
import qualified Pawl.RingSpec
import qualified Pawl.RoomSpec
import qualified Pawl.SacrificeRestrictionSpec
import qualified Pawl.SagaSpec
import qualified Pawl.SetupSpec
import qualified Pawl.SlugSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.SpecialActionSpec
import qualified Pawl.SpeedSpec
import qualified Pawl.SplitSecondSpec
import qualified Pawl.TargetSpec
import qualified Pawl.TransformSpec
import qualified Pawl.TriggerSpec
import qualified Pawl.TurnSpec
import qualified Pawl.UntapRestrictionSpec
import qualified Pawl.Uri.FragmentSpec
import qualified Pawl.VariableEffectSpec
import qualified Pawl.ZoneChangeSpec
import qualified Pawl.ZoneTriggerSpec
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
    ( -- Pawl.EngineSpec comes first because tasty hands tests to its thread
      -- pool in tree order, and this group holds the suite's two longest cases
      -- by an order of magnitude. Started last they were the whole tail.
      fmap
        (Tasty.localOption (Tasty.mkTimeout 300000000))
        (Writer.execWriter (Pawl.EngineSpec.spec tasty registry))
        <> [Tasty.testGroup "spec" . Writer.execWriter $ spec tasty registry]
        -- These two subtrees are wired separately because their timeouts are
        -- tasty options and Pawl.Spec cannot express one. Every case now has
        -- SOME budget under CI: flake.nix's testFlags pass --timeout 15s to the
        -- `nix build` check phase, which used to run a bare `Setup test` and
        -- leave tasty at NoTimeout. Read that figure off flake.nix and not off
        -- here: it was 5s when #1446 added it and 15s since #1882, and a stale
        -- copy of it in this comment is what sent #2113 chasing a timeout that
        -- was not happening. These two keep their own budgets anyway, because
        -- localOption beats the command line -- so they hold whatever an agent
        -- passes, and ReplacementSpec's five seconds is now TIGHTER than that
        -- floor, which costs nothing at 250x its slowest case. Each is
        -- at least 30x its group's slowest case, rounded up to a round
        -- number: headroom for a loaded shared runner, not a
        -- speed assertion. The option is deliberately NOT hoisted onto the
        -- whole "pawl" group, because that same precedence would silently make
        -- an agent's --timeout ineffective suite-wide.
        --
        -- Pawl.ReplacementSpec guards CR 616.1's termination, where a
        -- regression hangs rather than fails; the fuller rationale is at that
        -- module's `spec`. Slowest case 0.02s, so five seconds.
        <> fmap
          (Tasty.localOption (Tasty.mkTimeout 5000000))
          (Writer.execWriter (Pawl.ReplacementSpec.spec tasty registry))
          -- Pawl.EngineSpec, wired above, guards the same failure mode one level
          -- up: it asserts that a whole game terminates. Its budget is the looser
          -- one because each case plays out one or two complete games rather than
          -- resolving a single event -- slowest case 9.96s measured 2026-08-23,
          -- so 300 seconds.
    )

-- Specs that reach for card data take a registry over the same monad they
-- assert in. The rest need none at all.
spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = do
  Pawl.ActivateSpec.spec s registry
  Pawl.AdventureSpec.spec s registry
  Pawl.AuraSpec.spec s registry
  Pawl.BattleSpec.spec s registry
  Pawl.BindingSpec.spec s
  Pawl.CardSpec.spec s registry
  Pawl.CardsSpec.spec s
  Pawl.CaseSpec.spec s registry
  Pawl.ClassSpec.spec s registry
  Pawl.CastSpec.spec s registry
  Pawl.CoinSpec.spec s registry
  Pawl.Codec.AbilityNameSpec.spec s
  Pawl.Codec.AbilityTriggeredSpec.spec s
  Pawl.Codec.ActivatedAbilitySourceSpec.spec s
  Pawl.Codec.ActivatedAbilitySpec.spec s
  Pawl.Codec.ActivationRestrictionSpec.spec s
  Pawl.Codec.ActiveAttackRequirementSpec.spec s
  Pawl.Codec.ActiveBlockRequirementSpec.spec s
  Pawl.Codec.ActivePlayerEffectSpec.spec s
  Pawl.Codec.ActiveReplacementSpec.spec s
  Pawl.Codec.ActiveUnregeneratableSpec.spec s
  Pawl.Codec.AddActivationCostSpec.spec s
  Pawl.Codec.AddSpellCostSpec.spec s
  Pawl.Codec.AffectPlayersSpec.spec s
  Pawl.Codec.AffectedPlayersSpec.spec s
  Pawl.Codec.AffectedSpec.spec s
  Pawl.Codec.AffectedUnlessSpec.spec s
  Pawl.Codec.AfterTurnSpec.spec s
  Pawl.Codec.AgainstSlotSpec.spec s
  Pawl.Codec.AggregationSpec.spec s
  Pawl.Codec.AlternativeCostSpec.spec s
  Pawl.Codec.ArmDelayedTriggerSpec.spec s
  Pawl.Codec.AsCopySpec.spec s
  Pawl.Codec.AttachRestrictionSpec.spec s
  Pawl.Codec.AttachTargetSpec.spec s
  Pawl.Codec.AttackCostSpec.spec s
  Pawl.Codec.AttackRequirementSpec.spec s
  Pawl.Codec.AttackTargetSpec.spec s
  Pawl.Codec.AttackerBlockedSpec.spec s
  Pawl.Codec.AttackerDeclaredSpec.spec s
  Pawl.Codec.AttackingPlayersSpec.spec s
  Pawl.Codec.BattlefieldCandidateSpec.spec s
  Pawl.Codec.BecameAttackedSpec.spec s
  Pawl.Codec.BecameDesignatedSpec.spec s
  Pawl.Codec.BecomeCopySpec.spec s
  Pawl.Codec.BeginningStepSpec.spec s
  Pawl.Codec.BindingSpec.spec s
  Pawl.Codec.BlockCostSpec.spec s
  Pawl.Codec.BlockPermissionSpec.spec s
  Pawl.Codec.BlockRequirementSpec.spec s
  Pawl.Codec.BecameBlockingSpec.spec s
  Pawl.Codec.BlocksDeclaredSpec.spec s
  Pawl.Codec.CandidateIdSpec.spec s
  Pawl.Codec.CantBeBlockedBySpec.spec s
  Pawl.Codec.CantBeRegeneratedSpec.spec s
  Pawl.Codec.CardNameSpec.spec s
  Pawl.Codec.CardSpec.spec s
  Pawl.Codec.CardTypeSpec.spec s
  Pawl.Codec.CastObligationSpec.spec s
  Pawl.Codec.CastOfferSpec.spec s
  Pawl.Codec.CastingPermissionSpec.spec s
  Pawl.Codec.CastingRestrictionSpec.spec s
  Pawl.Codec.ChangeSubtypeWordSpec.spec s
  Pawl.Codec.ChangeTextSpec.spec s
  Pawl.Codec.CharacteristicPTSpec.spec s
  Pawl.Codec.ChooseBetweenSpec.spec s
  Pawl.Codec.ChooserSpec.spec s
  Pawl.Codec.ChosenCardFromAmongSpec.spec s
  Pawl.Codec.ChosenCardInGraveyardSpec.spec s
  Pawl.Codec.ChosenCardInHandSpec.spec s
  Pawl.Codec.ClassLevelChangeSpec.spec s
  Pawl.Codec.ClassLevelSpec.spec s
  Pawl.Codec.ClauseIndexSpec.spec s
  Pawl.Codec.ClauseSpec.spec s
  Pawl.Codec.CoinFlippedSpec.spec s
  Pawl.Codec.ColorSpec.spec s
  Pawl.Codec.CombatRestrictionSpec.spec s
  Pawl.Codec.CombatSpec.spec s
  Pawl.Codec.CombatStepSpec.spec s
  Pawl.Codec.ComparesSpec.spec s
  Pawl.Codec.ComparisonSpec.spec s
  Pawl.Codec.ConditionSpec.spec s
  Pawl.Codec.ContinuousEffectSpec.spec s
  Pawl.Codec.ControlChangedSpec.spec s
  Pawl.Codec.ControllerBecomesTargetSpec.spec s
  Pawl.Codec.ControllerRelationSpec.spec s
  Pawl.Codec.CopyExceptionSpec.spec s
  Pawl.Codec.CostComponentSpec.spec s
  Pawl.Codec.CostReductionSpec.spec s
  Pawl.Codec.CostScaleSpec.spec s
  Pawl.Codec.CostSpec.spec s
  Pawl.Codec.CountSpec.spec s
  Pawl.Codec.CountedDiscardSpec.spec s
  Pawl.Codec.CounterChangeSpec.spec s
  Pawl.Codec.CounterKindSpec.spec s
  Pawl.Codec.CounterNameSpec.spec s
  Pawl.Codec.CounterPatternSpec.spec s
  Pawl.Codec.CounterRSpec.spec s
  Pawl.Codec.CounterSpec.spec s
  Pawl.Codec.CounterSubjectSpec.spec s
  Pawl.Codec.CounterabilitySpec.spec s
  Pawl.Codec.CounteringSpec.spec s
  Pawl.Codec.CopySpellSpec.spec s
  Pawl.Codec.CreateCopySpec.spec s
  Pawl.Codec.CreateSpec.spec s
  Pawl.Codec.CreatureBecomesBlockedByAtLeastSpec.spec s
  Pawl.Codec.CyclingSpec.spec s
  Pawl.Codec.DamageDirectionSpec.spec s
  Pawl.Codec.DamageEventSpec.spec s
  Pawl.Codec.DamageKindSpec.spec s
  Pawl.Codec.DamagePatternSpec.spec s
  Pawl.Codec.DamagePreventedSpec.spec s
  Pawl.Codec.DamageRSpec.spec s
  Pawl.Codec.DamageRewriteSpec.spec s
  Pawl.Codec.DaytimeSpec.spec s
  Pawl.Codec.DealDamageSpec.spec s
  Pawl.Codec.DeciderSpec.spec s
  Pawl.Codec.DefenseSpec.spec s
  Pawl.Codec.DelayedTriggerSpec.spec s
  Pawl.Codec.DepartureSpec.spec s
  Pawl.Codec.DesignateSpec.spec s
  Pawl.Codec.DesignationSpec.spec s
  Pawl.Codec.DestroySpec.spec s
  Pawl.Codec.DestructionRewriteSpec.spec s
  Pawl.Codec.DiscardCardsSpec.spec s
  Pawl.Codec.DiscardCauseSpec.spec s
  Pawl.Codec.DiscardSpec.spec s
  Pawl.Codec.DiscardedSpec.spec s
  Pawl.Codec.DrawSpec.spec s
  Pawl.Codec.DrewSpec.spec s
  Pawl.Codec.DungeonRoomSpec.spec s
  Pawl.Codec.DurationRefSpec.spec s
  Pawl.Codec.DurationSpec.spec s
  Pawl.Codec.DuringPhaseSpec.spec s
  Pawl.Codec.EachCardFromAmongSpec.spec s
  Pawl.Codec.EachCardInGraveyardSpec.spec s
  Pawl.Codec.EachCardInHandSpec.spec s
  Pawl.Codec.EffectSpec.spec s
  Pawl.Codec.EndTurnSignalSpec.spec s
  Pawl.Codec.EndingStepSpec.spec s
  Pawl.Codec.EntryFlipSpec.spec s
  Pawl.Codec.EntryOptionSpec.spec s
  Pawl.Codec.EntryRSpec.spec s
  Pawl.Codec.CounterRestrictionSpec.spec s
  Pawl.Codec.EntryRestrictionSpec.spec s
  Pawl.Codec.EntryRewriteSpec.spec s
  Pawl.Codec.EntryRidersSpec.spec s
  Pawl.Codec.EventGroupSpec.spec s
  Pawl.Codec.EventShapeSpec.spec s
  Pawl.Codec.ExcessDestinationSpec.spec s
  Pawl.Codec.ExchangeSidesSpec.spec s
  Pawl.Codec.ExileCardsFromGraveyardSpec.spec s
  Pawl.Codec.ExileHauntingSpec.spec s
  Pawl.Codec.ExilePlayPermissionSpec.spec s
  Pawl.Codec.ExpirySpec.spec s
  Pawl.Codec.ExtraPhaseSpec.spec s
  Pawl.Codec.ExtraTurnSpec.spec s
  Pawl.Codec.FaceDownCharacteristicsSpec.spec s
  Pawl.Codec.FaceDownReasonSpec.spec s
  Pawl.Codec.FaceDownStateSpec.spec s
  Pawl.Codec.FaceSpec.spec s
  Pawl.Codec.FacingSpec.spec s
  Pawl.Codec.FightSpec.spec s
  Pawl.Codec.FilterSpec.spec s
  Pawl.Codec.FlipCoinSpec.spec s
  Pawl.Codec.FloatingCandidateSpec.spec s
  Pawl.Codec.ForEachSpec.spec s
  Pawl.Codec.GameEventSpec.spec s
  Pawl.Codec.GrantPlayFromExileSpec.spec s
  Pawl.Codec.GrantedAbilitySpec.spec s
  Pawl.Codec.GraveyardScopeSpec.spec s
  Pawl.Codec.HalfUnlockedSpec.spec s
  Pawl.Codec.HalvedSpec.spec s
  Pawl.Codec.HandActionSpec.spec s
  Pawl.Codec.HybridSpec.spec s
  Pawl.Codec.IgnoredAbilitySpec.spec s
  Pawl.Codec.InZoneSpec.spec s
  Pawl.Codec.IncreaseActivationCostSpec.spec s
  Pawl.Codec.IncreaseSpellCostSpec.spec s
  Pawl.Codec.InherentTriggerSourceSpec.spec s
  Pawl.Codec.InstanceOrdinalSpec.spec s
  Pawl.Codec.KeywordFamilySpec.spec s
  Pawl.Codec.KeywordSpec.spec s
  Pawl.Codec.LastKnownSpec.spec s
  Pawl.Codec.LayoutSpec.spec s
  Pawl.Codec.LibraryPlacementSpec.spec s
  Pawl.Codec.LibraryPositionSpec.spec s
  Pawl.Codec.LifeChangeSpec.spec s
  Pawl.Codec.LimitUnlessSpec.spec s
  Pawl.Codec.LoggedEventSpec.spec s
  Pawl.Codec.LookAtSpec.spec s
  Pawl.Codec.LoyaltySpec.spec s
  Pawl.Codec.ManaAdditionSpec.spec s
  Pawl.Codec.ManaCostSpec.spec s
  Pawl.Codec.ManaCountSpec.spec s
  Pawl.Codec.ManaFilterSpec.spec s
  Pawl.Codec.ManaProductionSpec.spec s
  Pawl.Codec.ManaRestrictionSpec.spec s
  Pawl.Codec.ManaRetentionSpec.spec s
  Pawl.Codec.ManaRiderEffectSpec.spec s
  Pawl.Codec.ManaRiderSpec.spec s
  Pawl.Codec.ManaSpec.spec s
  Pawl.Codec.ManaSpendingSpec.spec s
  Pawl.Codec.ManaSymbolSpec.spec s
  Pawl.Codec.ManaTypeSpec.spec s
  Pawl.Codec.ManaUnitSpec.spec s
  Pawl.Codec.MentoredSpec.spec s
  Pawl.Codec.MillSpec.spec s
  Pawl.Codec.MillTallySpec.spec s
  Pawl.Codec.MilledSpec.spec s
  Pawl.Codec.ModalSpec.spec s
  Pawl.Codec.ModeIndexSpec.spec s
  Pawl.Codec.ModeSelectionSpec.spec s
  Pawl.Codec.ModeSpec.spec s
  Pawl.Codec.ModificationSpec.spec s
  Pawl.Codec.ModifyPowerToughnessSpec.spec s
  Pawl.Codec.ModifyTargetSpec.spec s
  Pawl.Codec.MonarchTargetSpec.spec s
  Pawl.Codec.MonarchWatchSpec.spec s
  Pawl.Codec.MorphSpec.spec s
  Pawl.Codec.MorphVariantSpec.spec s
  Pawl.Codec.MoveToZoneSpec.spec s
  Pawl.Codec.MovedBetweenSpec.spec s
  Pawl.Codec.MovedSpec.spec s
  Pawl.Codec.ObjectIdSpec.spec s
  Pawl.Codec.ObjectRefSpec.spec s
  Pawl.Codec.ObjectSpec.spec s
  Pawl.Codec.OfferCastSpec.spec s
  Pawl.Codec.OnsetSpec.spec s
  Pawl.Codec.OptionalitySpec.spec s
  Pawl.Codec.PaidExpirySpec.spec s
  Pawl.Codec.PayBranchSpec.spec s
  Pawl.Codec.PayGateSpec.spec s
  Pawl.Codec.PayObligationSpec.spec s
  Pawl.Codec.PendingEntryEffectSpec.spec s
  Pawl.Codec.PermanentBecomesDesignatedSpec.spec s
  Pawl.Codec.CounterPlacementSpec.spec s
  Pawl.Codec.PermanentCandidateSpec.spec s
  Pawl.Codec.PermanentSacrificedSpec.spec s
  Pawl.Codec.PhasePatternSpec.spec s
  Pawl.Codec.PhaseSelectorSpec.spec s
  Pawl.Codec.PhaseSpec.spec s
  Pawl.Codec.PhasedOutSpec.spec s
  Pawl.Codec.PlayPermissionOriginSpec.spec s
  Pawl.Codec.PlayerCounterKindSpec.spec s
  Pawl.Codec.PlayerAttacksWithSpec.spec s
  Pawl.Codec.PlayerCounterTallySpec.spec s
  Pawl.Codec.PlayerCountersSpec.spec s
  Pawl.Codec.PlayerDrawsNthCardSpec.spec s
  Pawl.Codec.PlayerEffectSpec.spec s
  Pawl.Codec.PlayerIdSpec.spec s
  Pawl.Codec.PlayerQuantitySpec.spec s
  Pawl.Codec.PlayerRefSpec.spec s
  Pawl.Codec.PlayerRelationSpec.spec s
  Pawl.Codec.PlayerSacrificesSpec.spec s
  Pawl.Codec.PlayerScopeSpec.spec s
  Pawl.Codec.PlayerSpec.spec s
  Pawl.Codec.PlayerStaticAbilitySpec.spec s
  Pawl.Codec.PlusSpec.spec s
  Pawl.Codec.PoolSpec.spec s
  Pawl.Codec.PowerSpec.spec s
  Pawl.Codec.PreventAllDamageSpec.spec s
  Pawl.Codec.PreventNextDamageSpec.spec s
  Pawl.Codec.PreventionRiderSpec.spec s
  Pawl.Codec.PreventionSpec.spec s
  Pawl.Codec.PrintedReplacementSpec.spec s
  Pawl.Codec.PrintingIdSpec.spec s
  Pawl.Codec.PrintingSpec.spec s
  Pawl.Codec.ProductionTagSpec.spec s
  Pawl.Codec.ProjectedCharacteristicsSpec.spec s
  Pawl.Codec.MoveCountersSpec.spec s
  Pawl.Codec.PutCountersSpec.spec s
  Pawl.Codec.QuantitySpec.spec s
  Pawl.Codec.RecipientSpec.spec s
  Pawl.Codec.RedirectDamageSpec.spec s
  Pawl.Codec.ReduceActivationCostSpec.spec s
  Pawl.Codec.ReduceSpellCostSpec.spec s
  Pawl.Codec.RegenerabilitySpec.spec s
  Pawl.Codec.ReinforceSpec.spec s
  Pawl.Codec.RemoveCountersSpec.spec s
  Pawl.Codec.ReplaceSpec.spec s
  Pawl.Codec.ReplacementEffectSpec.spec s
  Pawl.Codec.ReplacementOriginSpec.spec s
  Pawl.Codec.RequireAttackSpec.spec s
  Pawl.Codec.RequireBlockSpec.spec s
  Pawl.Codec.RestartSignalSpec.spec s
  Pawl.Codec.ResultSpec.spec s
  Pawl.Codec.RevealCauseSpec.spec s
  Pawl.Codec.RevealSpec.spec s
  Pawl.Codec.RevealedSpec.spec s
  Pawl.Codec.RollDieSpec.spec s
  Pawl.Codec.RoomIndexSpec.spec s
  Pawl.Codec.RoundingSpec.spec s
  Pawl.Codec.SacrificeAnyNumberSpec.spec s
  Pawl.Codec.SacrificeRestrictionSpec.spec s
  Pawl.Codec.SacrificeSpec.spec s
  Pawl.Codec.ScalingSpec.spec s
  Pawl.Codec.ScopeSpec.spec s
  Pawl.Codec.SearchDestinationSpec.spec s
  Pawl.Codec.SearchSpec.spec s
  Pawl.Codec.SelfCountersReachedSpec.spec s
  Pawl.Codec.SetBasePowerToughnessSpec.spec s
  Pawl.Codec.SetClassLevelSpec.spec s
  Pawl.Codec.SetPowerToughnessSpec.spec s
  Pawl.Codec.ShuffleIntoLibrarySpec.spec s
  Pawl.Codec.SicknessSpec.spec s
  Pawl.Codec.SkipNextPhaseSpec.spec s
  Pawl.Codec.SlotNameSpec.spec s
  Pawl.Codec.SourceSpec.spec s
  Pawl.Codec.SpecialActionSpec.spec s
  Pawl.Codec.SpeedDecreaseSpec.spec s
  Pawl.Codec.SpellCastSpec.spec s
  Pawl.Codec.SpellWasCastSpec.spec s
  Pawl.Codec.SpendManaAsThoughSpec.spec s
  Pawl.Codec.StackObjectKindSpec.spec s
  Pawl.Codec.StaticAbilitySpec.spec s
  Pawl.Codec.StatusSpec.spec s
  Pawl.Codec.StepBeganSpec.spec s
  Pawl.Codec.StepBeginsSpec.spec s
  Pawl.Codec.SubtypeFamilySpec.spec s
  Pawl.Codec.SubtypeSpec.spec s
  Pawl.Codec.SupertypeSpec.spec s
  Pawl.Codec.TakeExtraTurnSpec.spec s
  Pawl.Codec.TapForTotalPowerSpec.spec s
  Pawl.Codec.TapPermanentsSpec.spec s
  Pawl.Codec.TapStateSpec.spec s
  Pawl.Codec.TargetCountSpec.spec s
  Pawl.Codec.TargetSlotSpec.spec s
  Pawl.Codec.TimestampSpec.spec s
  Pawl.Codec.TokenPatternSpec.spec s
  Pawl.Codec.TokenRSpec.spec s
  Pawl.Codec.TopOfLibrarySpec.spec s
  Pawl.Codec.TopOfLibraryUntilSpec.spec s
  Pawl.Codec.ToughnessSpec.spec s
  Pawl.Codec.TransformedSpec.spec s
  Pawl.Codec.TriggerConditionSpec.spec s
  Pawl.Codec.TriggerFrequencySpec.spec s
  Pawl.Codec.TriggerLimitSpec.spec s
  Pawl.Codec.TriggeredAbilitySourceSpec.spec s
  Pawl.Codec.TriggeredAbilitySpec.spec s
  Pawl.Codec.TurnFaceDownSpec.spec s
  Pawl.Codec.TurnScopeSpec.spec s
  Pawl.Codec.TurnUpProcedureSpec.spec s
  Pawl.Codec.TurnUpRSpec.spec s
  Pawl.Codec.TurnUpRewriteSpec.spec s
  Pawl.Codec.UntapRewriteSpec.spec s
  Pawl.Codec.TurnWindowSpec.spec s
  Pawl.Codec.TypeLineSpec.spec s
  Pawl.Codec.UntapRestrictionSpec.spec s
  Pawl.Codec.UsesSpec.spec s
  Pawl.Codec.VentureMarkerEnteredSpec.spec s
  Pawl.Codec.WhileSpec.spec s
  Pawl.Codec.WithCountersSpec.spec s
  Pawl.Codec.ZoneChangePatternSpec.spec s
  Pawl.Codec.ZoneChangeRSpec.spec s
  Pawl.Codec.ZoneChangeSpec.spec s
  Pawl.Codec.ZoneSpec.spec s
  Pawl.CodecIntegrationSpec.spec s registry
  Pawl.ColorSpec.spec s registry
  Pawl.CombatEffectSpec.spec s registry
  Pawl.CombatSpec.spec s registry
  Pawl.CommanderSpec.spec s registry
  Pawl.ConditionSpec.spec s registry
  Pawl.CopySpec.spec s registry
  Pawl.CopySpec.copySpellSpec s registry
  Pawl.CoreSpec.spec s registry
  Pawl.CostSpec.spec s registry
  Pawl.CountSpec.spec s registry
  Pawl.CounterspellSpec.spec s registry
  Pawl.CrewSpec.spec s registry
  Pawl.DamageSpec.spec s registry
  Pawl.DungeonSpec.spec s registry
  Pawl.DaytimeSpec.spec s registry
  Pawl.DecideSpec.spec s
  Pawl.DecimalSpec.spec s
  Pawl.DepartureSpec.spec s registry
  Pawl.DetainSpec.spec s registry
  Pawl.DiceSpec.spec s registry
  Pawl.EventSpec.spec s registry
  Pawl.EventTriggerSpec.spec s registry
  Pawl.ExileSpec.spec s registry
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
  Pawl.Extra.Word8Spec.spec s
  Pawl.FaceDownSpec.spec s registry
  Pawl.FilterSpec.spec s
  Pawl.GameSpec.spec s registry
  Pawl.GoadSpec.spec s registry
  Pawl.Json.ArraySpec.spec s
  Pawl.Json.BooleanSpec.spec s
  Pawl.Json.NullSpec.spec s
  Pawl.Json.NumberSpec.spec s
  Pawl.Json.ObjectSpec.spec s
  Pawl.Json.PairSpec.spec s
  Pawl.Json.StringSpec.spec s
  Pawl.Json.ValueSpec.spec s
  Pawl.JsonCodec.ArmSpec.spec s
  Pawl.JsonCodec.CommonSpec.spec s
  Pawl.JsonCodec.FieldsSpec.spec s
  Pawl.JsonPointer.EvaluateSpec.spec s
  Pawl.JsonPointer.PointerSpec.spec s
  Pawl.JsonPointer.TokenSpec.spec s
  Pawl.JsonSchema.DefineSpec.spec s
  Pawl.JsonSchema.NameSpec.spec s
  Pawl.JsonSchema.PatternSpec.spec s
  Pawl.JsonSchema.SchemaSpec.spec s
  Pawl.JsonSchema.ValidateSpec.spec s
  Pawl.InvestigateSpec.spec s registry
  Pawl.KeywordTriggerSpec.spec s registry
  Pawl.LibraryOrderSpec.spec s registry
  Pawl.ManaSpec.spec s registry
  Pawl.MassEffectSpec.spec s registry
  Pawl.ModalDoubleFacedSpec.spec s registry
  Pawl.ModalSpec.spec s registry
  Pawl.MulliganSpec.spec s registry
  Pawl.PhasingSpec.spec s registry
  Pawl.PlaneswalkerSpec.spec s registry
  Pawl.PlaneswalkerSpec.combinedLoyaltyCostSpec s registry
  Pawl.PlaneswalkerSpec.countersRemovedSpec s registry
  Pawl.PlaneswalkerSpec.variableLoyaltySpec s registry
  Pawl.PlaneswalkerSpec.gristLoyaltySpec s registry
  Pawl.PlayerEffectSpec.spec s registry
  Pawl.PowerToughnessSpec.spec s registry
  Pawl.ProjectionSpec.spec s registry
  Pawl.RadSpec.spec s registry
  Pawl.RegistrySpec.spec s
  Pawl.ReplaySpec.spec s registry
  Pawl.MoveCounterSpec.spec s registry
  Pawl.ResolveSpec.spec s registry
  Pawl.RingSpec.spec s registry
  Pawl.RoomSpec.spec s registry
  Pawl.SetupSpec.spec s registry
  Pawl.SlugSpec.spec s
  Pawl.CounterRestrictionSpec.spec s registry
  Pawl.EntryRestrictionSpec.spec s registry
  Pawl.SacrificeRestrictionSpec.spec s registry
  Pawl.SagaSpec.spec s registry
  Pawl.SpecialActionSpec.spec s registry
  Pawl.SpeedSpec.spec s registry
  Pawl.SplitSecondSpec.spec s registry
  Pawl.TargetSpec.spec s registry
  Pawl.TransformSpec.spec s registry
  Pawl.TriggerSpec.spec s registry
  Pawl.TurnSpec.spec s registry
  Pawl.UntapRestrictionSpec.spec s registry
  Pawl.Uri.FragmentSpec.spec s
  Pawl.VariableEffectSpec.spec s registry
  Pawl.ZoneChangeSpec.spec s registry
  Pawl.ZoneTriggerSpec.spec s registry
