import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Pawl.ActivateSpec
import qualified Pawl.AuraSpec
import qualified Pawl.BindingSpec
import qualified Pawl.CardSpec
import qualified Pawl.CardsSpec
import qualified Pawl.CastSpec
import qualified Pawl.CodecSpec
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
import qualified Pawl.JsonSpec
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
  Pawl.CardsSpec.spec s registry
  Pawl.CastSpec.spec s registry
  Pawl.CodecSpec.spec s registry
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
  Pawl.JsonSpec.spec s
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
