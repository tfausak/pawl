import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.AuraSpec as AuraSpec
import qualified Pawl.BindingSpec
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.CardsSpec
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.CodecSpec as CodecSpec
import qualified Pawl.ColorSpec
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.ConditionSpec
import qualified Pawl.CopySpec
import qualified Pawl.CoreSpec
import qualified Pawl.CostSpec
import qualified Pawl.CountSpec
import qualified Pawl.DamageSpec as DamageSpec
import qualified Pawl.DecideSpec
import qualified Pawl.DecimalSpec
import qualified Pawl.DepartureSpec
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
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.Json.ArraySpec
import qualified Pawl.Json.BooleanSpec
import qualified Pawl.Json.NullSpec
import qualified Pawl.Json.NumberSpec
import qualified Pawl.Json.ObjectSpec
import qualified Pawl.Json.PairSpec
import qualified Pawl.Json.StringSpec
import qualified Pawl.Json.ValueSpec
import qualified Pawl.JsonSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.ModalSpec
import qualified Pawl.MulliganSpec
import qualified Pawl.PlaneswalkerSpec
import qualified Pawl.PlayerEffectSpec as PlayerEffectSpec
import qualified Pawl.PowerToughnessSpec
import qualified Pawl.ProjectionSpec as ProjectionSpec
import qualified Pawl.PropertySpec as PropertySpec
import qualified Pawl.Registry as Registry
import qualified Pawl.RegistrySpec
import qualified Pawl.ReplacementSpec as ReplacementSpec
import qualified Pawl.ReplaySpec
import qualified Pawl.ResolveSpec as ResolveSpec
import qualified Pawl.SetupSpec
import qualified Pawl.SlugSpec
import qualified Pawl.Spec as Spec
import qualified Pawl.TriggerSpec as TriggerSpec
import qualified Pawl.TurnSpec
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

main :: IO ()
main = do
  root <- Registry.defaultRoot
  registry <- Registry.new root
  Tasty.defaultMain (testTree registry)

tasty :: Spec.Spec IO (Writer.Writer [Tasty.TestTree])
tasty =
  Spec.MkSpec
    { Spec.assertFailure = HU.assertFailure,
      Spec.describe = \s -> Writer.tell . List.singleton . Tasty.testGroup s . Writer.execWriter,
      Spec.it = \s -> Writer.tell . List.singleton . HU.testCase s
    }

testTree :: Registry.Registry -> Tasty.TestTree
testTree registry =
  Tasty.testGroup
    "pawl"
    [ CardSpec.tests registry,
      GameSpec.tests registry,
      DamageSpec.tests registry,
      PropertySpec.tests registry,
      CodecSpec.tests registry,
      ManaSpec.tests registry,
      CastSpec.tests registry,
      CombatSpec.tests registry,
      ResolveSpec.tests registry,
      ProjectionSpec.tests registry,
      PlayerEffectSpec.tests registry,
      ActivateSpec.tests registry,
      ReplacementSpec.tests registry,
      TriggerSpec.tests registry,
      AuraSpec.tests registry,
      Tasty.testGroup "spec" . Writer.execWriter $ spec tasty registry
    ]

-- Specs that reach for card data are pinned to IO, since Registry.printing is an
-- IO action. The rest stay polymorphic in the assertion monad.
spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry -> n ()
spec s registry = do
  Pawl.BindingSpec.spec s
  Pawl.CardsSpec.spec s registry
  Pawl.ColorSpec.spec s registry
  Pawl.ConditionSpec.spec s registry
  Pawl.CopySpec.spec s registry
  Pawl.CoreSpec.spec s registry
  Pawl.CostSpec.spec s registry
  Pawl.CountSpec.spec s registry
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
  Pawl.Json.ArraySpec.spec s
  Pawl.Json.BooleanSpec.spec s
  Pawl.Json.NullSpec.spec s
  Pawl.Json.NumberSpec.spec s
  Pawl.Json.ObjectSpec.spec s
  Pawl.Json.PairSpec.spec s
  Pawl.Json.StringSpec.spec s
  Pawl.Json.ValueSpec.spec s
  Pawl.JsonSpec.spec s
  Pawl.ModalSpec.spec s registry
  Pawl.MulliganSpec.spec s registry
  Pawl.PlaneswalkerSpec.spec s registry
  Pawl.PowerToughnessSpec.spec s registry
  Pawl.RegistrySpec.spec s
  Pawl.ReplaySpec.spec s registry
  Pawl.SetupSpec.spec s registry
  Pawl.SlugSpec.spec s
  Pawl.TurnSpec.spec s registry
