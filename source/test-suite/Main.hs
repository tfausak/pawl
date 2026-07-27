import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.AuraSpec as AuraSpec
import qualified Pawl.BindingSpec as BindingSpec
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.CardsSpec as CardsSpec
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.CodecSpec as CodecSpec
import qualified Pawl.ColorSpec as ColorSpec
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.ConditionSpec as ConditionSpec
import qualified Pawl.CopySpec as CopySpec
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.CostSpec as CostSpec
import qualified Pawl.CountSpec as CountSpec
import qualified Pawl.DamageSpec as DamageSpec
import qualified Pawl.DecideSpec as DecideSpec
import qualified Pawl.DepartureSpec as DepartureSpec
import qualified Pawl.EventSpec as EventSpec
import qualified Pawl.ExpirySpec as ExpirySpec
import qualified Pawl.ExtraSpec as ExtraSpec
import qualified Pawl.FilterSpec as FilterSpec
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.JsonSpec as JsonSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.ModalSpec as ModalSpec
import qualified Pawl.MulliganSpec as MulliganSpec
import qualified Pawl.PlayerEffectSpec as PlayerEffectSpec
import qualified Pawl.PowerToughnessSpec as PowerToughnessSpec
import qualified Pawl.ProjectionSpec as ProjectionSpec
import qualified Pawl.PropertySpec as PropertySpec
import qualified Pawl.Registry as Registry
import qualified Pawl.RegistrySpec as RegistrySpec
import qualified Pawl.ReplacementSpec as ReplacementSpec
import qualified Pawl.ReplaySpec as ReplaySpec
import qualified Pawl.ResolveSpec as ResolveSpec
import qualified Pawl.SetupSpec as SetupSpec
import qualified Pawl.SlugSpec as SlugSpec
import qualified Pawl.TriggerSpec as TriggerSpec
import qualified Pawl.TurnSpec as TurnSpec
import qualified Pawl.Type.Registry as Registry.Type
import qualified Test.Tasty as Tasty

main :: IO ()
main = do
  root <- Registry.defaultRoot
  registry <- Registry.new root
  Tasty.defaultMain (testTree registry)

testTree :: Registry.Type.Registry -> Tasty.TestTree
testTree registry =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests registry,
      BindingSpec.tests,
      CardSpec.tests registry,
      CardsSpec.tests registry,
      TurnSpec.tests registry,
      GameSpec.tests registry,
      SetupSpec.tests registry,
      MulliganSpec.tests registry,
      DamageSpec.tests registry,
      DecideSpec.tests,
      DepartureSpec.tests registry,
      EventSpec.tests registry,
      ExpirySpec.tests registry,
      ReplaySpec.tests registry,
      PropertySpec.tests registry,
      JsonSpec.tests,
      CodecSpec.tests registry,
      ManaSpec.tests registry,
      CastSpec.tests registry,
      CostSpec.tests registry,
      CombatSpec.tests registry,
      CountSpec.tests registry,
      ConditionSpec.tests registry,
      ResolveSpec.tests registry,
      ProjectionSpec.tests registry,
      PowerToughnessSpec.tests registry,
      PlayerEffectSpec.tests registry,
      ActivateSpec.tests registry,
      ModalSpec.tests registry,
      CopySpec.tests registry,
      ReplacementSpec.tests registry,
      ColorSpec.tests registry,
      TriggerSpec.tests registry,
      FilterSpec.tests,
      RegistrySpec.tests,
      SlugSpec.tests,
      ExtraSpec.tests,
      AuraSpec.tests registry
    ]
