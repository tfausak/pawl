import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.BindingSpec as BindingSpec
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.Cards as Cards
import qualified Pawl.CardsSpec as CardsSpec
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.CodecSpec as CodecSpec
import qualified Pawl.ColorSpec as ColorSpec
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.CopySpec as CopySpec
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.CostSpec as CostSpec
import qualified Pawl.DamageSpec as DamageSpec
import qualified Pawl.DecideSpec as DecideSpec
import qualified Pawl.EventSpec as EventSpec
import qualified Pawl.ExpirySpec as ExpirySpec
import qualified Pawl.FilterSpec as FilterSpec
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.JsonSpec as JsonSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.ModalSpec as ModalSpec
import qualified Pawl.PlayerEffectSpec as PlayerEffectSpec
import qualified Pawl.PowerToughnessSpec as PowerToughnessSpec
import qualified Pawl.ProjectionSpec as ProjectionSpec
import qualified Pawl.PropertySpec as PropertySpec
import qualified Pawl.ReplacementSpec as ReplacementSpec
import qualified Pawl.ReplaySpec as ReplaySpec
import qualified Pawl.ResolveSpec as ResolveSpec
import qualified Pawl.SetupSpec as SetupSpec
import qualified Pawl.TriggerSpec as TriggerSpec
import qualified Pawl.TurnSpec as TurnSpec
import qualified Test.Tasty as Tasty

main :: IO ()
main = do
  cards <- Cards.loadCards
  Tasty.defaultMain (testTree cards)

testTree :: Cards.Cards -> Tasty.TestTree
testTree cards =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests cards,
      BindingSpec.tests,
      CardSpec.tests cards,
      CardsSpec.tests cards,
      TurnSpec.tests cards,
      GameSpec.tests cards,
      SetupSpec.tests cards,
      DamageSpec.tests cards,
      DecideSpec.tests,
      EventSpec.tests cards,
      ExpirySpec.tests cards,
      ReplaySpec.tests cards,
      PropertySpec.tests cards,
      JsonSpec.tests,
      CodecSpec.tests cards,
      ManaSpec.tests cards,
      CastSpec.tests cards,
      CostSpec.tests cards,
      CombatSpec.tests cards,
      ResolveSpec.tests cards,
      ProjectionSpec.tests cards,
      PowerToughnessSpec.tests cards,
      PlayerEffectSpec.tests cards,
      ActivateSpec.tests cards,
      ModalSpec.tests cards,
      CopySpec.tests cards,
      ReplacementSpec.tests cards,
      ColorSpec.tests cards,
      TriggerSpec.tests cards,
      FilterSpec.tests
    ]
