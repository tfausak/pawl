import qualified Pawl.ActivateSpec as ActivateSpec
import qualified Pawl.CardSpec as CardSpec
import qualified Pawl.CardsSpec as CardsSpec
import qualified Pawl.CastSpec as CastSpec
import qualified Pawl.CodecSpec as CodecSpec
import qualified Pawl.CombatSpec as CombatSpec
import qualified Pawl.CoreSpec as CoreSpec
import qualified Pawl.DamageSpec as DamageSpec
import qualified Pawl.DecideSpec as DecideSpec
import qualified Pawl.EventSpec as EventSpec
import qualified Pawl.GameSpec as GameSpec
import qualified Pawl.JsonSpec as JsonSpec
import qualified Pawl.ManaSpec as ManaSpec
import qualified Pawl.ProjectionSpec as ProjectionSpec
import qualified Pawl.PropertySpec as PropertySpec
import qualified Pawl.ReplaySpec as ReplaySpec
import qualified Pawl.ResolveSpec as ResolveSpec
import qualified Pawl.SetupSpec as SetupSpec
import qualified Pawl.TurnSpec as TurnSpec
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ CoreSpec.tests,
      CardSpec.tests,
      CardsSpec.tests,
      TurnSpec.tests,
      GameSpec.tests,
      SetupSpec.tests,
      DamageSpec.tests,
      DecideSpec.tests,
      EventSpec.tests,
      ReplaySpec.tests,
      PropertySpec.tests,
      JsonSpec.tests,
      CodecSpec.tests,
      ManaSpec.tests,
      CastSpec.tests,
      CombatSpec.tests,
      ResolveSpec.tests,
      ProjectionSpec.tests,
      ActivateSpec.tests
    ]
