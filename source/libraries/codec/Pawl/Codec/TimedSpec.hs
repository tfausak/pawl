module Pawl.Codec.TimedSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.Timed as Timed
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CombatStep as CombatStep.Type
import qualified Pawl.Types.Entry as Entry.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Move as Move.Type
import qualified Pawl.Types.Phase as Phase.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.Timed as Timed.Type
import qualified Pawl.Types.When as When.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Timed" $ do
  Spec.it s "one flat object" $
    Common.assertCodec
      s
      Timed.codec
      (Timed.Type.MkTimed (When.Type.MkWhen 1 (Phase.Type.Combat CombatStep.Type.CombatDamage) (Label.Type.MkLabel (Text.pack "alice"))) (Just (ref "bear")) (Entry.Type.Do (Move.Type.AssignDamage (Map.singleton (ref "wall") 2))))
      " {\"turn\":1,\"step\":\"CombatDamage\",\"player\":\"alice\",\"source\":\"$bear\",\"do\":{\"AssignDamage\":{\"$wall\":2}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Timed.codec

ref :: String -> Reference.Type.Reference
ref = Reference.Type.Labelled . Label.Type.MkLabel . Text.pack
