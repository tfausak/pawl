module Pawl.Codec.WhenSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.When as When
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EndingStep as EndingStep.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Phase as Phase.Type
import qualified Pawl.Types.When as When.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.When" $ do
  Spec.it s "a turn, a flat step and a player" $
    Common.assertCodec s When.codec (When.Type.MkWhen 2 (Phase.Type.Ending EndingStep.Type.EndStep) (Label.Type.MkLabel (Text.pack "bob"))) " {\"turn\":2,\"step\":\"EndStep\",\"player\":\"bob\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s When.codec
