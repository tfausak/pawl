module Pawl.Codec.LabelSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Label as Label
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Label as Label.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Label" $ do
  Spec.it s "a label is a bare string" $
    Common.assertCodec s Label.codec (Label.Type.MkLabel (Text.pack "bear")) " \"bear\" "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Label.codec
