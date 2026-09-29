module Pawl.Codec.TappedIsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.TappedIs as TappedIs
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type
import qualified Pawl.Types.TapState as TapState.Type
import qualified Pawl.Types.TappedIs as TappedIs.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TappedIs" $ do
  Spec.it s "tapped is a flag" $
    Common.assertCodec s TappedIs.codec (TappedIs.Type.MkTappedIs (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bear"))) TapState.Type.Tapped) " {\"object\":\"@bear\",\"tapped\":true} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s TappedIs.codec
