module Pawl.Codec.LifeIsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.LifeIs as LifeIs
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.LifeIs as LifeIs.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeIs" $ do
  Spec.it s "a player and a life total" $
    Common.assertCodec s LifeIs.codec (LifeIs.Type.MkLifeIs (Label.Type.MkLabel (Text.pack "bob")) 18) " {\"player\":\"bob\",\"life\":18} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s LifeIs.codec
