module Pawl.Codec.ActivationSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Activation as Activation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Activation as Activation.Type
import qualified Pawl.Types.Choices as Choices.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Activation" $ do
  Spec.it s "an ability index beside its choices" $
    Common.assertCodec
      s
      Activation.codec
      Activation.Type.MkActivation
        { Activation.Type.object = Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "stone")),
          Activation.Type.ability = Just 1,
          Activation.Type.choices = Choices.Type.none {Choices.Type.x = Just 2}
        }
      " {\"object\":\"$stone\",\"ability\":1,\"x\":2} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Activation.codec
