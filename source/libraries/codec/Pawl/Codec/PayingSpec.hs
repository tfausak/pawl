module Pawl.Codec.PayingSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Paying as Paying
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Choices as Choices.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Paying as Paying.Type
import qualified Pawl.Types.PaymentDecision as PaymentDecision.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Paying" $ do
  Spec.it s "a decision alone" $
    Common.assertCodec s Paying.codec (Paying.Type.MkPaying PaymentDecision.Type.Declines Choices.Type.none) " {\"decision\":{\"type\":\"Declines\"}} "
  Spec.it s "a decision and the mana that pays it" $
    Common.assertCodec
      s
      Paying.codec
      (Paying.Type.MkPaying PaymentDecision.Type.Pays Choices.Type.none {Choices.Type.manaSources = Seq.fromList [Just (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "island")))]})
      " {\"decision\":{\"type\":\"Pays\"},\"mana\":[\"$island\"]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Paying.codec
