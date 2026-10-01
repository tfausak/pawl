module Pawl.Codec.AnswerSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Answer as Answer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Answer as Answer.Type
import qualified Pawl.Types.Reply as Reply.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Answer" $ do
  Spec.it s "a prompt and its answer" $
    Common.assertCodec s Answer.codec (Answer.Type.MkAnswer (Text.pack "ChooseDiscard") (Reply.Type.Array [Reply.Type.Text (Text.pack "@card")])) " {\"prompt\":\"ChooseDiscard\",\"with\":[\"@card\"]} "
  Spec.it s "an answer of every JSON shape" $
    Common.assertCodec s Answer.codec (Answer.Type.MkAnswer (Text.pack "X") (Reply.Type.Array [Reply.Type.Null, Reply.Type.Boolean True, Reply.Type.Number 3, Reply.Type.Object [(Text.pack "type", Reply.Type.Text (Text.pack "Keep"))]])) " {\"prompt\":\"X\",\"with\":[null,true,3,{\"type\":\"Keep\"}]} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Answer.codec
