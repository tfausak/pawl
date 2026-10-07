module Pawl.Codec.ReferenceSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName.Type
import qualified Pawl.Types.Label as Label.Type
import qualified Pawl.Types.Reference as Reference.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Reference" $ do
  Spec.it s "a label is written after $" $
    Common.assertCodec s Reference.codec (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bear"))) " \"$bear\" "
  Spec.it s "the first object with a card name is the bare name" $
    Common.assertCodec s Reference.codec (Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Grizzly Bears")) 1) " \"Grizzly Bears\" "
  Spec.it s "a later one carries its occurrence after #" $
    Common.assertCodec s Reference.codec (Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Grizzly Bears")) 2) " \"Grizzly Bears#2\" "
  Spec.it s "an ability on the stack is named by its kind and source" $
    Common.assertCodec s Reference.codec (Reference.Type.TriggerOf (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "bear")))) " \"trigger of $bear\" "
  Spec.it s "an activated one names a printed source too" $
    Common.assertCodec s Reference.codec (Reference.Type.AbilityOf (Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Grizzly Bears")) 2)) " \"ability of Grizzly Bears#2\" "
  Spec.it s "a cast spell is named by the card it was cast from" $
    Common.assertCodec s Reference.codec (Reference.Type.SpellOf (Reference.Type.Labelled (Label.Type.MkLabel (Text.pack "will")))) " \"spell of $will\" "
  Spec.it s "an explicit first occurrence reads as the bare name" $
    Common.assertFromJson s (Codec.decode Reference.codec) " \"Grizzly Bears#1\" " (Reference.Type.Printed (CardName.Type.MkCardName (Text.pack "Grizzly Bears")) 1)
  Spec.it s "rejects a bare @, an empty string and a non-number occurrence" $
    Spec.assertBool
      s
      (all (Either.isLeft . Reference.fromText . Text.pack) ["$", "", "Grizzly Bears#", "Grizzly Bears#two"])
      "expected every one to fail"
  Spec.it s "has a schema" $
    Common.assertHasSchema s Reference.codec
