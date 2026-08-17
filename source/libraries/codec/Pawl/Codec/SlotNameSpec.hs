module Pawl.Codec.SlotNameSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SlotName" $ do
  Spec.it s "MkSlotName" $
    Common.assertCodec
      s
      SlotName.codec
      (SlotName.MkSlotName (Text.pack "target"))
      " \"target\" "

  Spec.it s "has a schema" $
    Common.assertHasSchema s SlotName.codec

  Spec.it s "rejects a non-string" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " 1 ") >>= Codec.decode SlotName.codec))
      "expected a decode failure"
