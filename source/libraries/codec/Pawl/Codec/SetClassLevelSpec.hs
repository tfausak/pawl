module Pawl.Codec.SetClassLevelSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.SetClassLevel as SetClassLevel
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.SetClassLevel as SetClassLevel
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SetClassLevel" $ do
  -- CR 716.2a: "[Cost]: This Class's level becomes N."
  Spec.it s "MkSetClassLevel, both keys" $
    Common.assertCodec
      s
      SetClassLevel.codec
      ( SetClassLevel.MkSetClassLevel
          { SetClassLevel.level = ClassLevel.MkClassLevel 3,
            SetClassLevel.slot = SlotName.MkSlotName (Text.pack "self")
          }
      )
      " {\"level\":3,\"slot\":\"self\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SetClassLevel.codec
