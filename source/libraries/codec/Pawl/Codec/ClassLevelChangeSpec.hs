module Pawl.Codec.ClassLevelChangeSpec where

import qualified Pawl.Codec.ClassLevelChange as ClassLevelChange
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.ClassLevelChange as ClassLevelChange
import qualified Pawl.Types.ObjectId as ObjectId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ClassLevelChange" $ do
  -- CR 716.2a's crossing: a level set from 1 straight to 3 crosses "becomes level
  -- 2" and "becomes level 3" at once, which neither level alone can say. BEFORE
  -- and AFTER deliberately differ here -- a symmetric fixture would round-trip a
  -- codec that swapped them.
  Spec.it s "MkClassLevelChange, every key" $
    Common.assertCodec
      s
      ClassLevelChange.codec
      ( ClassLevelChange.MkClassLevelChange
          { ClassLevelChange.object = ObjectId.MkObjectId 1,
            ClassLevelChange.before = ClassLevel.MkClassLevel 1,
            ClassLevelChange.after = ClassLevel.MkClassLevel 3
          }
      )
      " {\"object\":1,\"before\":1,\"after\":3} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ClassLevelChange.codec
