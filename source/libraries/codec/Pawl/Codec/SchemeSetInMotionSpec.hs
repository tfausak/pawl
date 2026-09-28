module Pawl.Codec.SchemeSetInMotionSpec where

import qualified Pawl.Codec.SchemeSetInMotion as SchemeSetInMotion
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.SchemeSetInMotion as SchemeSetInMotion

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SchemeSetInMotion" $ do
  Spec.it s "keys the player and the scheme" $
    Common.assertCodec
      s
      SchemeSetInMotion.codec
      (SchemeSetInMotion.MkSchemeSetInMotion (PlayerId.MkPlayerId 1) (ObjectId.MkObjectId 7))
      " {\"player\":1,\"scheme\":7} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s SchemeSetInMotion.codec
