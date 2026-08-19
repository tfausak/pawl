module Pawl.Codec.DrewSpec where

import qualified Pawl.Codec.Drew as Drew
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Drew as Drew
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Drew" $ do
  -- CR 121.1, and WHICH draw of the turn it was.
  Spec.it s "MkDrew, both keys" $
    Common.assertCodec
      s
      Drew.codec
      (Drew.MkDrew {Drew.player = PlayerId.MkPlayerId 0, Drew.nth = 2})
      " {\"player\":0,\"nth\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Drew.codec
