module Pawl.Codec.CoinFlippedSpec where

import qualified Pawl.Codec.CoinFlipped as CoinFlipped
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CoinFlipped as CoinFlipped
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CoinFlipped" $ do
  -- CR 705.2's two outcomes, both spelled: a codec that dropped the key would
  -- round-trip one of them by accident.
  Spec.it s "MkCoinFlipped, a won flip" $
    Common.assertCodec
      s
      CoinFlipped.codec
      ( CoinFlipped.MkCoinFlipped
          { CoinFlipped.flipper = PlayerId.MkPlayerId 4,
            CoinFlipped.won = True
          }
      )
      " {\"flipper\":4,\"won\":true} "
  Spec.it s "MkCoinFlipped, a lost flip" $
    Common.assertCodec
      s
      CoinFlipped.codec
      ( CoinFlipped.MkCoinFlipped
          { CoinFlipped.flipper = PlayerId.MkPlayerId 5,
            CoinFlipped.won = False
          }
      )
      " {\"flipper\":5,\"won\":false} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CoinFlipped.codec
