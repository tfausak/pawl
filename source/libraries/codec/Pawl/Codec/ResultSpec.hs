module Pawl.Codec.ResultSpec where

import qualified Pawl.Codec.Result as Result
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Result as Result

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Result" $ do
  -- CR 104.2: the winner is named, so the arm carries a seat.
  Spec.it s "Won" $
    Common.assertCodec
      s
      Result.codec
      (Result.Won (PlayerId.MkPlayerId 1))
      " {\"type\":\"Won\",\"value\":1} "
  -- CR 104.4: a draw names nobody.
  Spec.it s "Drawn" $
    Common.assertCodec
      s
      Result.codec
      Result.Drawn
      " {\"type\":\"Drawn\"} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s Result.codec
