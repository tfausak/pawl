{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CopyExceptionSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyException as CopyException

-- CR 707.9: the "except ..." clause of a copy effect. One constructor -- and
-- the printed card it comes from, Quicksilver Gargantuan, is square, so the
-- asymmetric case below is what actually pins the array's order.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyException" $ do
  Spec.it s "SetPowerToughness round-trips" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.SetPowerToughness 7 7)
      """ {"type":"SetPowerToughness","value":[7,7]} """

  Spec.it s "SetPowerToughness writes power before toughness" $
    Common.assertCodec
      s
      CopyException.codec
      (CopyException.SetPowerToughness 4 5)
      """ {"type":"SetPowerToughness","value":[4,5]} """

  Spec.it s "rejects a payload of the wrong length" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack """ {"type":"SetPowerToughness","value":[4]} """) >>= Codec.decode CopyException.codec))
      "expected a decode failure"

  Spec.it s "has a schema" $
    Common.assertHasSchema s CopyException.codec
