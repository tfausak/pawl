{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounterabilitySpec where

import qualified Pawl.Codec.Counterability as Counterability
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Counterability as Counterability

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Counterability" $ do
  Spec.it s "Counterable" $
    Common.assertCodec
      s
      Counterability.codec
      Counterability.Counterable
      """ {"type":"Counterable"} """
  Spec.it s "CantBeCountered" $
    Common.assertCodec
      s
      Counterability.codec
      Counterability.CantBeCountered
      """ {"type":"CantBeCountered"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Counterability.codec
