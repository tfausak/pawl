{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CopyExceptionSpec where

import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyException as CopyException

-- CR 707.9: the "except ..." clause of a copy effect. One constructor, so one
-- case -- Quicksilver Gargantuan's "except it's 7/7".
spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.CopyException"
    . Spec.it s "SetPowerToughness round-trips"
    $ Common.assertJsonCodec
      s
      CopyException.toJson
      CopyException.fromJson
      (CopyException.SetPowerToughness 7 7)
      """ {"type":"SetPowerToughness","value":[7,7]} """
