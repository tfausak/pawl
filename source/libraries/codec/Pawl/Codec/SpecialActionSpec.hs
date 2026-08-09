{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SpecialActionSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.SpecialAction as SpecialAction
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SpecialAction as SpecialAction

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.SpecialAction"
    . Spec.it s "DiscardThisAnyTime"
    $ Common.assertJsonCodec
      s
      SpecialAction.toJson
      SpecialAction.fromJson
      SpecialAction.DiscardThisAnyTime
      """ {"type":"DiscardThisAnyTime"} """
