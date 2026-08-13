{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ChooserSpec where

import qualified Pawl.Codec.Chooser as Chooser
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Chooser as Chooser

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Chooser" $ do
  Spec.it s "TheController" $
    Common.assertCodec
      s
      Chooser.codec
      Chooser.TheController
      """ {"type":"TheController"} """
  Spec.it s "EachInScope" $
    Common.assertCodec
      s
      Chooser.codec
      Chooser.EachInScope
      """ {"type":"EachInScope"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Chooser.codec
