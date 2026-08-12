{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ControllerRelationSpec where

import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControllerRelation" $ do
  Spec.it s "Yours" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Yours
      """ {"type":"Yours"} """
  Spec.it s "Anyones" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Anyones
      """ {"type":"Anyones"} """
  Spec.it s "Opponents" $
    Common.assertCodec
      s
      ControllerRelation.codec
      ControllerRelation.Opponents
      """ {"type":"Opponents"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s ControllerRelation.codec
