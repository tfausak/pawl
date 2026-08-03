{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ControllerRelationSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ControllerRelation" $ do
  Spec.it s "Yours" $
    Common.assertJsonCodec
      s
      ControllerRelation.toJson
      ControllerRelation.fromJson
      ControllerRelation.Yours
      """ {"type":"Yours"} """
  Spec.it s "Anyones" $
    Common.assertJsonCodec
      s
      ControllerRelation.toJson
      ControllerRelation.fromJson
      ControllerRelation.Anyones
      """ {"type":"Anyones"} """
  Spec.it s "Opponents" $
    Common.assertJsonCodec
      s
      ControllerRelation.toJson
      ControllerRelation.fromJson
      ControllerRelation.Opponents
      """ {"type":"Opponents"} """
