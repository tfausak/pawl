{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DesignationSpec where

import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Designation as Designation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Designation" $ do
  Spec.it s "Renowned" $
    Common.assertJsonCodec
      s
      Designation.toJson
      Designation.fromJson
      Designation.Renowned
      """ {"type":"Renowned"} """
  Spec.it s "Monstrous" $
    Common.assertJsonCodec
      s
      Designation.toJson
      Designation.fromJson
      Designation.Monstrous
      """ {"type":"Monstrous"} """
  Spec.it s "Suspected" $
    Common.assertJsonCodec
      s
      Designation.toJson
      Designation.fromJson
      Designation.Suspected
      """ {"type":"Suspected"} """
