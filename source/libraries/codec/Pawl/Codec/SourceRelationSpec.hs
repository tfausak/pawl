{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SourceRelationSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.SourceRelation as SourceRelation
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SourceRelation as SourceRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SourceRelation" $ do
  Spec.it s "AnySource" $
    Common.assertJsonCodec
      s
      SourceRelation.toJson
      SourceRelation.fromJson
      SourceRelation.AnySource
      """ {"type":"AnySource"} """
  Spec.it s "TheSource" $
    Common.assertJsonCodec
      s
      SourceRelation.toJson
      SourceRelation.fromJson
      SourceRelation.TheSource
      """ {"type":"TheSource"} """
