{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.SourceRelationSpec where

import qualified Pawl.Codec.SourceRelation as SourceRelation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SourceRelation as SourceRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SourceRelation" $ do
  Spec.it s "AnySource" $
    Common.assertCodec
      s
      SourceRelation.codec
      SourceRelation.AnySource
      """ {"type":"AnySource"} """
  Spec.it s "TheSource" $
    Common.assertCodec
      s
      SourceRelation.codec
      SourceRelation.TheSource
      """ {"type":"TheSource"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s SourceRelation.codec
