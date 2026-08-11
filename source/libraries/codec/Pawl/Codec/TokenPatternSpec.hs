{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TokenPatternSpec where

import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.TokenPattern as TokenPattern

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TokenPattern" $ do
  Spec.it s "MkTokenPattern" $
    Common.assertJsonCodec
      s
      TokenPattern.toJson
      TokenPattern.fromJson
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours}
      """ {"whose":{"type":"Yours"}} """
  -- CR 109.5: Anyones is what a pattern that says nothing about the
  -- controller means, so the sole field's key is omitted.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      TokenPattern.toJson
      TokenPattern.fromJson
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Anyones}
      """ {} """
