module Pawl.Codec.TokenPatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TokenPattern as TokenPattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.TokenPattern as TokenPattern

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.TokenPattern" . Spec.it s "MkTokenPattern" $
    Common.assertJsonCodec
      s
      TokenPattern.toJson
      TokenPattern.fromJson
      TokenPattern.MkTokenPattern {TokenPattern.whose = ControllerRelation.Yours}
      "{\"whose\":{\"type\":\"Yours\"}}"
