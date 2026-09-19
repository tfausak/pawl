module Pawl.Codec.MillCountRewrite where

import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MillCountRewrite as MillCountRewrite

-- The matcher is an irrefutable lambda rather than a case with a `_ -> Nothing`
-- fallthrough, Pawl.Codec.LifeGainRewrite's shape and for its reason: with one
-- constructor it is exhaustive as written, so a second constructor is a -Werror
-- incomplete-pattern here as well as in 'tagOf'.
codec :: Codec.Codec MillCountRewrite.MillCountRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Scaled" Scaling.codec MillCountRewrite.Scaled (\(MillCountRewrite.Scaled y) -> Just y)
    ]

tagOf :: MillCountRewrite.MillCountRewrite -> String
tagOf x = case x of
  MillCountRewrite.Scaled {} -> "Scaled"
