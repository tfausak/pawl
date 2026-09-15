module Pawl.Codec.LifeGainRewrite where

import qualified Pawl.Codec.Scaling as Scaling
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite

-- The matcher is an irrefutable lambda rather than a case with a `_ -> Nothing`
-- fallthrough, unlike its siblings: with one constructor it is exhaustive as
-- written, so a second constructor is a -Werror incomplete-pattern here as well
-- as in 'tagOf'.
codec :: Codec.Codec LifeGainRewrite.LifeGainRewrite
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Scaled" Scaling.codec LifeGainRewrite.Scaled (\(LifeGainRewrite.Scaled y) -> Just y)
    ]

tagOf :: LifeGainRewrite.LifeGainRewrite -> String
tagOf x = case x of
  LifeGainRewrite.Scaled {} -> "Scaled"
