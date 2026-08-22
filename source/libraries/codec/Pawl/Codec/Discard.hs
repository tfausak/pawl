module Pawl.Codec.Discard where

import qualified Pawl.Codec.CountedDiscard as CountedDiscard
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.Discard as Discard

-- | Tagged like every other sum. Each arm carries a payload of its own rather
-- than a positional array (#1464): Counted's is the record CR 701.9b's two
-- questions make -- who, and how many -- and These' is the ref that names the
-- set outright.
codec :: Codec.Codec Discard.Discard
codec =
  Arm.tagged
    [ Arm.payload "Counted" CountedDiscard.codec Discard.Counted (\x -> case x of Discard.Counted y -> Just y; _ -> Nothing),
      Arm.payload "These" ObjectRef.codec Discard.These (\x -> case x of Discard.These y -> Just y; _ -> Nothing)
    ]
