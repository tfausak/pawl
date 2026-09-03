module Pawl.Codec.CopyTargets where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CopyTargets as CopyTargets

-- | Tagged rather than an enum: CR 707.10d's arm carries the ref naming its
-- candidates, where the other two carry nothing.
codec :: Codec.Codec CopyTargets.CopyTargets
codec =
  Arm.tagged
    [ Arm.nullary "Copied" CopyTargets.Copied,
      Arm.nullary "ChosenByController" CopyTargets.ChosenByController,
      Arm.payload "ForEach" ObjectRef.codec CopyTargets.ForEach (\x -> case x of CopyTargets.ForEach y -> Just y; _ -> Nothing)
    ]
