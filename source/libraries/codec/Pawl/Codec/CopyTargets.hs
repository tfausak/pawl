module Pawl.Codec.CopyTargets where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CopyTargets as CopyTargets

-- | Tagged rather than an enum: CR 707.10d's and CR 707.10e's arms carry a ref
-- naming their candidates, where the other two carry nothing.
codec :: Codec.Codec CopyTargets.CopyTargets
codec =
  Arm.tagged
    [ Arm.nullary "Copied" CopyTargets.Copied,
      Arm.nullary "ChosenByController" CopyTargets.ChosenByController,
      Arm.payload "ForEach" ObjectRef.codec CopyTargets.ForEach (\x -> case x of CopyTargets.ForEach y -> Just y; _ -> Nothing),
      Arm.payload "Stated" ObjectRef.codec CopyTargets.Stated (\x -> case x of CopyTargets.Stated y -> Just y; _ -> Nothing)
    ]
