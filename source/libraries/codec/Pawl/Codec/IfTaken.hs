module Pawl.Codec.IfTaken where

import qualified Pawl.Codec.ClauseIndex as ClauseIndex
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.IfTaken as IfTaken

codec :: Codec.Codec IfTaken.IfTaken
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "AnyTaken" (Common.nonEmpty ClauseIndex.codec) IfTaken.AnyTaken (\x -> case x of IfTaken.AnyTaken y -> Just y; _ -> Nothing),
      Arm.payload "NoneTaken" (Common.nonEmpty ClauseIndex.codec) IfTaken.NoneTaken (\x -> case x of IfTaken.NoneTaken y -> Just y; _ -> Nothing)
    ]

tagOf :: IfTaken.IfTaken -> String
tagOf x = case x of
  IfTaken.AnyTaken {} -> "AnyTaken"
  IfTaken.NoneTaken {} -> "NoneTaken"
