module Pawl.Codec.Operand where

import qualified Pawl.Codec.BoundMeasure as BoundMeasure
import qualified Pawl.Codec.Measure as Measure
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.Operand as Operand

codec :: Codec.Codec Operand.Operand
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Literal" Common.integer Operand.Literal (\x -> case x of Operand.Literal y -> Just y; _ -> Nothing),
      Arm.payload "OfSource" Measure.codec Operand.OfSource (\x -> case x of Operand.OfSource y -> Just y; _ -> Nothing),
      Arm.payload "Own" Measure.codec Operand.Own (\x -> case x of Operand.Own y -> Just y; _ -> Nothing),
      Arm.payload "OfBound" BoundMeasure.codec Operand.OfBound (\x -> case x of Operand.OfBound y -> Just y; _ -> Nothing),
      Arm.payload "AmountInSlot" SlotName.codec Operand.AmountInSlot (\x -> case x of Operand.AmountInSlot y -> Just y; _ -> Nothing),
      Arm.nullary "EnclosingAmount" Operand.EnclosingAmount
    ]

tagOf :: Operand.Operand -> String
tagOf x = case x of
  Operand.Literal {} -> "Literal"
  Operand.OfSource {} -> "OfSource"
  Operand.Own {} -> "Own"
  Operand.OfBound {} -> "OfBound"
  Operand.AmountInSlot {} -> "AmountInSlot"
  Operand.EnclosingAmount {} -> "EnclosingAmount"
