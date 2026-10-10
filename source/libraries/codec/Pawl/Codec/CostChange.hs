module Pawl.Codec.CostChange where

import qualified Pawl.Codec.AppliedReduction as AppliedReduction
import qualified Pawl.Codec.CostAddition as CostAddition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CostChange as CostChange

codec :: Codec.Codec CostChange.CostChange
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Increase" Common.natural CostChange.Increase (\x -> case x of CostChange.Increase y -> Just y; _ -> Nothing),
      Arm.payload "Reduce" AppliedReduction.codec CostChange.Reduce (\x -> case x of CostChange.Reduce y -> Just y; _ -> Nothing),
      Arm.payload "Add" CostAddition.codec CostChange.Add (\x -> case x of CostChange.Add y -> Just y; _ -> Nothing)
    ]

tagOf :: CostChange.CostChange -> String
tagOf x = case x of
  CostChange.Increase {} -> "Increase"
  CostChange.Reduce {} -> "Reduce"
  CostChange.Add {} -> "Add"
