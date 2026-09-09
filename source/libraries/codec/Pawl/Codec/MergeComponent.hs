module Pawl.Codec.MergeComponent where

import qualified Pawl.Codec.MeldSource as MeldSource
import qualified Pawl.Codec.PrintingId as PrintingId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.MergeComponent as MergeComponent

codec :: Codec.Codec MergeComponent.MergeComponent
codec =
  Arm.tagged
    [ Arm.payload "OfCard" PrintingId.codec MergeComponent.OfCard (\x -> case x of MergeComponent.OfCard y -> Just y; _ -> Nothing),
      Arm.payload "OfToken" PrintingId.codec MergeComponent.OfToken (\x -> case x of MergeComponent.OfToken y -> Just y; _ -> Nothing),
      Arm.payload "OfMeld" MeldSource.codec MergeComponent.OfMeld (\x -> case x of MergeComponent.OfMeld y -> Just y; _ -> Nothing)
    ]
