module Pawl.Codec.RemovalCount where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.RemovalCount as RemovalCount

-- | The tag that picks an arm is written by Pawl.Codec.RemoveCountersAmong's
-- `count` field.
codec :: Codec.Codec RemovalCount.RemovalCount
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Exactly" Quantity.codec RemovalCount.Exactly (\x -> case x of RemovalCount.Exactly y -> Just y; _ -> Nothing),
      Arm.payload "UpTo" Quantity.codec RemovalCount.UpTo (\x -> case x of RemovalCount.UpTo y -> Just y; _ -> Nothing),
      Arm.nullary "AnyNumber" RemovalCount.AnyNumber
    ]

tagOf :: RemovalCount.RemovalCount -> String
tagOf x = case x of
  RemovalCount.Exactly {} -> "Exactly"
  RemovalCount.UpTo {} -> "UpTo"
  RemovalCount.AnyNumber {} -> "AnyNumber"
