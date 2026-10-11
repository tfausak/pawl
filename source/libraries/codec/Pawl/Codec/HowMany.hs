module Pawl.Codec.HowMany where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.HowMany as HowMany

-- | @UpTo@'s ceiling is an optional value, so "any number" writes the bare tag.
codec :: Codec.Codec HowMany.HowMany
codec =
  Arm.tagged
    tagOf
    [ Arm.nullary "One" HowMany.One,
      Arm.optionalPayload "UpTo" Quantity.codec HowMany.UpTo (\x -> case x of HowMany.UpTo y -> Just y; _ -> Nothing)
    ]

tagOf :: HowMany.HowMany -> String
tagOf x = case x of
  HowMany.One -> "One"
  HowMany.UpTo {} -> "UpTo"
