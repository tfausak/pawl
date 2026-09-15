module Pawl.Codec.ManaType where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaType as ManaType

codec :: Codec.Codec ManaType.ManaType
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Colored" Color.codec ManaType.Colored (\x -> case x of ManaType.Colored y -> Just y; _ -> Nothing),
      Arm.nullary "Colorless" ManaType.Colorless
    ]

tagOf :: ManaType.ManaType -> String
tagOf x = case x of
  ManaType.Colored {} -> "Colored"
  ManaType.Colorless {} -> "Colorless"
