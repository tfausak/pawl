module Pawl.Codec.CounteredEnd where

import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.CounteredEnd as CounteredEnd

codec :: Codec.Codec CounteredEnd.CounteredEnd
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "Stated" LibraryPosition.codec CounteredEnd.Stated (\x -> case x of CounteredEnd.Stated y -> Just y; _ -> Nothing),
      Arm.nullary "CounteringPlayerChooses" CounteredEnd.CounteringPlayerChooses
    ]

tagOf :: CounteredEnd.CounteredEnd -> String
tagOf x = case x of
  CounteredEnd.Stated {} -> "Stated"
  CounteredEnd.CounteringPlayerChooses {} -> "CounteringPlayerChooses"
