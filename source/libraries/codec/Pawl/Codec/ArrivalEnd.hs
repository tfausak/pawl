module Pawl.Codec.ArrivalEnd where

import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ArrivalEnd as ArrivalEnd

codec :: Codec.Codec ArrivalEnd.ArrivalEnd
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "IntoLibrary" LibraryPosition.codec ArrivalEnd.IntoLibrary (\x -> case x of ArrivalEnd.IntoLibrary y -> Just y; _ -> Nothing),
      Arm.nullary "OntoGraveyard" ArrivalEnd.OntoGraveyard
    ]

tagOf :: ArrivalEnd.ArrivalEnd -> String
tagOf x = case x of
  ArrivalEnd.IntoLibrary {} -> "IntoLibrary"
  ArrivalEnd.OntoGraveyard -> "OntoGraveyard"
