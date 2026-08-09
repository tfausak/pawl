{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.LibraryPlacementSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.LibraryPlacement as LibraryPlacement
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LibraryPlacement" $ do
  -- A stated end keeps the POSITION's tag, so no card file moved when the
  -- placement type landed.
  Spec.it s "Stated Top" $
    Common.assertJsonCodec
      s
      LibraryPlacement.toJson
      LibraryPlacement.fromJson
      (LibraryPlacement.Stated LibraryPosition.Top)
      """ {"type":"Top"} """
  Spec.it s "Stated Bottom" $
    Common.assertJsonCodec
      s
      LibraryPlacement.toJson
      LibraryPlacement.fromJson
      (LibraryPlacement.Stated LibraryPosition.Bottom)
      """ {"type":"Bottom"} """
  Spec.it s "OwnerChooses" $
    Common.assertJsonCodec
      s
      LibraryPlacement.toJson
      LibraryPlacement.fromJson
      LibraryPlacement.OwnerChooses
      """ {"type":"OwnerChooses"} """
