module Pawl.Codec.LibraryPosition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.LibraryPosition as LibraryPosition

toJson :: LibraryPosition.LibraryPosition -> Value.Value
toJson p = Common.nullary $ case p of
  LibraryPosition.Top -> "Top"
  LibraryPosition.Bottom -> "Bottom"

fromJson :: Value.Value -> Either Text.Text LibraryPosition.LibraryPosition
fromJson =
  Common.decodeNullary
    "LibraryPosition"
    [ ("Top", LibraryPosition.Top),
      ("Bottom", LibraryPosition.Bottom)
    ]
