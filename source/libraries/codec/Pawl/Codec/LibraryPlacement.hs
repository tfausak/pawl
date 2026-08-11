module Pawl.Codec.LibraryPlacement where

import qualified Data.Text as Text
import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement

-- A stated placement writes the POSITION's own tag, so every card file written
-- before the placement type existed still round-trips unchanged and only
-- Aetherspouts' shape is new. The three tags are disjoint, which is what
-- Pawl.Codec.Effect's `moveTail` needs to tell a placement from a zone.
toJson :: LibraryPlacement.LibraryPlacement -> Value.Value
toJson p = case p of
  LibraryPlacement.Stated position -> LibraryPosition.toJson position
  LibraryPlacement.OwnerChooses -> Common.nullary "OwnerChooses"

fromJson :: Value.Value -> Either Text.Text LibraryPlacement.LibraryPlacement
fromJson value = case LibraryPosition.fromJson value of
  Right position -> Right (LibraryPlacement.Stated position)
  Left _ ->
    Common.decodeNullary
      "LibraryPlacement"
      [("OwnerChooses", LibraryPlacement.OwnerChooses)]
      value
