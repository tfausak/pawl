module Pawl.Codec.CastingPermission where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CastingPermission as CastingPermission

toJson :: CastingPermission.CastingPermission -> Value.Value
toJson c = Common.nullary $ case c of
  CastingPermission.CastFromLibraryWhileSearching -> "CastFromLibraryWhileSearching"
  CastingPermission.CastFromGraveyard -> "CastFromGraveyard"

fromJson :: Value.Value -> Either Text.Text CastingPermission.CastingPermission
fromJson =
  Common.decodeNullary
    "CastingPermission"
    [ ("CastFromLibraryWhileSearching", CastingPermission.CastFromLibraryWhileSearching),
      ("CastFromGraveyard", CastingPermission.CastFromGraveyard)
    ]
