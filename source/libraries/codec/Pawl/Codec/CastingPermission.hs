-- | The @CastingPermission ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.CastingPermission where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CastingPermission as CastingPermission

castingPermissionToJson :: CastingPermission.CastingPermission -> Value
castingPermissionToJson c = Json.nullary . Text.pack $ case c of
  CastingPermission.CastFromLibraryWhileSearching -> "CastFromLibraryWhileSearching"
  CastingPermission.CastFromGraveyard -> "CastFromGraveyard"

jsonToCastingPermission :: Value -> Either Text CastingPermission.CastingPermission
jsonToCastingPermission =
  Json.decodeNullary
    (Text.pack "CastingPermission")
    [ (Text.pack "CastFromLibraryWhileSearching", CastingPermission.CastFromLibraryWhileSearching),
      (Text.pack "CastFromGraveyard", CastingPermission.CastFromGraveyard)
    ]
