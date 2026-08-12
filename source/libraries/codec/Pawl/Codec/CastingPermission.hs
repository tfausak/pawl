module Pawl.Codec.CastingPermission where

import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CastingPermission as CastingPermission

codec :: Codec.Codec CastingPermission.CastingPermission
codec =
  Arm.tagged
    encode
    [ Arm.nullary "CastFromLibraryWhileSearching" CastingPermission.CastFromLibraryWhileSearching,
      Arm.nullary "CastFromGraveyard" CastingPermission.CastFromGraveyard
    ]
  where
    encode c = Common.nullary $ case c of
      CastingPermission.CastFromLibraryWhileSearching -> "CastFromLibraryWhileSearching"
      CastingPermission.CastFromGraveyard -> "CastFromGraveyard"
