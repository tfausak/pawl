{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CastingPermissionSpec where

import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastingPermission as CastingPermission

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastingPermission" $ do
  Spec.it s "CastFromLibraryWhileSearching" $
    Common.assertJsonCodec
      s
      CastingPermission.toJson
      CastingPermission.fromJson
      CastingPermission.CastFromLibraryWhileSearching
      """ {"type":"CastFromLibraryWhileSearching"} """
  Spec.it s "CastFromGraveyard" $
    Common.assertJsonCodec
      s
      CastingPermission.toJson
      CastingPermission.fromJson
      CastingPermission.CastFromGraveyard
      """ {"type":"CastFromGraveyard"} """
