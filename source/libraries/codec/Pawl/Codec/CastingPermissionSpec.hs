{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CastingPermissionSpec where

import qualified Pawl.Codec.CastingPermission as CastingPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastingPermission as CastingPermission

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastingPermission" $ do
  Spec.it s "CastFromLibraryWhileSearching" $
    Common.assertCodec
      s
      CastingPermission.codec
      CastingPermission.CastFromLibraryWhileSearching
      """ {"type":"CastFromLibraryWhileSearching"} """
  Spec.it s "CastFromGraveyard" $
    Common.assertCodec
      s
      CastingPermission.codec
      CastingPermission.CastFromGraveyard
      """ {"type":"CastFromGraveyard"} """
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s CastingPermission.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastingPermission.codec
