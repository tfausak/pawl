{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.BlockPermissionSpec where

import qualified Pawl.Codec.BlockPermission as BlockPermission
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.BlockPermission as BlockPermission

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Echo Circlet's shape (CR 303.4m): the equipped creature is the one that may
  -- block one creature more than CR 509.1a allows.
  Spec.describe s "Pawl.Codec.BlockPermission" . Spec.it s "MkBlockPermission" $
    Common.assertJsonCodec
      s
      BlockPermission.toJson
      BlockPermission.fromJson
      (BlockPermission.MkBlockPermission Affected.Attached 1)
      """ {"affected":{"type":"Attached"},"additional":1} """
