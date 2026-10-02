{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackPermission where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackPermission as AttackPermission

-- | An object with one named key, Pawl.Codec.CrewRestriction's shape and for its
-- reason: the type is a newtype over one field.
codec :: Codec.Codec AttackPermission.AttackPermission
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec AttackPermission.affected
  pure AttackPermission.MkAttackPermission {AttackPermission.affected = affected}
