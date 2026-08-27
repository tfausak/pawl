{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ModifyTarget where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Modification as Modification
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ModifyTarget as ModifyTarget

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's ModifyTarget arm.
--
-- Takes the GRANTED ABILITY's codec, for Pawl.Codec.Modification's reason:
-- naming Pawl.Codec.GrantedAbility here would close the module cycle the type's
-- own parameter opens.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (ModifyTarget.ModifyTarget ability)
codec abilityCodec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ModifyTarget.duration
  modification <- Fields.required "modification" (Modification.codec abilityCodec) ModifyTarget.modification
  ref <- Fields.required "ref" ObjectRef.codec ModifyTarget.ref
  pure
    ModifyTarget.MkModifyTarget
      { ModifyTarget.duration = duration,
        ModifyTarget.modification = modification,
        ModifyTarget.ref = ref
      }
