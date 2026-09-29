{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamageIs where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamageIs as DamageIs

codec :: Codec.Codec DamageIs.DamageIs
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec DamageIs.object
  damage <- Fields.required "damage" Common.natural DamageIs.damage
  pure DamageIs.MkDamageIs {DamageIs.object = object, DamageIs.damage = damage}
