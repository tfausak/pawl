{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SacrificeEffect where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Sacrificer as Sacrificer
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SacrificeEffect as SacrificeEffect
import qualified Pawl.Types.Sacrificer as Sacrificer

-- | The sacrificer is ELIDED when it is CR 701.21a's ordinary "sacrifice it",
-- which is every card in the pool but Golgothian Sylex; the other arm is
-- otherwise written only by the emblem Pawl.Engine.Ring mints, and that is not
-- card data.
codec :: Codec.Codec SacrificeEffect.SacrificeEffect
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec SacrificeEffect.ref
  sacrificer <- Fields.defaulted "sacrificer" Sacrificer.EffectController Sacrificer.codec SacrificeEffect.sacrificer
  pure
    SacrificeEffect.MkSacrificeEffect
      { SacrificeEffect.ref = ref,
        SacrificeEffect.sacrificer = sacrificer
      }
