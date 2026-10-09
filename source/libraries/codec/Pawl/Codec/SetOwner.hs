{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SetOwner where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SetOwner as SetOwner

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec SetOwner.SetOwner
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec SetOwner.player
  ref <- Fields.required "ref" ObjectRef.codec SetOwner.ref
  pure
    SetOwner.MkSetOwner
      { SetOwner.player = player,
        SetOwner.ref = ref
      }
