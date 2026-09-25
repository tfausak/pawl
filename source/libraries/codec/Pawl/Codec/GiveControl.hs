{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GiveControl where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GiveControl as GiveControl

codec :: Codec.Codec GiveControl.GiveControl
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec GiveControl.player
  ref <- Fields.required "ref" ObjectRef.codec GiveControl.ref
  pure
    GiveControl.MkGiveControl
      { GiveControl.player = player,
        GiveControl.ref = ref
      }
