module Pawl.Codec.InZoneSpec where

import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.InZone" $ do
  -- CR 400.1: the battlefield is shared, so EachPlayer is what most counts say.
  Spec.it s "MkInZone, both keys" $
    Common.assertCodec
      s
      InZone.codec
      (InZone.MkInZone {InZone.zone = Zone.Battlefield, InZone.player = PlayerRef.EachPlayer})
      " {\"zone\":{\"type\":\"Battlefield\"},\"player\":{\"type\":\"EachPlayer\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s InZone.codec
