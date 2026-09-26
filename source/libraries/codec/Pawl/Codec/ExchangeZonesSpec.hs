module Pawl.Codec.ExchangeZonesSpec where

import qualified Pawl.Codec.ExchangeZones as ExchangeZones
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExchangeZones as ExchangeZones
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ZonePair as ZonePair

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExchangeZones" $ do
  Spec.it s "MkExchangeZones" $
    Common.assertCodec
      s
      ExchangeZones.codec
      ( ExchangeZones.MkExchangeZones
          { ExchangeZones.player = PlayerRef.Relative PlayerRelation.You,
            ExchangeZones.zones = ZonePair.GraveyardAndLibrary
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"zones\":{\"type\":\"GraveyardAndLibrary\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ExchangeZones.codec
