module Pawl.Codec.PlotFromZoneSpec where

import qualified Pawl.Codec.PlotFromZone as PlotFromZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlotFromZone as PlotFromZone
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlotFromZone" $ do
  -- Fblthp, Lost on the Range's shape: nonland cards from the top of your library.
  Spec.it s "MkPlotFromZone" $
    Common.assertCodec
      s
      PlotFromZone.codec
      ( PlotFromZone.MkPlotFromZone
          { PlotFromZone.from = InZone.MkInZone {InZone.zone = Zone.Library, InZone.player = PlayerRef.Relative PlayerRelation.You},
            PlotFromZone.matching = Filter.Not (Filter.HasCardType CardType.Land)
          }
      )
      " {\"from\":{\"zone\":{\"type\":\"Library\"},\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"matching\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlotFromZone.codec
