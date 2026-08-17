module Pawl.Codec.ZoneChangeRSpec where

import qualified Pawl.Codec.ZoneChangeR as ZoneChangeR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ZoneChangeR" $ do
  -- CR 614.1a: Rest in Peace exiles what would go to a graveyard.
  Spec.it s "MkZoneChangeR" $
    Common.assertCodec
      s
      ZoneChangeR.codec
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Anyones
                },
            ZoneChangeR.destination = Zone.Exile
          }
      )
      " {\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ZoneChangeR.codec
