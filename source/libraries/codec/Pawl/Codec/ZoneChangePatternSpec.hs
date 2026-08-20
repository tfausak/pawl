module Pawl.Codec.ZoneChangePatternSpec where

import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ZoneChangePattern" $ do
  -- Rest in Peace: any object that would be put into a graveyard from
  -- anywhere. Both narrowing fields are at their defaults, so neither reaches
  -- the wire.
  Spec.it s "MkZoneChangePattern" $
    Common.assertCodec
      s
      ZoneChangePattern.codec
      ZoneChangePattern.MkZoneChangePattern
        { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
          ZoneChangePattern.whatObject = Filter.And [],
          ZoneChangePattern.whoseObject = ControllerRelation.Anyones
        }
      " {\"whenDestination\":{\"type\":\"Graveyard\"}} "
  -- CR 614.1a: Anafenza, the Foremost's "a nontoken creature an opponent owns
  -- would die". The characteristic filter is what distinguishes this from the
  -- shape above, so it has to survive the wire.
  Spec.it s "MkZoneChangePattern (Anafenza, a nontoken creature)" $
    Common.assertCodec
      s
      ZoneChangePattern.codec
      ZoneChangePattern.MkZoneChangePattern
        { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
          ZoneChangePattern.whatObject = Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsToken],
          ZoneChangePattern.whoseObject = ControllerRelation.Opponents
        }
      " {\"whatObject\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsToken\"}}]},\"whenDestination\":{\"type\":\"Graveyard\"},\"whoseObject\":{\"type\":\"Opponents\"}} "
  -- CR 702.34a's "instead of putting it anywhere else": no destination named,
  -- which is the default, so the whole pattern is the empty object.
  Spec.it s "MkZoneChangePattern (flashback, any destination)" $
    Common.assertCodec
      s
      ZoneChangePattern.codec
      ZoneChangePattern.MkZoneChangePattern
        { ZoneChangePattern.whenDestination = Nothing,
          ZoneChangePattern.whatObject = Filter.IsSource,
          ZoneChangePattern.whoseObject = ControllerRelation.Anyones
        }
      " {\"whatObject\":{\"type\":\"IsSource\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ZoneChangePattern.codec
