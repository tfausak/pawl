module Pawl.Codec.ZoneChangeRSpec where

import qualified Pawl.Codec.ZoneChangeR as ZoneChangeR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.PlayerRelation as PlayerRelation
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
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Exile,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False
          }
      )
      " {\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"}},\"destination\":{\"type\":\"Exile\"}} "
  -- CR 701.20 / 701.24: Nexus of Fate's shape, where both riders are written
  -- out rather than defaulted away.
  Spec.it s "MkZoneChangeR with both riders" $
    Common.assertCodec
      s
      ZoneChangeR.codec
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.IsSource,
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.AnyPlayer,
                  ZoneChangePattern.whenDiscarded = Nothing,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Library,
            ZoneChangeR.revealing = True,
            ZoneChangeR.shuffling = True,
            ZoneChangeR.position = LibraryPosition.defaultValue,
            ZoneChangeR.optional = False
          }
      )
      " {\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"},\"whatObject\":{\"type\":\"IsSource\"}},\"destination\":{\"type\":\"Library\"},\"revealing\":true,\"shuffling\":true} "
  -- CR 614.1a / 401.2: Library of Leng's optional redirect of an effect's
  -- discard to the top of the library, every new field off its default.
  Spec.it s "MkZoneChangeR (Library of Leng)" $
    Common.assertCodec
      s
      ZoneChangeR.codec
      ( ZoneChangeR.MkZoneChangeR
          { ZoneChangeR.matching =
              ZoneChangePattern.MkZoneChangePattern
                { ZoneChangePattern.whenDestination = Just Zone.Graveyard,
                  ZoneChangePattern.whatObject = Filter.And [],
                  ZoneChangePattern.whoseObject = ControllerRelation.Related PlayerRelation.You,
                  ZoneChangePattern.whenDiscarded = Just DiscardCause.ByEffect,
                  ZoneChangePattern.duringResolution = False
                },
            ZoneChangeR.destination = Zone.Library,
            ZoneChangeR.revealing = False,
            ZoneChangeR.shuffling = False,
            ZoneChangeR.position = LibraryPosition.Top,
            ZoneChangeR.optional = True
          }
      )
      " {\"matching\":{\"whenDestination\":{\"type\":\"Graveyard\"},\"whoseObject\":{\"type\":\"You\"},\"whenDiscarded\":{\"type\":\"ByEffect\"}},\"destination\":{\"type\":\"Library\"},\"position\":{\"type\":\"Top\"},\"optional\":true} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ZoneChangeR.codec
