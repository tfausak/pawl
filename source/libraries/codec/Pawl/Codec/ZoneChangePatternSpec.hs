{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ZoneChangePatternSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.Types.ZoneChangeSubject as ZoneChangeSubject

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  -- Rest in Peace: any object that would be put into a graveyard from
  -- anywhere.
  Spec.describe s "Pawl.Codec.ZoneChangePattern" . Spec.it s "MkZoneChangePattern" $
    Common.assertJsonCodec
      s
      ZoneChangePattern.toJson
      ZoneChangePattern.fromJson
      ZoneChangePattern.MkZoneChangePattern
        { ZoneChangePattern.whenDestination = Zone.Graveyard,
          ZoneChangePattern.whichObject = ZoneChangeSubject.AnyObject,
          ZoneChangePattern.whoseObject = ControllerRelation.Anyones
        }
      """ {"whenDestination":{"type":"Graveyard"},"whichObject":{"type":"AnyObject"},"whoseObject":{"type":"Anyones"}} """
