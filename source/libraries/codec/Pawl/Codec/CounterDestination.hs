{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CounterDestination where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LibraryPosition as LibraryPosition
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterDestination as CounterDestination
import qualified Pawl.Types.LibraryPosition as LibraryPositionType

-- | The zone is required; the rest are elided at their defaults, so Remand's
-- hand writes only the @zone@ key.
codec :: Codec.Codec CounterDestination.CounterDestination
codec = Fields.object $ do
  zone <- Fields.required "zone" Zone.codec CounterDestination.zone
  position <- Fields.defaulted "position" LibraryPositionType.defaultValue LibraryPosition.codec CounterDestination.position
  only <- Fields.defaulted "only" Nothing (Common.maybe (Filter.codec Keyword.codec)) CounterDestination.only
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) CounterDestination.slot
  pure
    CounterDestination.MkCounterDestination
      { CounterDestination.zone = zone,
        CounterDestination.position = position,
        CounterDestination.only = only,
        CounterDestination.slot = slot
      }
