{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MoveToZone where

import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.LibraryPlacement as LibraryPlacement
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.MoveToZone as MoveToZone

-- | Four independently elided keys, each omitted when it is its default: the
-- riders (CR 110.5b), the bound slot, CR 113.6m's origin zone and CR 401.2's
-- library placement.
--
-- This is what retires @moveTail@. That function read a VARIABLE-LENGTH array
-- tail and recovered which element was which from each one's JSON type, in a
-- fixed order -- zone and placement first, because 'EntryRiders' would have
-- accepted either tagged object and silently returned the defaults. Named keys
-- need none of it: absence is absence, and two objects that are both objects no
-- longer have to be told apart.
codec :: Codec.Codec MoveToZone.MoveToZone
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec MoveToZone.ref
  zone <- Fields.required "zone" Zone.codec MoveToZone.zone
  riders <- Fields.defaulted "riders" EntryRiders.defaultValue EntryRiders.codec MoveToZone.riders
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) MoveToZone.slot
  origin <- Fields.defaulted "origin" Nothing (Common.maybe Zone.codec) MoveToZone.origin
  placement <- Fields.defaulted "placement" LibraryPlacement.defaultValue LibraryPlacement.codec MoveToZone.placement
  pure
    MoveToZone.MkMoveToZone
      { MoveToZone.ref = ref,
        MoveToZone.zone = zone,
        MoveToZone.riders = riders,
        MoveToZone.slot = slot,
        MoveToZone.origin = origin,
        MoveToZone.placement = placement
      }
