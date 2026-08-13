{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MoveToZoneSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Codec.MoveToZone as MoveToZone
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import qualified Pawl.Types.MoveToZone as MoveToZone
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

target :: ObjectRef.ObjectRef
target = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))

-- Every optional field at its default, so only the two required keys are
-- written. Unsummon's shape.
bare :: MoveToZone.MoveToZone
bare =
  MoveToZone.MkMoveToZone
    { MoveToZone.ref = target,
      MoveToZone.zone = Zone.Hand,
      MoveToZone.riders = EntryRiders.defaultValue,
      MoveToZone.slot = Nothing,
      MoveToZone.origin = Nothing,
      MoveToZone.placement = LibraryPlacement.defaultValue
    }

tapped :: EntryRiders.EntryRiders
tapped = EntryRiders.defaultValue {EntryRiders.tapped = TapState.Tapped}

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MoveToZone" $ do
  Spec.it s "every optional key elided" $
    Common.assertCodec
      s
      MoveToZone.codec
      bare
      """ {"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Hand"}} """
  -- The case @moveTail@ existed for, and the one it could only just handle: an
  -- origin zone and the riders are BOTH objects, so the positional tail had to
  -- try the zone codec first and fall back to riders, because EntryRiders would
  -- have accepted either and silently returned the defaults. As named keys
  -- neither can be taken for the other, in any order.
  Spec.it s "riders and an origin zone together, which moveTail had to order" $
    Common.assertCodec
      s
      MoveToZone.codec
      bare
        { MoveToZone.zone = Zone.Battlefield,
          MoveToZone.riders = tapped,
          MoveToZone.origin = Just Zone.Graveyard
        }
      """ {"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Battlefield"},"riders":{"tapped":{"type":"Tapped"}},"origin":{"type":"Graveyard"}} """
  -- All four optional keys at once, which no card in the corpus writes: the
  -- longest form moveTail accepted was six elements and it had to reject a
  -- repeat of any of them by hand. Griptide's Top rather than Bottom, since
  -- Bottom is LibraryPosition's default and would elide the key.
  Spec.it s "all four optional keys at once" $
    Common.assertCodec
      s
      MoveToZone.codec
      bare
        { MoveToZone.zone = Zone.Library,
          MoveToZone.riders = tapped,
          MoveToZone.slot = Just (SlotName.MkSlotName (Text.pack "moved")),
          MoveToZone.origin = Just Zone.Exile,
          MoveToZone.placement = LibraryPlacement.Stated LibraryPosition.Top
        }
      """ {"ref":{"type":"InSlot","value":"target"},"zone":{"type":"Library"},"riders":{"tapped":{"type":"Tapped"}},"slot":"moved","origin":{"type":"Exile"},"placement":{"type":"Stated","value":{"type":"Top"}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s MoveToZone.codec
