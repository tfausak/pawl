{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EntryRidersSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRiders" $ do
  Spec.it s "MkEntryRiders, tapped and attacking" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True}
      """ {"tapped":{"type":"Tapped"},"attacking":true} """
  Spec.it s "MkEntryRiders, the CR 110.5b default written out explicitly" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.defaultValue
      """ {"tapped":{"type":"Untapped"},"attacking":false} """
  Spec.describe s "defaultValue" $ do
    Spec.it s "is untapped and not attacking" $
      Spec.assertEq s EntryRiders.defaultValue EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False}
    -- The local `orDefault` helper this module used to carry is exactly
    -- Common.decodeMaybe plus a default: an explicit JSON null for "tapped"
    -- decodes as Untapped, the same as the field being absent entirely.
    Spec.it s "an explicit null tapped decodes as Untapped" $
      Common.assertFromJson s EntryRiders.fromJson "{\"tapped\":null,\"attacking\":false}" EntryRiders.defaultValue
    Spec.it s "a missing tapped key decodes as Untapped" $
      Common.assertFromJson s EntryRiders.fromJson "{\"attacking\":false}" EntryRiders.defaultValue
    Spec.it s "a missing attacking key decodes as False" $
      Common.assertFromJson s EntryRiders.fromJson "{\"tapped\":{\"type\":\"Untapped\"}}" EntryRiders.defaultValue
