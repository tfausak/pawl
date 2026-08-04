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
  -- CR 110.5b's default written out means both keys elided: an untapped,
  -- non-attacking entry is what an EMPTY object means.
  Spec.it s "MkEntryRiders, the CR 110.5b default omits both keys" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.defaultValue
      """ {} """
  Spec.describe s "defaultValue" $ do
    Spec.it s "is untapped and not attacking" $
      Spec.assertEq s EntryRiders.defaultValue EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False}
    Spec.it s "a missing tapped key decodes as Untapped" $
      Common.assertFromJson s EntryRiders.fromJson "{\"attacking\":false}" EntryRiders.defaultValue
    Spec.it s "a missing attacking key decodes as False" $
      Common.assertFromJson s EntryRiders.fromJson "{\"tapped\":{\"type\":\"Untapped\"}}" EntryRiders.defaultValue
    -- An explicit null is tolerated only for a Maybe field, composed with
    -- Common.decodeMaybe. `tapped` isn't one, so a null is a decode error
    -- rather than a second spelling of the default.
    Spec.it s "an explicit null tapped is now a decode error" $
      Spec.assertBool
        s
        ( case EntryRiders.fromJson (Common.object [Common.pair "tapped" Common.null, Common.pair "attacking" (Common.boolean False)]) of
            Left _ -> True
            Right _ -> False
        )
        "expected a decode failure for an explicit null tapped"
