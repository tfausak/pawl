{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.EntryRidersSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRiders" $ do
  Spec.it s "MkEntryRiders, tapped and attacking" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False}
      """ {"tapped":{"type":"Tapped"},"attacking":true} """
  -- CR 712.14a's rider, which no other rider implies: a card returned
  -- transformed is not tapped and not attacking by that fact.
  Spec.it s "MkEntryRiders, transformed alone" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = True, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False}
      """ {"transformed":true} """
  -- CR 110.5b's default written out means every key elided: an untapped,
  -- non-attacking, untransformed entry is what an EMPTY object means.
  Spec.it s "MkEntryRiders, the CR 110.5b default omits every key" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.defaultValue
      """ {} """
  -- CR 122.6a's counters, as a multiset: the kind appears once per counter, so
  -- two of one kind is that tag twice.
  Spec.it s "MkEntryRiders, the counters an object enters with" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.MinusOneMinusOne, 1)], EntryRiders.underOwner = False}
      """ {"counters":[{"type":"PlusOnePlusOne"},{"type":"PlusOnePlusOne"},{"type":"MinusOneMinusOne"}]} """
  -- CR 110.2a's exception, which is independent of every other rider: undying
  -- returns its bearer under its owner's control and untapped.
  Spec.it s "MkEntryRiders, underOwner alone" $
    Common.assertJsonCodec
      s
      EntryRiders.toJson
      EntryRiders.fromJson
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = True}
      """ {"underOwner":true} """
  Spec.describe s "defaultValue" $ do
    Spec.it s "is untapped, not attacking and not transformed" $
      Spec.assertEq s EntryRiders.defaultValue EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False}
    Spec.it s "a missing tapped key decodes as Untapped" $
      Common.assertFromJson s EntryRiders.fromJson "{\"attacking\":false}" EntryRiders.defaultValue
    Spec.it s "a missing attacking key decodes as False" $
      Common.assertFromJson s EntryRiders.fromJson "{\"tapped\":{\"type\":\"Untapped\"}}" EntryRiders.defaultValue
    -- CR 712.14: the front face is the default, so a card file that says nothing
    -- about transforming is saying the card enters showing its front face.
    Spec.it s "a missing transformed key decodes as False" $
      Common.assertFromJson s EntryRiders.fromJson "{\"tapped\":{\"type\":\"Untapped\"},\"attacking\":false}" EntryRiders.defaultValue
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
