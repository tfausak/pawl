module Pawl.Codec.EntryRidersSpec where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRiders" $ do
  Spec.it s "MkEntryRiders, tapped and attacking" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
      " {\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true} "
  -- CR 712.14a's rider, which no other rider implies: a card returned
  -- transformed is not tapped and not attacking by that fact.
  Spec.it s "MkEntryRiders, transformed alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = True, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
      " {\"transformed\":true} "
  -- CR 110.5b's default written out means every key elided: an untapped,
  -- non-attacking, untransformed entry is what an EMPTY object means.
  Spec.it s "MkEntryRiders, the CR 110.5b default omits every key" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.defaultValue
      " {} "
  -- CR 122.6a's counters, as a multiset: the kind appears once per counter, so
  -- two of one kind is that tag twice.
  Spec.it s "MkEntryRiders, the counters an object enters with" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.fromList [(CounterKind.PlusOnePlusOne, 2), (CounterKind.MinusOneMinusOne, 1)], EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
      " {\"counters\":[{\"type\":\"PlusOnePlusOne\"},{\"type\":\"PlusOnePlusOne\"},{\"type\":\"MinusOneMinusOne\"}]} "
  -- CR 110.2a's exception, which is independent of every other rider: undying
  -- returns its bearer under its owner's control and untapped.
  Spec.it s "MkEntryRiders, underOwner alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = True, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
      " {\"underOwner\":true} "
  -- CR 406.3's rider, which is the one rider about a zone that is not the
  -- battlefield: Ignorant Bliss exiles face down and says nothing else.
  Spec.it s "MkEntryRiders, exiledFaceDown alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = True, EntryRiders.faceDown = False}
      " {\"exiledFaceDown\":true} "
  -- CR 708.3's rider, and the one above it are two different keys because they
  -- are two different rules (CR 110.5d): Soul Summons manifests and says nothing
  -- else, so `exiledFaceDown` stays absent alongside it.
  Spec.it s "MkEntryRiders, faceDown alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = True}
      " {\"faceDown\":true} "
  Spec.describe s "defaultValue" $ do
    Spec.it s "is untapped, not attacking and not transformed" $
      Spec.assertEq s EntryRiders.defaultValue EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = False}
    Spec.it s "a missing tapped key decodes as Untapped" $
      Common.assertFromJson s (Codec.decode EntryRiders.codec) "{\"attacking\":false}" EntryRiders.defaultValue
    Spec.it s "a missing attacking key decodes as False" $
      Common.assertFromJson s (Codec.decode EntryRiders.codec) "{\"tapped\":{\"type\":\"Untapped\"}}" EntryRiders.defaultValue
    -- CR 712.14: the front face is the default, so a card file that says nothing
    -- about transforming is saying the card enters showing its front face.
    Spec.it s "a missing transformed key decodes as False" $
      Common.assertFromJson s (Codec.decode EntryRiders.codec) "{\"tapped\":{\"type\":\"Untapped\"},\"attacking\":false}" EntryRiders.defaultValue
    -- An explicit null is tolerated only for a Maybe field, composed with
    -- Common.maybe. `tapped` isn't one, so a null is a decode error
    -- rather than a second spelling of the default.
    Spec.it s "an explicit null tapped is now a decode error" $
      Spec.assertBool
        s
        ( case Codec.decode EntryRiders.codec (Value.object [Value.pair "tapped" Value.null, Value.pair "attacking" (Value.boolean False)]) of
            Left _ -> True
            Right _ -> False
        )
        "expected a decode failure for an explicit null tapped"
