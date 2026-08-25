module Pawl.Codec.EntryRidersSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason
import qualified Pawl.Types.FaceDownState as FaceDownState
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TypeLine as TypeLine

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.EntryRiders" $ do
  Spec.it s "MkEntryRiders, tapped and attacking" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Tapped, EntryRiders.attacking = True, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"tapped\":{\"type\":\"Tapped\"},\"attacking\":true} "
  -- CR 509.4's rider, which is a SLOT and not a flag: the effect specifies which
  -- attacking creature the entering creature blocks (Flash Foliage's target),
  -- and it implies nothing about tapped-ness -- CR 509.4b exempts the creature
  -- from CR 509.1a's untapped condition either way.
  Spec.it s "MkEntryRiders, blocking names the attacker's slot" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Just (SlotName.MkSlotName (Text.pack "target")), EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"blocking\":\"target\"} "
  -- CR 712.14a's rider, which no other rider implies: a card returned
  -- transformed is not tapped and not attacking by that fact.
  Spec.it s "MkEntryRiders, transformed alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = True, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"transformed\":true} "
  -- CR 110.5b's default written out means every key elided: an untapped,
  -- non-attacking, untransformed entry is what an EMPTY object means.
  Spec.it s "MkEntryRiders, the CR 110.5b default omits every key" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.defaultValue
      " {} "
  -- CR 122.6's counters: one entry per KIND, carrying that kind's count, where
  -- this used to be a multiset spelling the count as repeats.
  Spec.it s "MkEntryRiders, the counters an object enters with" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.fromList [(CounterKind.PlusOnePlusOne, Quantity.Literal 2), (CounterKind.MinusOneMinusOne, Quantity.Literal 1)], EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}},{\"kind\":{\"type\":\"MinusOneMinusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}}]} "
  -- CR 107.3c: the count need not be a literal at all -- Printlifter Ooze's X,
  -- defined by the ability's own text, which is why this field is a Quantity.
  Spec.it s "MkEntryRiders, a counter count that is not a literal" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.singleton CounterKind.PlusOnePlusOne Quantity.Power, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Power\"}}]} "
  -- A repeated kind is rejected rather than combined, which the multiset could
  -- not do: there a repeat was how a count was written.
  Spec.it s "MkEntryRiders, a repeated counter kind is a decode error" $
    Spec.assertBool
      s
      ( case Codec.decode EntryRiders.codec =<< Common.parse (Text.pack "{\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}},{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":2}}]}") of
          Left _ -> True
          Right _ -> False
      )
      "expected a decode failure for a repeated counter kind"
  -- CR 110.2a's exception, which is independent of every other rider: undying
  -- returns its bearer under its owner's control and untapped.
  Spec.it s "MkEntryRiders, underOwner alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = True, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
      " {\"underOwner\":true} "
  -- CR 406.3's rider, which is the one rider about a zone that is not the
  -- battlefield: Ignorant Bliss exiles face down and says nothing else.
  Spec.it s "MkEntryRiders, exiledFaceDown alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = True, EntryRiders.faceDown = Nothing}
      " {\"exiledFaceDown\":true} "
  -- CR 708.3's rider, and the one above it are two different keys because they
  -- are two different rules (CR 110.5d): Soul Summons manifests and says nothing
  -- else, so `exiledFaceDown` stays absent alongside it.
  Spec.it s "MkEntryRiders, faceDown alone" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Just FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.Manifested, FaceDownState.listed = FaceDownCharacteristics.defaultValue}}
      " {\"faceDown\":{\"reason\":{\"type\":\"Manifested\"},\"listed\":{}}} "
  -- CR 708.2a's "unless otherwise specified": Yedora, Grave Gardener's "It's a
  -- Forest land", whose listing has no power or toughness (CR 208.3) and whose
  -- reason is CR 708.3's rather than manifest's.
  Spec.it s "MkEntryRiders, faceDown with a listing of its own" $
    Common.assertCodec
      s
      EntryRiders.codec
      EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Just FaceDownState.MkFaceDownState {FaceDownState.reason = FaceDownReason.EnteredFaceDown, FaceDownState.listed = FaceDownCharacteristics.defaultValue {FaceDownCharacteristics.typeLine = TypeLine.MkTypeLine {TypeLine.supertypes = Set.empty, TypeLine.types = Set.singleton CardType.Land, TypeLine.subtypes = Set.singleton Subtype.Forest}, FaceDownCharacteristics.power = Nothing, FaceDownCharacteristics.toughness = Nothing}}}
      " {\"faceDown\":{\"reason\":{\"type\":\"EnteredFaceDown\"},\"listed\":{\"power\":null,\"toughness\":null,\"typeLine\":{\"subtypes\":[{\"type\":\"Forest\"}],\"types\":[{\"type\":\"Land\"}]}}}} "
  Spec.describe s "defaultValue" $ do
    Spec.it s "is untapped, not attacking and not transformed" $
      Spec.assertEq s (EntryRiders.defaultValue :: EntryRiders.EntryRiders Quantity.Quantity) EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.blocking = Nothing, EntryRiders.transformed = False, EntryRiders.counters = Map.empty, EntryRiders.underOwner = False, EntryRiders.exiledFaceDown = False, EntryRiders.faceDown = Nothing}
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
