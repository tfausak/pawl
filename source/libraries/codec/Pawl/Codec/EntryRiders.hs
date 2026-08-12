module Pawl.Codec.EntryRiders where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

-- | CR 110.5b's default, which is what a Create or a MoveToZone that says
-- nothing about tapped-ness means.
defaultTapped :: TapState.TapState
defaultTapped = TapState.Untapped

toJson :: EntryRiders.EntryRiders -> Value.Value
toJson e =
  Value.object
    ( Common.optionalPair "tapped" defaultTapped TapState.toJson (EntryRiders.tapped e)
        <> Common.optionalPair "attacking" False Value.boolean (EntryRiders.attacking e)
        <> Common.optionalPair "transformed" False Value.boolean (EntryRiders.transformed e)
        -- A MULTISET, the shape ProjectedCharacteristics' keywords take: a kind
        -- repeated as many times as there are counters, ascending, so the
        -- encoding is canonical.
        <> Common.optionalPair "counters" Map.empty (Common.encodeMultiset (Codec.encode (CounterKind.codec Keyword.codec))) (EntryRiders.counters e)
        <> Common.optionalPair "underOwner" False Value.boolean (EntryRiders.underOwner e)
    )

fromJson :: Value.Value -> Either Text.Text EntryRiders.EntryRiders
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "tapped" defaultTapped TapState.fromJson ps
  a <- Common.defaultedField "attacking" False Common.asBoolean ps
  f <- Common.defaultedField "transformed" False Common.asBoolean ps
  c <- Common.defaultedField "counters" Map.empty (Common.decodeMultiset (Codec.decode (CounterKind.codec Keyword.codec))) ps
  o <- Common.defaultedField "underOwner" False Common.asBoolean ps
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = t,
        EntryRiders.attacking = a,
        EntryRiders.transformed = f,
        EntryRiders.counters = c,
        EntryRiders.underOwner = o
      }

-- | The value 'toJson' elides entirely: a card file carries riders only when
-- the effect really does say otherwise (CR 110.5b for tapped, CR 508.4 for a
-- creature put onto the battlefield attacking, CR 712.14 for the front face a
-- double-faced card enters showing by default, CR 122.6a for the counters an
-- object enters with, CR 110.2a for who it enters under).
defaultValue :: EntryRiders.EntryRiders
defaultValue =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = defaultTapped,
      EntryRiders.attacking = False,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False
    }
