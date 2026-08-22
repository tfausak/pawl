{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryRiders where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterKind as CounterKind.Type
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.TapState as TapState

-- | CR 110.5b's default, which is what a Create or a MoveToZone that says
-- nothing about tapped-ness means.
defaultTapped :: TapState.TapState
defaultTapped = TapState.Untapped

-- | One counter kind and the count an object enters with, which is a Quantity
-- rather than a number (CR 122.6, CR 107.3c): Printlifter Ooze's "X +1/+1
-- counters on it, where X is the number of other creatures you control".
--
-- A PAIR PER KIND, where this used to be 'Common.multiset' -- a plain array with
-- repeats, which can spell a literal count and nothing else.
counter :: Codec.Codec (CounterKind.Type.CounterKind Keyword.Type.Keyword, Quantity.Type.Quantity)
counter = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) fst
  count <- Fields.required "count" Quantity.codec snd
  pure (kind, count)

-- | Every field is defaulted, so riders equal to 'defaultValue' write the empty
-- object -- and their own key is then elided by whichever effect carries them.
codec :: Codec.Codec (EntryRiders.EntryRiders Quantity.Type.Quantity)
codec = Fields.object $ do
  tapped <- Fields.defaulted "tapped" defaultTapped TapState.codec EntryRiders.tapped
  attacking <- Fields.defaulted "attacking" False Common.boolean EntryRiders.attacking
  blocking <- Fields.defaulted "blocking" Nothing (Common.maybe SlotName.codec) EntryRiders.blocking
  transformed <- Fields.defaulted "transformed" False Common.boolean EntryRiders.transformed
  counters <- Fields.defaulted "counters" Map.empty (Common.keyedList counter) EntryRiders.counters
  underOwner <- Fields.defaulted "underOwner" False Common.boolean EntryRiders.underOwner
  exiledFaceDown <- Fields.defaulted "exiledFaceDown" False Common.boolean EntryRiders.exiledFaceDown
  faceDown <- Fields.defaulted "faceDown" False Common.boolean EntryRiders.faceDown
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = tapped,
        EntryRiders.attacking = attacking,
        EntryRiders.blocking = blocking,
        EntryRiders.transformed = transformed,
        EntryRiders.counters = counters,
        EntryRiders.underOwner = underOwner,
        EntryRiders.exiledFaceDown = exiledFaceDown,
        EntryRiders.faceDown = faceDown
      }

-- | The value every carrier elides: a card file carries riders only when the
-- effect really does say otherwise (CR 110.5b for tapped, CR 508.4 for a
-- creature put onto the battlefield attacking, CR 509.4 for one put onto the
-- battlefield blocking, CR 712.14 for the front face a
-- double-faced card enters showing by default, CR 122.6 for the counters an
-- object enters with, CR 110.2a for who it enters under, CR 406.3 for an exiled
-- card being kept face up, CR 110.5b for a permanent entering face up).
defaultValue :: EntryRiders.EntryRiders count
defaultValue =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = defaultTapped,
      EntryRiders.attacking = False,
      EntryRiders.blocking = Nothing,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = False,
      EntryRiders.faceDown = False
    }
