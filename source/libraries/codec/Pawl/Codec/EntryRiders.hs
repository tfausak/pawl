{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryRiders where

import qualified Data.Map.Strict as Map
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

-- | CR 110.5b's default, which is what a Create or a MoveToZone that says
-- nothing about tapped-ness means.
defaultTapped :: TapState.TapState
defaultTapped = TapState.Untapped

-- | Every field is defaulted, so riders equal to 'defaultValue' write the empty
-- object -- and their own key is then elided by whichever effect carries them.
codec :: Codec.Codec EntryRiders.EntryRiders
codec = Fields.object $ do
  tapped <- Fields.defaulted "tapped" defaultTapped TapState.codec EntryRiders.tapped
  attacking <- Fields.defaulted "attacking" False Common.boolean EntryRiders.attacking
  transformed <- Fields.defaulted "transformed" False Common.boolean EntryRiders.transformed
  counters <- Fields.defaulted "counters" Map.empty (Common.multiset (CounterKind.codec Keyword.codec)) EntryRiders.counters
  underOwner <- Fields.defaulted "underOwner" False Common.boolean EntryRiders.underOwner
  exiledFaceDown <- Fields.defaulted "exiledFaceDown" False Common.boolean EntryRiders.exiledFaceDown
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = tapped,
        EntryRiders.attacking = attacking,
        EntryRiders.transformed = transformed,
        EntryRiders.counters = counters,
        EntryRiders.underOwner = underOwner,
        EntryRiders.exiledFaceDown = exiledFaceDown
      }

-- | The value every carrier elides: a card file carries riders only when the
-- effect really does say otherwise (CR 110.5b for tapped, CR 508.4 for a
-- creature put onto the battlefield attacking, CR 712.14 for the front face a
-- double-faced card enters showing by default, CR 122.6a for the counters an
-- object enters with, CR 110.2a for who it enters under, CR 406.3 for an exiled
-- card being kept face up).
defaultValue :: EntryRiders.EntryRiders
defaultValue =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = defaultTapped,
      EntryRiders.attacking = False,
      EntryRiders.transformed = False,
      EntryRiders.counters = Map.empty,
      EntryRiders.underOwner = False,
      EntryRiders.exiledFaceDown = False
    }
