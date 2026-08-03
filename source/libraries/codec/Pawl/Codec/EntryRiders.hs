module Pawl.Codec.EntryRiders where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

-- | CR 110.5b: "permanents enter the battlefield untapped ... unless a spell or
-- ability says otherwise", so this is what a Create or a MoveToZone that says
-- nothing about tapped-ness means.
defaultTapped :: TapState.TapState
defaultTapped = TapState.Untapped

toJson :: EntryRiders.EntryRiders -> Value.Value
toJson e =
  Common.object
    ( Common.optionalPair "tapped" defaultTapped TapState.toJson (EntryRiders.tapped e)
        <> Common.optionalPair "attacking" False Common.boolean (EntryRiders.attacking e)
    )

fromJson :: Value.Value -> Either Text.Text EntryRiders.EntryRiders
fromJson value = do
  ps <- Common.asObject value
  t <- Common.defaultedField "tapped" defaultTapped TapState.fromJson ps
  a <- Common.defaultedField "attacking" False Common.asBoolean ps
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = t,
        EntryRiders.attacking = a
      }

-- | The value 'toJson' elides entirely: a card file carries riders only when
-- the effect really does say otherwise (CR 110.5b for tapped, CR 506.3 for a
-- creature being attacking), which is what keeps every card file written
-- before them byte-identical.
defaultValue :: EntryRiders.EntryRiders
defaultValue =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = defaultTapped,
      EntryRiders.attacking = False
    }
