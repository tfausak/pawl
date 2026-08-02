module Pawl.Codec.EntryRiders where

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

toJson :: EntryRiders.EntryRiders -> Value.Value
toJson e =
  Common.object
    [ Common.pair "tapped" . TapState.toJson $ EntryRiders.tapped e,
      Common.pair "attacking" . Common.boolean $ EntryRiders.attacking e
    ]

fromJson :: Value.Value -> Either Text.Text EntryRiders.EntryRiders
fromJson value = do
  ps <- Common.asObject value
  t <- Common.decodeMaybe TapState.fromJson (Common.nullableField "tapped" ps)
  a <- Common.decodeBooleanDefault False (Common.nullableField "attacking" ps)
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = Maybe.fromMaybe TapState.Untapped t,
        EntryRiders.attacking = a
      }

-- | CR 110.5b: "permanents enter the battlefield untapped ... unless a spell or
-- ability says otherwise", and a creature is attacking only if something says it
-- is (CR 506.3). So this is what a Create or a MoveToZone that says nothing
-- extra means, and the value the encoding ELIDES: a card file carries riders
-- only when the effect really does say otherwise, which is what keeps every card
-- file written before them byte-identical.
defaultValue :: EntryRiders.EntryRiders
defaultValue =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = False
    }
