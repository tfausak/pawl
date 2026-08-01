-- | The @EntryRiders ⇆ Json@ codec (#481).
module Pawl.Codec.EntryRiders where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.TapState as TapState
import Pawl.Json.Value (Value (Null))
import qualified Pawl.Types.EntryRiders as EntryRiders
import qualified Pawl.Types.TapState as TapState

entryRidersToJson :: EntryRiders.EntryRiders -> Value
entryRidersToJson e =
  Json.jObject
    [ (Text.pack "tapped", TapState.toJson (EntryRiders.tapped e)),
      (Text.pack "attacking", Json.jBool (EntryRiders.attacking e))
    ]

jsonToEntryRiders :: Value -> Either Text EntryRiders.EntryRiders
jsonToEntryRiders value = do
  ps <- Json.asObject value
  t <- Json.getOpt (Text.pack "tapped") ps `orDefault` (TapState.Untapped, TapState.fromJson)
  a <- Json.jsonToBoolDefault False (Json.getOpt (Text.pack "attacking") ps)
  pure
    EntryRiders.MkEntryRiders
      { EntryRiders.tapped = t,
        EntryRiders.attacking = a
      }
  where
    orDefault v (d, f) = case v of
      Null _ -> Right d
      _ -> f v

-- CR 110.5b: "permanents enter the battlefield untapped ... unless a spell or
-- ability says otherwise", and a creature is attacking only if something says it
-- is (CR 506.3). So this is what a Create or a MoveToZone that says nothing
-- extra means, and the value the encoding ELIDES: a card file carries riders
-- only when the effect really does say otherwise, which is what keeps every card
-- file written before them byte-identical.
defaultEntryRiders :: EntryRiders.EntryRiders
defaultEntryRiders =
  EntryRiders.MkEntryRiders
    { EntryRiders.tapped = TapState.Untapped,
      EntryRiders.attacking = False
    }
