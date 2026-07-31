-- | The @TokenEntry ⇆ Json@ codec (#481).
module Pawl.Codec.TokenEntry where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.TapState (jsonToTapState, tapStateToJson)
import Pawl.Json.Value (Value (Null))
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TokenEntry as TokenEntry

tokenEntryToJson :: TokenEntry.TokenEntry -> Value
tokenEntryToJson e =
  Json.jObject
    [ (Text.pack "tapped", tapStateToJson (TokenEntry.tapped e)),
      (Text.pack "attacking", Json.jBool (TokenEntry.attacking e))
    ]

jsonToTokenEntry :: Value -> Either Text TokenEntry.TokenEntry
jsonToTokenEntry value = do
  ps <- Json.asObject value
  t <- Json.getOpt (Text.pack "tapped") ps `orDefault` (TapState.Untapped, jsonToTapState)
  a <- Json.jsonToBoolDefault False (Json.getOpt (Text.pack "attacking") ps)
  pure
    TokenEntry.MkTokenEntry
      { TokenEntry.tapped = t,
        TokenEntry.attacking = a
      }
  where
    orDefault v (d, f) = case v of
      Null _ -> Right d
      _ -> f v

-- CR 110.5b: "permanents enter the battlefield untapped ... unless a spell or
-- ability says otherwise", and a creature is attacking only if something says it
-- is (CR 506.3). So this is what a Create that says nothing extra means, and the
-- value the encoding ELIDES: a card file carries a TokenEntry only when the
-- effect really does say otherwise, which is what keeps every token-making file
-- written before this one byte-identical.
defaultTokenEntry :: TokenEntry.TokenEntry
defaultTokenEntry =
  TokenEntry.MkTokenEntry
    { TokenEntry.tapped = TapState.Untapped,
      TokenEntry.attacking = False
    }
