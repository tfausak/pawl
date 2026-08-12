module Pawl.Codec.TokenPattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.TokenPattern as TokenPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

toJson :: TokenPattern.TokenPattern -> Value.Value
toJson p =
  Value.object (Common.optionalPair "whose" defaultWhose (Codec.encode ControllerRelation.codec) (TokenPattern.whose p))

fromJson :: Value.Value -> Either Text.Text TokenPattern.TokenPattern
fromJson value = do
  ps <- Common.asObject value
  w <- Common.defaultedField "whose" defaultWhose (Codec.decode ControllerRelation.codec) ps
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}
