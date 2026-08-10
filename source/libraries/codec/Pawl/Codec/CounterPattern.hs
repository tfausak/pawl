module Pawl.Codec.CounterPattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterPattern as CounterPattern

-- | CR 109.5 reads a controller relation against the effect's source; "anyone's"
-- is the unrestricted reading, so it is what a pattern that says nothing means.
defaultWhose :: ControllerRelation.ControllerRelation
defaultWhose = ControllerRelation.Anyones

toJson :: CounterPattern.CounterPattern -> Value.Value
toJson p =
  Common.object
    ( Common.optionalPair "whichKind" Nothing (Common.encodeMaybe (CounterKind.toJson Keyword.toJson)) (CounterPattern.whichKind p)
        <> Common.optionalPair "whose" defaultWhose ControllerRelation.toJson (CounterPattern.whose p)
        <> Common.requiredPair "onWhat" (Filter.toJson Keyword.toJson) (CounterPattern.onWhat p)
    )

fromJson :: Value.Value -> Either Text.Text CounterPattern.CounterPattern
fromJson value = do
  ps <- Common.asObject value
  k <- Common.defaultedField "whichKind" Nothing (Common.decodeMaybe (CounterKind.fromJson Keyword.fromJson)) ps
  w <- Common.defaultedField "whose" defaultWhose ControllerRelation.fromJson ps
  o <- Common.field "onWhat" ps >>= Filter.fromJson Keyword.fromJson
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }
