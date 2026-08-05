module Pawl.Codec.Mode where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Codec.TargetSpec as TargetSpec
import qualified Pawl.Codec.UnlessPaid as UnlessPaid
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.Optionality as Optionality

toJson :: (Eq card) => (card -> Value.Value) -> Mode.Mode card -> Value.Value
toJson codec m =
  Common.object . concat $
    [ Common.optionalPair "effects" Seq.empty (Common.encodeSeq (Effect.toJson codec)) (Mode.effects m),
      Common.optionalPair "targetSpecs" Map.empty TargetSpec.toJsonMap (Mode.targetSpecs m),
      -- R2 of the omit-defaults design: Mandatory is the absence of a rider
      -- (CR 603.5's "may" is the marked case).
      Common.optionalPair "optionality" Optionality.Mandatory Optionality.toJson (Mode.optionality m),
      -- R2 again: CR 118.12a's "unless" is the marked case, and stating no such
      -- cost is what every other card in the corpus does.
      Common.optionalPair "unlessPaid" Nothing (Common.encodeMaybe UnlessPaid.toJson) (Mode.unlessPaid m)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Mode.Mode card)
fromJson decode value = do
  ps <- Common.asObject value
  es <- Common.defaultedField "effects" Seq.empty (Common.decodeSeq (Effect.fromJson decode)) ps
  ts <- Common.defaultedField "targetSpecs" Map.empty TargetSpec.fromJsonMap ps
  o <- Common.defaultedField "optionality" Optionality.Mandatory Optionality.fromJson ps
  u <- Common.defaultedField "unlessPaid" Nothing (Common.decodeMaybe UnlessPaid.fromJson) ps
  pure (Mode.MkMode es ts o u)
