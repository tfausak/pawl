module Pawl.Codec.Clause where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Codec.UnlessPaid as UnlessPaid
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Optionality as Optionality

toJson :: (Eq card) => (card -> Value.Value) -> Clause.Clause card -> Value.Value
toJson codec c =
  Common.object . concat $
    [ -- R2 again: CR 701.46a's "if" is the marked case, and stating no gate is
      -- what every other card in the corpus does.
      Common.optionalPair "condition" Nothing (Common.encodeMaybe Condition.toJson) (Clause.condition c),
      Common.optionalPair "effects" Seq.empty (Common.encodeSeq (Effect.toJson codec)) (Clause.effects c),
      -- R2 of the omit-defaults design: Mandatory is the absence of a rider
      -- (CR 603.5's "may" is the marked case).
      Common.optionalPair "optionality" Optionality.Mandatory Optionality.toJson (Clause.optionality c),
      -- R2 again: CR 118.12a's "unless" is the marked case, and stating no such
      -- cost is what every other card in the corpus does.
      Common.optionalPair "unlessPaid" Nothing (Common.encodeMaybe UnlessPaid.toJson) (Clause.unlessPaid c)
    ]

fromJson :: (Value.Value -> Either Text.Text card) -> Value.Value -> Either Text.Text (Clause.Clause card)
fromJson decode value = do
  ps <- Common.asObject value
  c <- Common.defaultedField "condition" Nothing (Common.decodeMaybe Condition.fromJson) ps
  es <- Common.defaultedField "effects" Seq.empty (Common.decodeSeq (Effect.fromJson decode)) ps
  o <- Common.defaultedField "optionality" Optionality.Mandatory Optionality.fromJson ps
  u <- Common.defaultedField "unlessPaid" Nothing (Common.decodeMaybe UnlessPaid.fromJson) ps
  pure (Clause.MkClause c o u es)
