{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ClauseSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.UnlessPaid as UnlessPaid

-- | The `card` parameter is instantiated at 'Text.Text' throughout, for
-- 'Pawl.Codec.ModeSpec''s reason: the codec reaches it only through the supplied
-- Effect codec, so any type proves the shape.
cardToJson :: Text.Text -> Value.Value
cardToJson = Value.text

cardFromJson :: Value.Value -> Either Text.Text Text.Text
cardFromJson = Common.asText

toJson :: Clause.Clause Text.Text -> Value.Value
toJson = Clause.toJson cardToJson

fromJson :: Value.Value -> Either Text.Text (Clause.Clause Text.Text)
fromJson = Clause.fromJson cardFromJson

-- One constructor, so five cases: a populated clause, CR 603.5's `optionality`
-- flag when present, CR 118.12a's `unlessPaid` clause when present, CR 701.46a's
-- `condition` gate when present, and every field defaulted at once.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Clause" $ do
  Spec.it s "MkClause with one effect" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))))
      """ {"effects":[{"type":"Attach","value":"target"}]} """
  -- CR 603.5: an Optional clause is what a printed "may" encodes to, and the key
  -- is emitted only for that value.
  Spec.it s "an Optional clause's optionality key is present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Optionality.Optional Nothing Seq.empty)
      """ {"optionality":{"type":"Optional"}} """
  -- CR 118.12a: Mana Leak's "unless its controller pays {3}" is what the
  -- unlessPaid key encodes, and it is emitted only when there is one.
  Spec.it s "a clause carrying an unless-paid cost writes the key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Clause.MkClause
          Nothing
          Optionality.Mandatory
          ( Just
              UnlessPaid.MkUnlessPaid
                { UnlessPaid.payer = SlotName.MkSlotName (Text.pack "spell"),
                  UnlessPaid.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []}
                }
          )
          (Seq.singleton (Effect.Counter (SlotName.MkSlotName (Text.pack "spell"))))
      )
      """ {"effects":[{"type":"Counter","value":"spell"}],"unlessPaid":{"payer":"spell","cost":{"mana":[{"type":"Generic","value":3}]}}} """
  -- CR 701.46a: adapt's "if this permanent has no +1/+1 counters on it".
  Spec.it s "a clause carrying a condition writes the key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Clause.MkClause
          (Just (Condition.Compares (Quantity.ObjectCounters CounterKind.PlusOnePlusOne) Comparison.Exactly (Quantity.Literal 0)))
          Optionality.Mandatory
          Nothing
          Seq.empty
      )
      """ {"condition":{"measured":{"type":"ObjectCounters","value":{"type":"PlusOnePlusOne"}},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}}} """
  Spec.it s "an empty clause omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Optionality.Mandatory Nothing Seq.empty)
      """ {} """
