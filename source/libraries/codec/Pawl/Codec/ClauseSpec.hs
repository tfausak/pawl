module Pawl.Codec.ClauseSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.Clause as Clause
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The `card` parameter is instantiated at 'Text.Text' throughout, for
-- 'Pawl.Codec.ModeSpec''s reason: the codec reaches it only through the supplied
-- Effect codec, so any type proves the shape.
cardCodec :: Codec.Codec Text.Text
cardCodec = Common.text

codec :: Codec.Codec (Clause.Clause Text.Text)
codec = Clause.codec cardCodec

toJson :: Clause.Clause Text.Text -> Value.Value
toJson = Codec.encode codec

fromJson :: Value.Value -> Either Text.Text (Clause.Clause Text.Text)
fromJson = Codec.decode codec

-- One constructor, so seven cases: a populated clause, CR 603.5's `optionality`
-- flag when present, CR 118.12's `payGate` when present, CR 701.46a's
-- `condition` gate when present, CR 608.2c's `ifTaken` when present, CR 608.2d's
-- `orElse` branch when present, and every field defaulted at once.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Clause" $ do
  Spec.it s "MkClause with one effect" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing (Seq.singleton (Effect.Attach (SlotName.MkSlotName (Text.pack "target")))))
      " {\"effects\":[{\"type\":\"Attach\",\"value\":\"target\"}]} "
  -- CR 603.5: an Optional clause is what a printed "may" encodes to, and the key
  -- is emitted only for that value.
  Spec.it s "an Optional clause's optionality key is present" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Nothing Nothing (Optionality.Optional (PlayerRef.Relative PlayerRelation.You)) Nothing Seq.empty)
      " {\"optionality\":{\"type\":\"Optional\"}} "
  -- CR 608.2c: Tweeze's "If you do" names the ordinal of the clause it hangs
  -- off, and the key is emitted only when there is one.
  Spec.it s "a clause hanging off an earlier one writes the ifTaken key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause (Just (ClauseIndex.MkClauseIndex 1)) Nothing Nothing Optionality.Mandatory Nothing Seq.empty)
      " {\"ifTaken\":1} "
  -- CR 608.2d: Twiddle's tap names the untap it is exclusive with, and the key is
  -- emitted only when there is one. A DIFFERENT key from ifTaken, though both
  -- hold a bare ordinal: one clause may hang off an earlier one and branch
  -- against another, and a wire that fused them could not say which.
  Spec.it s "a clause exclusive with a sibling writes the orElse key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Nothing (Just (ClauseIndex.MkClauseIndex 1)) Optionality.Mandatory Nothing Seq.empty)
      " {\"orElse\":1} "
  -- CR 118.12a: Mana Leak's "unless its controller pays {3}" is what the
  -- payGate key encodes, and it is emitted only when there is one.
  Spec.it s "a clause carrying a resolution cost writes the key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Clause.MkClause
          Nothing
          Nothing
          Nothing
          Optionality.Mandatory
          ( Just
              PayGate.MkPayGate
                { PayGate.payer = PlayerRef.ControllerOfBound (SlotName.MkSlotName (Text.pack "spell")),
                  PayGate.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []},
                  PayGate.branch = PayBranch.IfNotPaid,
                  PayGate.obligation = PayObligation.Optional,
                  PayGate.offeredAt = Nothing
                }
          )
          (Seq.singleton (Effect.Counter (Counter.MkCounter (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) Nothing)))
      )
      " {\"effects\":[{\"type\":\"Counter\",\"value\":{\"ref\":{\"type\":\"InSlot\",\"value\":\"spell\"}}}],\"payGate\":{\"payer\":{\"type\":\"ControllerOfBound\",\"value\":\"spell\"},\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":{\"type\":\"IfNotPaid\"}}} "
  -- CR 701.46a: adapt's "if this permanent has no +1/+1 counters on it".
  Spec.it s "a clause carrying a condition writes the key" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      ( Clause.MkClause
          Nothing
          (Just (Condition.Compares (Compares.MkCompares (Quantity.ObjectCounters CounterKind.PlusOnePlusOne) Comparison.Exactly (Quantity.Literal 0))))
          Nothing
          Optionality.Mandatory
          Nothing
          Seq.empty
      )
      " {\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"ObjectCounters\",\"value\":{\"type\":\"PlusOnePlusOne\"}},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}} "
  Spec.it s "an empty clause omits every default field" $
    Common.assertJsonCodec
      s
      toJson
      fromJson
      (Clause.MkClause Nothing Nothing Nothing Optionality.Mandatory Nothing Seq.empty)
      " {} "
