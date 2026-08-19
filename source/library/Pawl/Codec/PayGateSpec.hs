module Pawl.Codec.PayGateSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.PayGate as PayGate
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ClauseIndex as ClauseIndex
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.PayBranch as PayBranch
import qualified Pawl.Types.PayGate as PayGate
import qualified Pawl.Types.PayObligation as PayObligation
import qualified Pawl.Types.SlotName as SlotName

manaLeak :: PayGate.PayGate
manaLeak =
  PayGate.MkPayGate
    { PayGate.payer = SlotName.MkSlotName (Text.pack "spell"),
      PayGate.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []},
      PayGate.branch = PayBranch.IfNotPaid,
      PayGate.obligation = PayObligation.Optional,
      PayGate.offeredAt = Nothing
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PayGate" $ do
  -- CR 118.12a: Mana Leak's "unless its controller pays {3}" -- the payer named
  -- by the same slot the Counter effect reads, and the cost it offers.
  Spec.it s "MkPayGate, Mana Leak's clause" $
    Common.assertCodec
      s
      PayGate.codec
      manaLeak
      " {\"payer\":\"spell\",\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":{\"type\":\"IfNotPaid\"}} "
  -- CR 118.12's other branch, Merfolk Seer's: the same three keys, and only the
  -- branch differs.
  Spec.it s "MkPayGate, Merfolk Seer's clause" $
    Common.assertCodec
      s
      PayGate.codec
      manaLeak {PayGate.branch = PayBranch.IfPaid}
      " {\"payer\":\"spell\",\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":{\"type\":\"IfPaid\"}} "
  -- CR 118.12's mandatory limb, Standstill's, and the shared offer, Don't Make
  -- a Sound's second clause -- the two keys that are elided everywhere else.
  Spec.it s "MkPayGate, Standstill's mandatory sacrifice" $
    Common.assertCodec
      s
      PayGate.codec
      manaLeak {PayGate.branch = PayBranch.IfPaid, PayGate.obligation = PayObligation.Mandatory}
      " {\"payer\":\"spell\",\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":{\"type\":\"IfPaid\"},\"obligation\":{\"type\":\"Mandatory\"}} "
  Spec.it s "MkPayGate, a clause hanging off an earlier clause's offer" $
    Common.assertCodec
      s
      PayGate.codec
      manaLeak {PayGate.branch = PayBranch.IfPaid, PayGate.offeredAt = Just (ClauseIndex.MkClauseIndex 0)}
      " {\"payer\":\"spell\",\"cost\":{\"mana\":[{\"type\":\"Generic\",\"value\":3}]},\"branch\":{\"type\":\"IfPaid\"},\"offeredAt\":0} "
  -- The first three keys are required: none has a default an absent key could
  -- mean.
  Spec.it s "an omitted payer field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode PayGate.codec (Value.object [Value.pair "cost" (Value.object [Value.pair "mana" (Value.array [])]), Value.pair "branch" (Value.object [Value.pair "type" (Value.text (Text.pack "IfNotPaid"))])])))
      "expected a decode failure"
  Spec.it s "an omitted cost field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode PayGate.codec (Value.object [Value.pair "payer" (Value.text (Text.pack "spell")), Value.pair "branch" (Value.object [Value.pair "type" (Value.text (Text.pack "IfNotPaid"))])])))
      "expected a decode failure"
  Spec.it s "an omitted branch field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (Codec.decode PayGate.codec (Value.object [Value.pair "payer" (Value.text (Text.pack "spell")), Value.pair "cost" (Value.object [Value.pair "mana" (Value.array [])])])))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s PayGate.codec
