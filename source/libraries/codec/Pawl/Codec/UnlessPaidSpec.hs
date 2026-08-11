{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.UnlessPaidSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.UnlessPaid as UnlessPaid
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.UnlessPaid as UnlessPaid

manaLeak :: UnlessPaid.UnlessPaid
manaLeak =
  UnlessPaid.MkUnlessPaid
    { UnlessPaid.payer = SlotName.MkSlotName (Text.pack "spell"),
      UnlessPaid.cost = Cost.MkCost {Cost.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 3]), Cost.components = []}
    }

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.UnlessPaid" $ do
  -- CR 118.12a: Mana Leak's "unless its controller pays {3}" -- the payer named
  -- by the same slot the Counter effect reads, and the cost it offers.
  Spec.it s "MkUnlessPaid, Mana Leak's clause" $
    Common.assertJsonCodec
      s
      UnlessPaid.toJson
      UnlessPaid.fromJson
      manaLeak
      """ {"payer":"spell","cost":{"mana":[{"type":"Generic","value":3}]}} """
  -- Both keys are required: neither has a default an absent key could mean.
  Spec.it s "an omitted payer field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (UnlessPaid.fromJson (Common.object [Common.pair "cost" (Common.object [Common.pair "mana" (Common.array [])])])))
      "expected a decode failure"
  Spec.it s "an omitted cost field is a decode error" $
    Spec.assertBool
      s
      (Either.isLeft (UnlessPaid.fromJson (Common.object [Common.pair "payer" (Common.text (Text.pack "spell"))])))
      "expected a decode failure"
