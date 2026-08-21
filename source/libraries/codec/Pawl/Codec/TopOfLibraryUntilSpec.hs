module Pawl.Codec.TopOfLibraryUntilSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.TopOfLibraryUntil as TopOfLibraryUntil
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TopOfLibraryUntil as TopOfLibraryUntil

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TopOfLibraryUntil" $ do
  -- Treasure Hunt's "until you reveal a nonland card": one match, written as a
  -- literal.
  Spec.it s "MkTopOfLibraryUntil, all three keys" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.Not (Filter.HasCardType CardType.Land),
            TopOfLibraryUntil.count = Quantity.Literal 1
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"Not\",\"value\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}},\"count\":{\"type\":\"Literal\",\"value\":1}} "
  -- Open the Way's "until you reveal X land cards": the count is CR 601.2b's
  -- announced X, read from the slot casting filled.
  Spec.it s "MkTopOfLibraryUntil carries the announced X Open the Way needs" $
    Common.assertCodec
      s
      TopOfLibraryUntil.codec
      ( TopOfLibraryUntil.MkTopOfLibraryUntil
          { TopOfLibraryUntil.player = PlayerRef.Relative PlayerRelation.You,
            TopOfLibraryUntil.filter = Filter.HasCardType CardType.Land,
            TopOfLibraryUntil.count = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))
          }
      )
      " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"count\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  -- The filter is what a MATCH is, so a ref without one names a walk that never
  -- stops: a decode failure rather than a library taken whole.
  Spec.it s "a walk with no filter is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"count\":{\"type\":\"Literal\",\"value\":1}} ") >>= Codec.decode TopOfLibraryUntil.codec))
      "expected a decode failure"
  -- And a ref with no count does not say how many matches end the walk.
  Spec.it s "a walk with no count is rejected" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"player\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}},\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}}} ") >>= Codec.decode TopOfLibraryUntil.codec))
      "expected a decode failure"
  Spec.it s "has a schema" $ Common.assertHasSchema s TopOfLibraryUntil.codec
