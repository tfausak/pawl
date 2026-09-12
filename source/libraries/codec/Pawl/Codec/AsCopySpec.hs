module Pawl.Codec.AsCopySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AsCopy as AsCopy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CopyException as CopyException
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SetPowerToughness as SetPowerToughness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.WithCounters as WithCounters

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AsCopy" $ do
  -- CR 707.5: Clone's "any creature", excepting nothing -- the empty exception
  -- list is defaulted away.
  Spec.it s "MkAsCopy, no exceptions: the key is omitted" $
    Common.assertCodec
      s
      (AsCopy.codec Common.text)
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [] False Nothing)
      " {\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}} "
  -- CR 707.9d: Quicksilver Gargantuan's "except it's 7/7", beside the same
  -- eligible set.
  Spec.it s "MkAsCopy, an exception: both keys" $
    Common.assertCodec
      s
      (AsCopy.codec Common.text)
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [CopyException.SetPowerToughness (SetPowerToughness.MkSetPowerToughness 7 7)] False Nothing)
      " {\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"exceptions\":[{\"type\":\"SetPowerToughness\",\"value\":{\"power\":7,\"toughness\":7}}]} "
  -- CR 614.1d inside CR 614.1c: Vesuva's "enter tapped as a copy".
  Spec.it s "MkAsCopy, entering tapped (Vesuva)" $
    Common.assertCodec
      s
      (AsCopy.codec Common.text)
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Land) [] True Nothing)
      " {\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Land\"}},\"tapped\":true} "
  -- CR 707.9e: Altered Ego's "except it enters with X additional +1/+1 counters
  -- on it", the exception that is an additional effect. CR 107.3m's X is the
  -- announced one, spelled as the slot Pawl.Engine.Quantity.substituteAnnouncedX
  -- rewrites.
  Spec.it s "MkAsCopy, additional counters (Altered Ego)" $
    Common.assertCodec
      s
      (AsCopy.codec Common.text)
      (AsCopy.MkAsCopy (Filter.HasCardType CardType.Creature) [] False (Just (WithCounters.one CounterKind.PlusOnePlusOne (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))))))
      " {\"eligible\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"InSlot\",\"value\":\"X\"}}]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (AsCopy.codec Common.text)
