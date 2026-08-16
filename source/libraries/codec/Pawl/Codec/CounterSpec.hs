{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounterSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Counter as Counter
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Counter as Counter
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Counter" $ do
  -- Cancel's "counter target spell": the targeted slot is the same ObjectRef's
  -- InSlot, and CR 701.6a's count is bound only where a later effect reads it,
  -- so the key is absent here and present below.
  Spec.it s "MkCounter, no bound slot: the key is omitted" $
    Common.assertCodec
      s
      Counter.codec
      (Counter.MkCounter (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "spell"))) Nothing)
      """ {"ref":{"type":"InSlot","value":"spell"}} """
  -- Swift Silence's "counter all other spells. Draw a card for each spell
  -- countered this way".
  Spec.it s "MkCounter, a swept set and a bound slot" $
    Common.assertCodec
      s
      Counter.codec
      (Counter.MkCounter (ObjectRef.EachSpell (Filter.Not Filter.IsSource)) (Just (SlotName.MkSlotName (Text.pack "countered"))))
      """ {"ref":{"type":"EachSpell","value":{"type":"Not","value":{"type":"IsSource"}}},"slot":"countered"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s Counter.codec
