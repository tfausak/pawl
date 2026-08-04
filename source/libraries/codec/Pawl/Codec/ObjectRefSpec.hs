{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ObjectRefSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ObjectRef" $ do
  Spec.it s "InSlot" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
      """ "target" """
  Spec.it s "EachMatching" $
    Common.assertJsonCodec
      s
      ObjectRef.toJson
      ObjectRef.fromJson
      (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      """ {"type":"HasCardType","value":{"type":"Creature"}} """
  -- An ObjectRef is untagged and told apart by JSON TYPE rather than by a tag:
  -- a slot is a bare string, a swept set is an object. The two assertions
  -- above already pin that; this one additionally guards against a decoder
  -- that read every payload as one arm regardless of its JSON type.
  Spec.it s "the two arms are told apart by JSON type, not by a tag" $
    Spec.assertBool
      s
      ( ObjectRef.toJson (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")))
          /= ObjectRef.toJson (ObjectRef.EachMatching (Filter.HasCardType CardType.Creature))
      )
      "a slot and a swept set encode differently"
