{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DealDamageSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DealDamage as DealDamage
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DealDamage as DealDamage
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DealDamage" $ do
  -- CR 120.1: this much damage to the objects or players the ref names, from the
  -- resolving object's own source (CR 113.7) when no dealer is written.
  Spec.it s "MkDealDamage, no dealer" $
    Common.assertCodec
      s
      DealDamage.codec
      ( DealDamage.MkDealDamage
          { DealDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            DealDamage.quantity = Quantity.Literal 3,
            DealDamage.dealer = Nothing
          }
      )
      """ {"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3}} """
  -- CR 120.2b: the sentence names some other object as the source.
  Spec.it s "MkDealDamage, all three keys" $
    Common.assertCodec
      s
      DealDamage.codec
      ( DealDamage.MkDealDamage
          { DealDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            DealDamage.quantity = Quantity.Literal 3,
            DealDamage.dealer = Just (SlotName.MkSlotName (Text.pack "dealer"))
          }
      )
      """ {"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3},"dealer":"dealer"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s DealDamage.codec
