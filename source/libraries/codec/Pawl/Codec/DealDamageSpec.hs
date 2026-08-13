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
  -- CR 119.3: this much damage to the objects or players the ref names.
  Spec.it s "MkDealDamage, both keys" $
    Common.assertCodec
      s
      DealDamage.codec
      ( DealDamage.MkDealDamage
          { DealDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            DealDamage.quantity = Quantity.Literal 3
          }
      )
      """ {"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s DealDamage.codec
