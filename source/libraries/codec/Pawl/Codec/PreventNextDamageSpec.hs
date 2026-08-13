{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PreventNextDamageSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.PreventNextDamage as PreventNextDamage
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PreventNextDamage as PreventNextDamage
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | The @effect@ parameter is instantiated at 'Text.Text' rather than at
-- 'Pawl.Types.Effect.Effect': the record is parametric in it precisely so that
-- neither module names the other, and a codec round trip proves the shape at any
-- type. Pawl.Codec.EffectSpec covers the real instantiation.
codec :: Codec.Codec (PreventNextDamage.PreventNextDamage Text.Text)
codec = PreventNextDamage.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PreventNextDamage" $ do
  -- CR 615.7's shield with no CR 615.5 clause, which is every prevention in the
  -- pool but Test of Faith: the riders key is elided rather than written empty.
  Spec.it s "MkPreventNextDamage, riders elided" $
    Common.assertCodec
      s
      codec
      ( PreventNextDamage.MkPreventNextDamage
          { PreventNextDamage.duration = Duration.UntilEndOfTurn,
            PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventNextDamage.quantity = Quantity.Literal 4,
            PreventNextDamage.riders = Seq.empty
          }
      )
      """ {"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":4}} """
  -- CR 615.5's additional effect, riding the shield.
  Spec.it s "MkPreventNextDamage, riders written" $
    Common.assertCodec
      s
      codec
      ( PreventNextDamage.MkPreventNextDamage
          { PreventNextDamage.duration = Duration.UntilEndOfTurn,
            PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventNextDamage.quantity = Quantity.Literal 3,
            PreventNextDamage.riders = Seq.singleton (Text.pack "a rider")
          }
      )
      """ {"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"target"},"quantity":{"type":"Literal","value":3},"riders":["a rider"]} """
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
