module Pawl.Codec.ForEachSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.ForEach as ForEach
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ForEach as ForEach
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

-- | The @effect@ parameter is instantiated at 'Text.Text' rather than at
-- 'Pawl.Types.Effect.Effect', for the reason Pawl.Codec.PreventNextDamageSpec
-- gives. Pawl.Codec.EffectSpec covers the real instantiation.
codec :: Codec.Codec (ForEach.ForEach Text.Text)
codec = ForEach.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForEach" $ do
  -- Soulfire Eruption's shape: the swept set is the spell's own target slot,
  -- and the body's instructions read the member under the loop's name.
  Spec.it s "MkForEach" $
    Common.assertCodec
      s
      codec
      ( ForEach.MkForEach
          { ForEach.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "victims")),
            ForEach.slot = SlotName.MkSlotName (Text.pack "victim"),
            ForEach.body = Seq.fromList [Text.pack "first", Text.pack "second"],
            -- CR 608.2f's second sentence, which that rule's Soulfire Eruption
            -- example states outright.
            ForEach.individually = True
          }
      )
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"victims\"},\"slot\":\"victim\",\"body\":[\"first\",\"second\"],\"individually\":true} "
  -- An empty body is representable rather than rejected: the codec's `required`
  -- keys are about a key being ABSENT, and CR 101.3 already makes an
  -- instruction that does nothing a no-op rather than an error.
  Spec.it s "MkForEach with an empty body" $
    Common.assertCodec
      s
      codec
      ( ForEach.MkForEach
          { ForEach.ref = ObjectRef.EachPlayer,
            ForEach.slot = SlotName.MkSlotName (Text.pack "victim"),
            ForEach.body = Seq.empty,
            -- The default CR 608.2f's "in most cases" gives, so the key is absent
            -- from the wire.
            ForEach.individually = False
          }
      )
      " {\"ref\":{\"type\":\"EachPlayer\"},\"slot\":\"victim\",\"body\":[]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
