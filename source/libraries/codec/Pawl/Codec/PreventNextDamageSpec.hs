module Pawl.Codec.PreventNextDamageSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.PreventNextDamage as PreventNextDamage
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Filter as Filter
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
  -- CR 615.7's shield naming no kind and carrying no CR 615.5 clause, which is
  -- Mending Hands: both optional keys are elided rather than written out.
  Spec.it s "MkPreventNextDamage, kind and riders elided" $
    Common.assertCodec
      s
      codec
      ( PreventNextDamage.MkPreventNextDamage
          { PreventNextDamage.duration = Duration.UntilEndOfTurn,
            PreventNextDamage.kind = Nothing,
            PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventNextDamage.chosenSource = Nothing,
            PreventNextDamage.quantity = Quantity.Literal 4,
            PreventNextDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":4}} "
  -- Decorated Griffin's "combat" and Test of Faith's CR 615.5 additional effect,
  -- together.
  Spec.it s "MkPreventNextDamage, kind and riders written" $
    Common.assertCodec
      s
      codec
      ( PreventNextDamage.MkPreventNextDamage
          { PreventNextDamage.duration = Duration.UntilEndOfTurn,
            PreventNextDamage.kind = Just DamageKind.Combat,
            PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventNextDamage.chosenSource = Nothing,
            PreventNextDamage.quantity = Quantity.Literal 3,
            PreventNextDamage.riders = Seq.singleton (Text.pack "a rider")
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"quantity\":{\"type\":\"Literal\",\"value\":3},\"riders\":[\"a rider\"]} "
  -- CR 609.7a's "by a source of your choice", which is Healing Grace: the key is
  -- WRITTEN and its Filter is the trivial one, since the card names no property
  -- the chosen source must have. Elided and present-but-trivial are different
  -- shields, so the round trip has to keep them apart.
  Spec.it s "MkPreventNextDamage, CR 609.7a's chosen source" $
    Common.assertCodec
      s
      codec
      ( PreventNextDamage.MkPreventNextDamage
          { PreventNextDamage.duration = Duration.UntilEndOfTurn,
            PreventNextDamage.kind = Nothing,
            PreventNextDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventNextDamage.chosenSource = Just (Filter.And []),
            PreventNextDamage.quantity = Quantity.Literal 3,
            PreventNextDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"chosenSource\":{\"type\":\"And\",\"value\":[]},\"quantity\":{\"type\":\"Literal\",\"value\":3}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
