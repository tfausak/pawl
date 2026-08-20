module Pawl.Codec.PreventAllDamageSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.PreventAllDamage as PreventAllDamage
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageDirection as DamageDirection
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.SlotName as SlotName

-- | The @effect@ parameter is instantiated at 'Text.Text' rather than at
-- 'Pawl.Types.Effect.Effect', for the reason Pawl.Codec.PreventNextDamageSpec
-- gives.
codec :: Codec.Codec (PreventAllDamage.PreventAllDamage Text.Text)
codec = PreventAllDamage.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PreventAllDamage" $ do
  -- CR 615.1's shield naming no kind and carrying no CR 615.5 clause -- Selfless
  -- Squire's. All three optional keys are elided, so this is byte for byte what
  -- Pawl.Codec.DurationRef used to write for this arm.
  Spec.it s "MkPreventAllDamage, kind, direction and riders elided" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Nothing,
            PreventAllDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you")),
            PreventAllDamage.direction = DamageDirection.DealtTo,
            PreventAllDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"you\"}} "
  -- Inkshield's "all COMBAT damage", Brace for Impact's CR 615.5 clause and
  -- Dovin, Hand of Control's "dealt BY target permanent", together: every
  -- defaulted key written.
  Spec.it s "MkPreventAllDamage, kind, direction and riders written" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Just DamageKind.Combat,
            PreventAllDamage.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            PreventAllDamage.direction = DamageDirection.DealtBy,
            PreventAllDamage.riders = Seq.singleton (Text.pack "a rider")
          }
      )
      " {\"direction\":{\"type\":\"DealtBy\"},\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"riders\":[\"a rider\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
