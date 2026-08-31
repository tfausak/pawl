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
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PreventAllDamage as PreventAllDamage
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

-- | The @effect@ parameter is instantiated at 'Text.Text' rather than at
-- 'Pawl.Types.Effect.Effect', for the reason Pawl.Codec.PreventNextDamageSpec
-- gives.
codec :: Codec.Codec (PreventAllDamage.PreventAllDamage Text.Text)
codec = PreventAllDamage.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PreventAllDamage" $ do
  -- CR 615.1's shield naming no kind and carrying no CR 615.5 clause -- Selfless
  -- Squire's. Every optional key is elided, so this is byte for byte what
  -- Pawl.Codec.DurationRef used to write for this arm.
  Spec.it s "MkPreventAllDamage, kind, direction and riders elided" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Nothing,
            PreventAllDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))),
            PreventAllDamage.whatRecipient = Nothing,
            PreventAllDamage.direction = DamageDirection.DealtTo,
            PreventAllDamage.chosenSource = Nothing,
            PreventAllDamage.whatSource = Filter.And [],
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
            PreventAllDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))),
            PreventAllDamage.whatRecipient = Nothing,
            PreventAllDamage.direction = DamageDirection.DealtBy,
            PreventAllDamage.chosenSource = Nothing,
            PreventAllDamage.whatSource = Filter.And [],
            PreventAllDamage.riders = Seq.singleton (Text.pack "a rider")
          }
      )
      " {\"direction\":{\"type\":\"DealtBy\"},\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"riders\":[\"a rider\"]} "
  -- CR 609.7a's "by a source of your choice" on the UNBOUNDED shield, which is
  -- Auriok Replica: the key is WRITTEN and its Filter is the trivial one, since
  -- the card names no property the chosen source must have. Elided and
  -- present-but-trivial are different shields, so the round trip has to keep them
  -- apart.
  Spec.it s "MkPreventAllDamage, CR 609.7a's chosen source" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Nothing,
            PreventAllDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))),
            PreventAllDamage.whatRecipient = Nothing,
            PreventAllDamage.direction = DamageDirection.DealtTo,
            PreventAllDamage.chosenSource = Just (Filter.And []),
            PreventAllDamage.whatSource = Filter.And [],
            PreventAllDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"you\"},\"chosenSource\":{\"type\":\"And\",\"value\":[]}} "
  -- CR 609.7b's PROPERTY-named source, which is Scarecrow's "by creatures with
  -- flying": the key is written and NO chosen source sits beside it, which is
  -- what tells this shield apart from Auriok Replica's above. The trivial
  -- predicate is what every other shield writes, so a non-trivial Filter is what
  -- proves the key decodes.
  Spec.it s "MkPreventAllDamage, CR 609.7b's property-named source" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Nothing,
            PreventAllDamage.ref = Just (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "you"))),
            PreventAllDamage.whatRecipient = Nothing,
            PreventAllDamage.direction = DamageDirection.DealtTo,
            PreventAllDamage.chosenSource = Nothing,
            PreventAllDamage.whatSource = Filter.HasKeyword Keyword.Flying,
            PreventAllDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"you\"},\"whatSource\":{\"type\":\"HasKeyword\",\"value\":{\"type\":\"Flying\"}}} "
  -- CR 611.2c's DESCRIBED recipient side, which is Pack Leader's "to Dogs you
  -- control": the ref key is absent and the predicate carries the whole recipient
  -- question, the alternative spelling to every case above.
  Spec.it s "MkPreventAllDamage, a described recipient side and no ref" $
    Common.assertCodec
      s
      codec
      ( PreventAllDamage.MkPreventAllDamage
          { PreventAllDamage.duration = Duration.UntilEndOfTurn,
            PreventAllDamage.kind = Just DamageKind.Combat,
            PreventAllDamage.ref = Nothing,
            PreventAllDamage.whatRecipient = Just (Filter.HasSubtype Subtype.Dog),
            PreventAllDamage.direction = DamageDirection.DealtTo,
            PreventAllDamage.chosenSource = Nothing,
            PreventAllDamage.whatSource = Filter.And [],
            PreventAllDamage.riders = Seq.empty
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"kind\":{\"type\":\"Combat\"},\"whatRecipient\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Dog\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
