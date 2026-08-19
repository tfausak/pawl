module Pawl.Codec.GrantPlayFromExileSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GrantPlayFromExile" $ do
  -- Victor Mancha, Runaway's grant: a permission that says nothing about mana,
  -- so the rider key is absent in both directions.
  Spec.it s "MkGrantPlayFromExile, an ordinary permission omits the rider" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.UntilEndOfTurn,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}} "
  -- Dire Fleet Daredevil's: CR 118.14's clause written out.
  Spec.it s "MkGrantPlayFromExile, CR 118.14's rider" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.UntilEndOfTurn,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AnyType
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"spending\":{\"type\":\"AnyType\"}} "
  Spec.it s "a missing spending key decodes as AsProduced" $
    Common.assertFromJson
      s
      (Codec.decode GrantPlayFromExile.codec)
      "{\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}}"
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced
          }
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s GrantPlayFromExile.codec
