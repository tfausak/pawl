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
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GrantPlayFromExile" $ do
  -- Victor Mancha, Runaway's grant: a permission that says nothing about mana,
  -- so neither rider key is present in either direction.
  Spec.it s "MkGrantPlayFromExile, an ordinary permission omits the rider" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.UntilEndOfTurn,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.withoutPayingManaCost = False,
            GrantPlayFromExile.verb = PermissionVerb.Play
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
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AnyType,
            GrantPlayFromExile.withoutPayingManaCost = False,
            GrantPlayFromExile.verb = PermissionVerb.Play
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"spending\":{\"type\":\"AnyType\"}} "
  -- Extract Power's: CR 118.9's waiver written out, and no CR 118.14 rider --
  -- the two ride the same grant and no card in the pool prints both.
  Spec.it s "MkGrantPlayFromExile, CR 118.9's waiver" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.withoutPayingManaCost = True,
            GrantPlayFromExile.verb = PermissionVerb.Play
          }
      )
      " {\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"withoutPayingManaCost\":true} "
  -- Elkin Lair's: CR 601.3's permission for a seat a slot holds rather than for
  -- the resolving controller, and no other rider.
  Spec.it s "MkGrantPlayFromExile, CR 601.3's grantee" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.UntilEndOfTurn,
            GrantPlayFromExile.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.withoutPayingManaCost = False,
            GrantPlayFromExile.verb = PermissionVerb.Play
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"player\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}} "
  -- Ragavan, Nimble Pilferer's "you may cast that card": CR 601.3's Cast verb,
  -- written out against the Play default.
  Spec.it s "MkGrantPlayFromExile, a cast-only permission" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.UntilEndOfTurn,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.withoutPayingManaCost = False,
            GrantPlayFromExile.verb = PermissionVerb.Cast
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"verb\":{\"type\":\"Cast\"}} "
  Spec.it s "a missing player, spending, withoutPayingManaCost or verb key decodes as the default" $
    Common.assertFromJson
      s
      (Codec.decode GrantPlayFromExile.codec)
      "{\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}}"
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.withoutPayingManaCost = False,
            GrantPlayFromExile.verb = PermissionVerb.Play
          }
      )
  Spec.it s "has a schema" $ Common.assertHasSchema s GrantPlayFromExile.codec
