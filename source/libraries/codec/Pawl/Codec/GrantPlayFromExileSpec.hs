module Pawl.Codec.GrantPlayFromExileSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PermissionCost as PermissionCost
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TapState as TapState

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
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
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
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
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
            GrantPlayFromExile.alternativeCost = Just (PermissionCost.InsteadOfManaCost (ManaCost.MkManaCost [])),
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
          }
      )
      " {\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"alternativeCost\":{\"type\":\"InsteadOfManaCost\",\"value\":[]}} "
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
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
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
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Cast,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"verb\":{\"type\":\"Cast\"}} "
  Spec.it s "a missing player, spending, alternativeCost, condition, verb, increase or landEnters key decodes as the default" $
    Common.assertFromJson
      s
      (Codec.decode GrantPlayFromExile.codec)
      "{\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"}}"
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
          }
      )
  -- Hama, the Bloodbender's: CR 118.9's waterbend alternative, open only
  -- during the holder's turn.
  Spec.it s "MkGrantPlayFromExile, a waterbend during your turn" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.player = PlayerRef.Relative PlayerRelation.You,
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.alternativeCost = Just PermissionCost.WaterbendManaValue,
            GrantPlayFromExile.condition = Just (Condition.Compares (Compares.MkCompares (Quantity.IsActivePlayer (PlayerRef.Relative PlayerRelation.You)) Comparison.Exactly (Quantity.Literal 1))),
            GrantPlayFromExile.verb = PermissionVerb.Cast,
            GrantPlayFromExile.increase = 0,
            GrantPlayFromExile.landEnters = TapState.Untapped
          }
      )
      " {\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"alternativeCost\":{\"type\":\"WaterbendManaValue\"},\"condition\":{\"type\":\"Compares\",\"value\":{\"comparison\":{\"type\":\"Exactly\"},\"measured\":{\"type\":\"IsActivePlayer\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"threshold\":{\"type\":\"Literal\",\"value\":1}}},\"verb\":{\"type\":\"Cast\"}} "
  -- Lightstall Inquisitor's: CR 601.2f's increase and CR 614.1d's tapped entry
  -- written out.
  Spec.it s "MkGrantPlayFromExile, Lightstall Inquisitor's riders" $
    Common.assertCodec
      s
      GrantPlayFromExile.codec
      ( GrantPlayFromExile.MkGrantPlayFromExile
          { GrantPlayFromExile.duration = Duration.Indefinite,
            GrantPlayFromExile.player = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "opponent")),
            GrantPlayFromExile.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "exiled")),
            GrantPlayFromExile.spending = ManaSpending.AsProduced,
            GrantPlayFromExile.alternativeCost = Nothing,
            GrantPlayFromExile.condition = Nothing,
            GrantPlayFromExile.verb = PermissionVerb.Play,
            GrantPlayFromExile.increase = 1,
            GrantPlayFromExile.landEnters = TapState.Tapped
          }
      )
      " {\"duration\":{\"type\":\"Indefinite\"},\"player\":{\"type\":\"InSlot\",\"value\":\"opponent\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"exiled\"},\"increase\":1,\"landEnters\":{\"type\":\"Tapped\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s GrantPlayFromExile.codec
