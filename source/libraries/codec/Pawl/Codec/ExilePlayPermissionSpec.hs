module Pawl.Codec.ExilePlayPermissionSpec where

import qualified Pawl.Codec.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PermissionCost as PermissionCost
import qualified Pawl.Types.PermissionVerb as PermissionVerb
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExilePlayPermission" $ do
  -- CR 715.3d: the Adventure permission, which states no duration (CR 611.2a's
  -- default) and no CR 118.14 rider. `origin` is what CR 715.3d's closing clause
  -- reads, so the two arms are written out separately below rather than left to
  -- one case.
  Spec.it s "CR 715.3d's Adventure permission" $
    Common.assertCodec
      s
      ExilePlayPermission.codec
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = PlayerId.MkPlayerId 1,
          ExilePlayPermission.source = ObjectId.MkObjectId 2,
          ExilePlayPermission.expiry = Expiry.Never,
          ExilePlayPermission.spending = ManaSpending.AsProduced,
          ExilePlayPermission.alternativeCost = Nothing,
          ExilePlayPermission.condition = Nothing,
          ExilePlayPermission.origin = PlayPermissionOrigin.Adventure,
          ExilePlayPermission.verb = PermissionVerb.Play,
          ExilePlayPermission.increase = 0,
          ExilePlayPermission.landEnters = TapState.Untapped
        }
      " {\"player\":1,\"source\":2,\"expiry\":{\"type\":\"Never\"},\"spending\":{\"type\":\"AsProduced\"},\"alternativeCost\":null,\"condition\":null,\"origin\":{\"type\":\"Adventure\"},\"verb\":{\"type\":\"Play\"},\"increase\":0,\"landEnters\":{\"type\":\"Untapped\"}} "
  -- CR 601.3 with CR 118.14's rider, the shape Dire Fleet Daredevil writes: a
  -- granted permission lasting until end of turn, mana of any type spendable on
  -- it.
  Spec.it s "a granted permission with CR 118.14's rider" $
    Common.assertCodec
      s
      ExilePlayPermission.codec
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = PlayerId.MkPlayerId 3,
          ExilePlayPermission.source = ObjectId.MkObjectId 4,
          ExilePlayPermission.expiry = Expiry.AtCleanup,
          ExilePlayPermission.spending = ManaSpending.AnyType,
          ExilePlayPermission.alternativeCost = Nothing,
          ExilePlayPermission.condition = Nothing,
          ExilePlayPermission.origin = PlayPermissionOrigin.Granted,
          ExilePlayPermission.verb = PermissionVerb.Play,
          ExilePlayPermission.increase = 0,
          ExilePlayPermission.landEnters = TapState.Untapped
        }
      " {\"player\":3,\"source\":4,\"expiry\":{\"type\":\"AtCleanup\"},\"spending\":{\"type\":\"AnyType\"},\"alternativeCost\":null,\"condition\":null,\"origin\":{\"type\":\"Granted\"},\"verb\":{\"type\":\"Play\"},\"increase\":0,\"landEnters\":{\"type\":\"Untapped\"}} "
  -- CR 118.9's waiver, the shape Extract Power writes: a granted permission
  -- lasting as long as the card remains exiled, with the mana cost waived -- an
  -- alternative cost of nothing. The two riders are independent, so this one
  -- carries CR 118.14's default.
  Spec.it s "a granted permission with CR 118.9's waiver" $
    Common.assertCodec
      s
      ExilePlayPermission.codec
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = PlayerId.MkPlayerId 5,
          ExilePlayPermission.source = ObjectId.MkObjectId 6,
          ExilePlayPermission.expiry = Expiry.Never,
          ExilePlayPermission.spending = ManaSpending.AsProduced,
          ExilePlayPermission.alternativeCost = Just (PermissionCost.InsteadOfManaCost (ManaCost.MkManaCost [])),
          ExilePlayPermission.condition = Nothing,
          ExilePlayPermission.origin = PlayPermissionOrigin.Granted,
          ExilePlayPermission.verb = PermissionVerb.Play,
          ExilePlayPermission.increase = 0,
          ExilePlayPermission.landEnters = TapState.Untapped
        }
      " {\"player\":5,\"source\":6,\"expiry\":{\"type\":\"Never\"},\"spending\":{\"type\":\"AsProduced\"},\"alternativeCost\":{\"type\":\"InsteadOfManaCost\",\"value\":[]},\"condition\":null,\"origin\":{\"type\":\"Granted\"},\"verb\":{\"type\":\"Play\"},\"increase\":0,\"landEnters\":{\"type\":\"Untapped\"}} "
  -- Lightstall Inquisitor's: CR 601.2f's increase and CR 614.1d's tapped entry.
  Spec.it s "a granted permission with Lightstall Inquisitor's riders" $
    Common.assertCodec
      s
      ExilePlayPermission.codec
      ExilePlayPermission.MkExilePlayPermission
        { ExilePlayPermission.player = PlayerId.MkPlayerId 7,
          ExilePlayPermission.source = ObjectId.MkObjectId 8,
          ExilePlayPermission.expiry = Expiry.Never,
          ExilePlayPermission.spending = ManaSpending.AsProduced,
          ExilePlayPermission.alternativeCost = Nothing,
          ExilePlayPermission.condition = Nothing,
          ExilePlayPermission.origin = PlayPermissionOrigin.Granted,
          ExilePlayPermission.verb = PermissionVerb.Play,
          ExilePlayPermission.increase = 1,
          ExilePlayPermission.landEnters = TapState.Tapped
        }
      " {\"player\":7,\"source\":8,\"expiry\":{\"type\":\"Never\"},\"spending\":{\"type\":\"AsProduced\"},\"alternativeCost\":null,\"condition\":null,\"origin\":{\"type\":\"Granted\"},\"verb\":{\"type\":\"Play\"},\"increase\":1,\"landEnters\":{\"type\":\"Tapped\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ExilePlayPermission.codec
