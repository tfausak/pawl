module Pawl.Codec.ExilePlayPermissionSpec where

import qualified Pawl.Codec.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Types.PlayerId as PlayerId

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
          ExilePlayPermission.origin = PlayPermissionOrigin.Adventure
        }
      " {\"player\":1,\"source\":2,\"expiry\":{\"type\":\"Never\"},\"spending\":{\"type\":\"AsProduced\"},\"origin\":{\"type\":\"Adventure\"}} "
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
          ExilePlayPermission.origin = PlayPermissionOrigin.Granted
        }
      " {\"player\":3,\"source\":4,\"expiry\":{\"type\":\"AtCleanup\"},\"spending\":{\"type\":\"AnyType\"},\"origin\":{\"type\":\"Granted\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ExilePlayPermission.codec
