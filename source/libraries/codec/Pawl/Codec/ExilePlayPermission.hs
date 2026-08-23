{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExilePlayPermission where

import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission

-- | All five axes 'Fields.required', because none of them has a default the
-- rules give: `spending` is CR 118.14's rider and `origin` is CR 715.3d's own
-- question, and a permission written without either would decode as a different
-- permission rather than as an incomplete one. The type's haddock argues each
-- field.
codec :: Codec.Codec ExilePlayPermission.ExilePlayPermission
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec ExilePlayPermission.player
  source <- Fields.required "source" ObjectId.codec ExilePlayPermission.source
  expiry <- Fields.required "expiry" Expiry.codec ExilePlayPermission.expiry
  spending <- Fields.required "spending" ManaSpending.codec ExilePlayPermission.spending
  origin <- Fields.required "origin" PlayPermissionOrigin.codec ExilePlayPermission.origin
  pure
    ExilePlayPermission.MkExilePlayPermission
      { ExilePlayPermission.player = player,
        ExilePlayPermission.source = source,
        ExilePlayPermission.expiry = expiry,
        ExilePlayPermission.spending = spending,
        ExilePlayPermission.origin = origin
      }
