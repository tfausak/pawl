{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExilePlayPermission where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PermissionCost as PermissionCost
import qualified Pawl.Codec.PermissionVerb as PermissionVerb
import qualified Pawl.Codec.PlayPermissionOrigin as PlayPermissionOrigin
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission

-- | Every axis 'Fields.required', because none of them has a default the rules
-- give: `spending` is CR 118.14's rider, `alternativeCost` is CR 118.9a's,
-- `increase` and `landEnters` are CR 601.2f's and CR 614.1d's, and `origin`
-- is CR 715.3d's own question -- a permission written without any of them
-- would decode as a different permission rather than as an incomplete
-- one. The type's haddock argues each field. Unlike
-- Pawl.Codec.GrantPlayFromExile's opcode, which elides its riders at their
-- defaults, this is a whole game state's record of a permission already
-- granted.
codec :: Codec.Codec ExilePlayPermission.ExilePlayPermission
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec ExilePlayPermission.player
  source <- Fields.required "source" ObjectId.codec ExilePlayPermission.source
  expiry <- Fields.required "expiry" Expiry.codec ExilePlayPermission.expiry
  spending <- Fields.required "spending" ManaSpending.codec ExilePlayPermission.spending
  alternativeCost <- Fields.required "alternativeCost" (Common.maybe PermissionCost.codec) ExilePlayPermission.alternativeCost
  condition <- Fields.required "condition" (Common.maybe Condition.codec) ExilePlayPermission.condition
  origin <- Fields.required "origin" PlayPermissionOrigin.codec ExilePlayPermission.origin
  verb <- Fields.required "verb" PermissionVerb.codec ExilePlayPermission.verb
  increase <- Fields.required "increase" Common.natural ExilePlayPermission.increase
  landEnters <- Fields.required "landEnters" TapState.codec ExilePlayPermission.landEnters
  pure
    ExilePlayPermission.MkExilePlayPermission
      { ExilePlayPermission.player = player,
        ExilePlayPermission.source = source,
        ExilePlayPermission.expiry = expiry,
        ExilePlayPermission.spending = spending,
        ExilePlayPermission.alternativeCost = alternativeCost,
        ExilePlayPermission.condition = condition,
        ExilePlayPermission.origin = origin,
        ExilePlayPermission.verb = verb,
        ExilePlayPermission.increase = increase,
        ExilePlayPermission.landEnters = landEnters
      }
