module Pawl.Types.IgnoredAbility where

import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 116.2d: one player has taken the special action that lets them ignore the
-- effect of a permanent's static ability, and for how long.
--
-- The record Pawl.Types.ExilePlayPermission is, one axis over: a
-- (player, source, expiry) triple that grants nothing and instead SUPPRESSES.
-- It lives on Pawl.Types.GameState rather than on the object, unlike that one,
-- because CR 604.2 ends the ability with the permanent -- an ignore whose source
-- has left the battlefield suppresses nothing that still exists, so nothing has
-- to travel with the object.
--
-- The source AND the ability's name, which is the grain CR 116.2d states: "the
-- effect from that ability". The source alone would not do it -- one permanent
-- may grant the permission on one of two unrelated abilities -- and the name
-- alone would not either, since two permanents may print the same name.
data IgnoredAbility = MkIgnoredAbility
  { -- | Who ignores it. CR 116.2d's "that player" -- always the player who took
    -- the action, since the action is the payment.
    player :: PlayerId.PlayerId,
    -- | The permanent whose static ability is ignored.
    source :: ObjectId.ObjectId,
    -- | CR 116.2d's "that ability", by the name its face gives it
    -- (PlayerStaticAbility.name). Every row on that face carrying this name is
    -- suppressed, which is one row for a card naming one ability and two for
    -- Damping Engine's one sentence.
    ability :: AbilityName.AbilityName,
    -- | CR 116.2d's "for a duration", in the stored form Pawl.Engine.Expiry
    -- sweeps. Every printed producer says "until end of turn", which arms as CR
    -- 514.2's Expiry.AtCleanup.
    expiry :: Expiry.Expiry
  }
  deriving (Eq, Ord, Show)
