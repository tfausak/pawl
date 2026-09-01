{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantAttackPlayer where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer

-- | Pawl.Codec.CantBeBlockedBy's shape with the second key naming PLAYERS
-- instead of blockers: three named keys, the last of them CR 508.1c's "unless"
-- gate.
--
-- "defenders" and not a second "affected", for that codec's reason: the two keys
-- name opposite sides of one sentence, and naming them is what stops a card file
-- barring the restricted creatures from themselves.
codec :: Codec.Codec CantAttackPlayer.CantAttackPlayer
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CantAttackPlayer.affected
  defenders <- Fields.required "defenders" PlayerScope.codec CantAttackPlayer.defenders
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) CantAttackPlayer.unless
  pure
    CantAttackPlayer.MkCantAttackPlayer
      { CantAttackPlayer.affected = affected,
        CantAttackPlayer.defenders = defenders,
        CantAttackPlayer.unless = unless
      }
