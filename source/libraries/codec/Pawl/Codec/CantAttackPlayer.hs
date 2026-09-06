{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantAttackPlayer where

import qualified Pawl.Codec.AbilityName as AbilityName
import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.AttackTargetKind as AttackTargetKind
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantAttackPlayer as CantAttackPlayer

-- | Pawl.Codec.CantBeBlockedBy's shape with the second key naming PLAYERS
-- instead of blockers: four named keys, the last of them CR 508.1c's "unless"
-- gate.
--
-- "kinds" is required rather than defaulted to OfPlayer alone: CR 506.3's list
-- has three entries and which of them a printing names is the sentence itself,
-- so a card file states it.
--
-- "defenders" and not a second "affected", for that codec's reason: the two keys
-- name opposite sides of one sentence, and naming them is what stops a card file
-- barring the restricted creatures from themselves.
--
-- "name" defaults to Nothing, Pawl.Codec.AffectedUnless's reason.
codec :: Codec.Codec CantAttackPlayer.CantAttackPlayer
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec CantAttackPlayer.affected
  defenders <- Fields.required "defenders" PlayerScope.codec CantAttackPlayer.defenders
  kinds <- Fields.required "kinds" (Common.set AttackTargetKind.codec) CantAttackPlayer.kinds
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) CantAttackPlayer.unless
  name <- Fields.defaulted "name" Nothing (Common.maybe AbilityName.codec) CantAttackPlayer.name
  pure
    CantAttackPlayer.MkCantAttackPlayer
      { CantAttackPlayer.affected = affected,
        CantAttackPlayer.defenders = defenders,
        CantAttackPlayer.kinds = kinds,
        CantAttackPlayer.unless = unless,
        CantAttackPlayer.name = name
      }
