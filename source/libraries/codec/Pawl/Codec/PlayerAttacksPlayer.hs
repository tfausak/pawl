{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerAttacksPlayer where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer

-- | CR 508.3e's two subjects, keyed by which side of the attack each names --
-- Pawl.Codec.PlayerAttacksWith's shape, with rule 508.3c's Filter replaced by a
-- second relation.
codec :: Codec.Codec PlayerAttacksPlayer.PlayerAttacksPlayer
codec = Fields.object $ do
  attacker <- Fields.required "attacker" PlayerRelation.codec PlayerAttacksPlayer.attacker
  attacked <- Fields.required "attacked" PlayerRelation.codec PlayerAttacksPlayer.attacked
  pure
    PlayerAttacksPlayer.MkPlayerAttacksPlayer
      { PlayerAttacksPlayer.attacker = attacker,
        PlayerAttacksPlayer.attacked = attacked
      }
