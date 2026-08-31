{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerAttacksPlayer where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerAttacksPlayer as PlayerAttacksPlayer

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PlayerAttacksWith is. Both keys are required: rule 508.3e names
-- both subjects, and neither has a default -- a card that qualifies only the
-- declaring side writes AnyPlayer on the other, which is what the twenty-four
-- printings of the bare "attacks a player" say.
codec :: Codec.Codec PlayerAttacksPlayer.PlayerAttacksPlayer
codec = Fields.object $ do
  attacker <- Fields.required "attacker" PlayerRelation.codec PlayerAttacksPlayer.attacker
  attacked <- Fields.required "attacked" PlayerRelation.codec PlayerAttacksPlayer.attacked
  pure PlayerAttacksPlayer.MkPlayerAttacksPlayer {PlayerAttacksPlayer.attacker = attacker, PlayerAttacksPlayer.attacked = attacked}
