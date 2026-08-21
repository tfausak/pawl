{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackingPlayers where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackingPlayers as AttackingPlayers

-- | A bare object keyed by the record's field names. Card data, unlike its
-- neighbours under Pawl.Codec.GameEvent: Curse of Vitality writes one.
codec :: Codec.Codec AttackingPlayers.AttackingPlayers
codec = Fields.object $ do
  relation <- Fields.required "relation" PlayerRelation.codec AttackingPlayers.relation
  attacked <- Fields.required "attacked" SlotName.codec AttackingPlayers.attacked
  pure
    AttackingPlayers.MkAttackingPlayers
      { AttackingPlayers.relation = relation,
        AttackingPlayers.attacked = attacked
      }
