{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaAddition where

import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 109.5's "you", the recipient a card that names nobody means.
defaultPlayer :: PlayerRef.PlayerRef
defaultPlayer = PlayerRef.Relative PlayerRelation.You

-- | A bare object keyed by the record's field names, Pawl.Codec.PlayerCounters'
-- shape.
--
-- @player@ is DEFAULTED rather than required, and the default is the rule's own
-- reading: CR 106.3 has a mana effect instruct "a player" to add, and a card that
-- names nobody means CR 109.5's "you". Every printing in the pool but Shizuko,
-- Caller of Autumn leaves it out, so requiring it would make every card restate a
-- rule.
codec :: Codec.Codec ManaAddition.ManaAddition
codec = Fields.object $ do
  player <- Fields.defaulted "player" defaultPlayer PlayerRef.codec ManaAddition.player
  production <- Fields.required "production" ManaProduction.codec ManaAddition.production
  pure
    ManaAddition.MkManaAddition
      { ManaAddition.player = player,
        ManaAddition.production = production
      }
