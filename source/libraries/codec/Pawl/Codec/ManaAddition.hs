{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ManaAddition where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.Codec.ManaRetention as ManaRetention
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaAddition as ManaAddition
import qualified Pawl.Types.ManaRetention as ManaRetention
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
--
-- @retention@ is DEFAULTED the same way and for the same reason: CR 106.4's
-- default is that the player loses the mana as the step ends, so a card that
-- says nothing means Ordinary. Shizuko, Caller of Autumn's "until end of turn,
-- they don't lose this mana" is the one printing in the pool that writes it.
codec :: Codec.Codec ManaAddition.ManaAddition
codec = Fields.object $ do
  player <- Fields.defaulted "player" defaultPlayer PlayerRef.codec ManaAddition.player
  production <- Fields.required "production" ManaProduction.codec ManaAddition.production
  retention <- Fields.defaulted "retention" ManaRetention.Ordinary ManaRetention.codec ManaAddition.retention
  restriction <- Fields.defaulted "restriction" Nothing (Common.maybe (Filter.codec Keyword.codec)) ManaAddition.restriction
  pure
    ManaAddition.MkManaAddition
      { ManaAddition.player = player,
        ManaAddition.production = production,
        ManaAddition.retention = retention,
        ManaAddition.restriction = restriction
      }
