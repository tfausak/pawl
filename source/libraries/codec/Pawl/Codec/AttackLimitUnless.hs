{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttackLimitUnless where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttackLimitUnless as AttackLimitUnless

-- | Pawl.Codec.LimitUnless's shape with one more key: "limit" and not
-- "affected", because a bound names no creature, and the key set is what tells a
-- reader of the card file which shape it is looking at without consulting the
-- tag.
--
-- "defenders" defaults to absent, which is CR 802.3a's WHOLE declaration: Silent
-- Arbiter and Caverns of Despair write no such key, and the one printing that
-- does is Crawlspace. Spelled as Pawl.Codec.CantAttackPlayer spells it, the two
-- naming the same side of a combat sentence.
codec :: Codec.Codec AttackLimitUnless.AttackLimitUnless
codec = Fields.object $ do
  limit <- Fields.required "limit" Common.natural AttackLimitUnless.limit
  defenders <- Fields.defaulted "defenders" Nothing (Common.maybe PlayerScope.codec) AttackLimitUnless.defenders
  unless <- Fields.defaulted "unless" Nothing (Common.maybe Condition.codec) AttackLimitUnless.unless
  pure
    AttackLimitUnless.MkAttackLimitUnless
      { AttackLimitUnless.limit = limit,
        AttackLimitUnless.defenders = defenders,
        AttackLimitUnless.unless = unless
      }
