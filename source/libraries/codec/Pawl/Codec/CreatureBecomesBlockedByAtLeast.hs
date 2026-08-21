{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CreatureBecomesBlockedByAtLeast where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CreatureBecomesBlockedByAtLeast as CreatureBecomesBlockedByAtLeast

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PlayerDrawsNthCard is.
codec :: Codec.Codec CreatureBecomesBlockedByAtLeast.CreatureBecomesBlockedByAtLeast
codec = Fields.object $ do
  attacked <- Fields.required "attacked" PlayerRelation.codec CreatureBecomesBlockedByAtLeast.attacked
  blockers <- Fields.required "blockers" Common.natural CreatureBecomesBlockedByAtLeast.blockers
  pure
    CreatureBecomesBlockedByAtLeast.MkCreatureBecomesBlockedByAtLeast
      { CreatureBecomesBlockedByAtLeast.attacked = attacked,
        CreatureBecomesBlockedByAtLeast.blockers = blockers
      }
