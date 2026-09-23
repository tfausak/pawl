{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerAttacksWith where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerAttacksWith as PlayerAttacksWith

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.CreatureBecomesBlockedByAtLeast is. All three keys are required:
-- no part of the printed sentence has a default, an unfiltered "whenever you
-- attack" being Pawl.Types.TriggerCondition's PlayerAttacks instead, and a
-- defaulted floor would let a card mean "one or more" by saying nothing.
--
-- The floor of one is 'Fields.objectWith''s check, Pawl.Codec.EntersWith's
-- posture: a zero floor would fire on any declaration whatever the filter.
codec :: Codec.Codec PlayerAttacksWith.PlayerAttacksWith
codec =
  let check p =
        if PlayerAttacksWith.attackers p == 0
          then Left (Text.pack "attack trigger has a zero attackers floor")
          else Right p
   in Fields.objectWith check $ do
        player <- Fields.required "player" PlayerRelation.codec PlayerAttacksWith.player
        filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PlayerAttacksWith.filter
        attackers <- Fields.required "attackers" Common.natural PlayerAttacksWith.attackers
        pure
          PlayerAttacksWith.MkPlayerAttacksWith
            { PlayerAttacksWith.player = player,
              PlayerAttacksWith.filter = filter_,
              PlayerAttacksWith.attackers = attackers
            }
