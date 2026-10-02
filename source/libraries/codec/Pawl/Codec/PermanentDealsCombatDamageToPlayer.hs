{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentDealsCombatDamageToPlayer where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentDealsCombatDamageToPlayer as PermanentDealsCombatDamageToPlayer

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentsDealCombatDamageToPlayer is. Both keys are required: a
-- card that qualifies only the damager writes AnyPlayer for the recipient.
codec :: Codec.Codec PermanentDealsCombatDamageToPlayer.PermanentDealsCombatDamageToPlayer
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentDealsCombatDamageToPlayer.filter
  recipient <- Fields.required "recipient" PlayerRelation.codec PermanentDealsCombatDamageToPlayer.recipient
  pure PermanentDealsCombatDamageToPlayer.MkPermanentDealsCombatDamageToPlayer {PermanentDealsCombatDamageToPlayer.filter = filter_, PermanentDealsCombatDamageToPlayer.recipient = recipient}
