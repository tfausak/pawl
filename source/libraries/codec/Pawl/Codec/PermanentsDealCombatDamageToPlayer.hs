{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentsDealCombatDamageToPlayer where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentsDealCombatDamageToPlayer as PermanentsDealCombatDamageToPlayer

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PlayerAttacksPlayer is. Both keys are required: a card that
-- qualifies only the damagers writes AnyPlayer for the recipient, which is what
-- "deal combat damage to a player" says.
codec :: Codec.Codec PermanentsDealCombatDamageToPlayer.PermanentsDealCombatDamageToPlayer
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentsDealCombatDamageToPlayer.filter
  recipient <- Fields.required "recipient" PlayerRelation.codec PermanentsDealCombatDamageToPlayer.recipient
  pure PermanentsDealCombatDamageToPlayer.MkPermanentsDealCombatDamageToPlayer {PermanentsDealCombatDamageToPlayer.filter = filter_, PermanentsDealCombatDamageToPlayer.recipient = recipient}
