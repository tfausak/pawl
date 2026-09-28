{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerDrawsNthCard where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerDrawsNthCard as PlayerDrawsNthCard

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. The ordinal is ELIDED when absent, so "a card"
-- writes no key.
codec :: Codec.Codec PlayerDrawsNthCard.PlayerDrawsNthCard
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PlayerDrawsNthCard.player
  nth <- Fields.defaulted "nth" Nothing (Common.maybe Common.natural) PlayerDrawsNthCard.nth
  pure
    PlayerDrawsNthCard.MkPlayerDrawsNthCard
      { PlayerDrawsNthCard.player = player,
        PlayerDrawsNthCard.nth = nth
      }
