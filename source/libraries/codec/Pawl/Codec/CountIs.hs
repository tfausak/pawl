{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CountIs where

import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Label as Label
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CountIs as CountIs

codec :: Codec.Codec CountIs.CountIs
codec = Fields.object $ do
  player <- Fields.required "player" Label.codec CountIs.player
  zone <- Fields.required "zone" Arm.keyedEnum CountIs.zone
  card <- Fields.required "card" CardName.codec CountIs.card
  count <- Fields.required "count" Common.natural CountIs.count
  pure
    CountIs.MkCountIs
      { CountIs.player = player,
        CountIs.zone = zone,
        CountIs.card = card,
        CountIs.count = count
      }
