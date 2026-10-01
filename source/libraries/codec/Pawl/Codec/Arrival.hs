{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Arrival where

import qualified Pawl.Codec.ArrivalEnd as ArrivalEnd
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Arrival as Arrival

codec :: Codec.Codec Arrival.Arrival
codec = Fields.object $ do
  owner <- Fields.required "owner" PlayerId.codec Arrival.owner
  end <- Fields.required "end" ArrivalEnd.codec Arrival.end
  cards <- Fields.required "cards" (Common.seq ObjectId.codec) Arrival.cards
  pure
    Arrival.MkArrival
      { Arrival.owner = owner,
        Arrival.end = end,
        Arrival.cards = cards
      }
