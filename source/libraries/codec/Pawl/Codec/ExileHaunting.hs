{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExileHaunting where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExileHaunting as ExileHaunting

-- | A bare object keyed by the record's field names. Naming them is the point:
-- both are a SlotName, so a positional payload would let a card file swap the
-- haunting card and its host and still decode.
codec :: Codec.Codec ExileHaunting.ExileHaunting
codec = Fields.object $ do
  card <- Fields.required "card" SlotName.codec ExileHaunting.card
  host <- Fields.required "host" SlotName.codec ExileHaunting.host
  pure
    ExileHaunting.MkExileHaunting
      { ExileHaunting.card = card,
        ExileHaunting.host = host
      }
