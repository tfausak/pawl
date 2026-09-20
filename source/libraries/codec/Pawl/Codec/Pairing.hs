{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Pairing where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Pairing as Pairing

-- | A bare object keyed by the record's field names, Pawl.Codec.ExileHaunting's
-- shape: the two fields are different types, but naming them keeps a saved game
-- readable and matches every other record codec here.
codec :: Codec.Codec Pairing.Pairing
codec = Fields.object $ do
  partner <- Fields.required "partner" ObjectId.codec Pairing.partner
  under <- Fields.required "under" PlayerId.codec Pairing.under
  pure
    Pairing.MkPairing
      { Pairing.partner = partner,
        Pairing.under = under
      }
