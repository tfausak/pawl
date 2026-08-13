{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamagePrevented where

import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamagePrevented as DamagePrevented

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec DamagePrevented.DamagePrevented
codec = Fields.object $ do
  recipient <- Fields.required "recipient" Recipient.codec DamagePrevented.recipient
  amount <- Fields.required "amount" Common.natural DamagePrevented.amount
  pure
    DamagePrevented.MkDamagePrevented
      { DamagePrevented.recipient = recipient,
        DamagePrevented.amount = amount
      }
