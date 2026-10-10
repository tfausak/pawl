module Pawl.Codec.EntryPrice where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryPrice as EntryPrice

-- | CR 614.1c's price for an untapped entry: life (CR 119.4) or a revealed
-- card (CR 701.20a).
codec :: Codec.Codec EntryPrice.EntryPrice
codec =
  Arm.tagged
    tagOf
    [ Arm.payload "PayLife" Common.natural EntryPrice.PayLife (\x -> case x of EntryPrice.PayLife y -> Just y; _ -> Nothing),
      Arm.payload "Reveal" (Filter.codec Keyword.codec) EntryPrice.Reveal (\x -> case x of EntryPrice.Reveal y -> Just y; _ -> Nothing)
    ]

tagOf :: EntryPrice.EntryPrice -> String
tagOf x = case x of
  EntryPrice.PayLife {} -> "PayLife"
  EntryPrice.Reveal {} -> "Reveal"
