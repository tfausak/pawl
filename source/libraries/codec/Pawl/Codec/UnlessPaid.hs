{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.UnlessPaid where

import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.UnlessPaid as UnlessPaid

-- | Both keys are required: an "unless" with no payer names nobody and one with
-- no cost offers nothing, so neither has a default an absent key could mean.
--
-- The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec UnlessPaid.UnlessPaid
codec = Fields.object $ do
  payer <- Fields.required "payer" SlotName.codec UnlessPaid.payer
  cost <- Fields.required "cost" (Cost.codec Keyword.codec) UnlessPaid.cost
  pure UnlessPaid.MkUnlessPaid {UnlessPaid.payer = payer, UnlessPaid.cost = cost}
