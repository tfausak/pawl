{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AgainstLastCardExiledWith where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AgainstLastCardExiledWith as AgainstLastCardExiledWith

-- | A bare object keyed by the record's field names. The quantity codec is a
-- PARAMETER for Pawl.Codec.AgainstSlot's reason.
codec :: (Typeable.Typeable quantity) => Codec.Codec quantity -> Codec.Codec (AgainstLastCardExiledWith.AgainstLastCardExiledWith quantity)
codec quantityCodec = Fields.object $ do
  criterion <- Fields.required "filter" (Filter.codec Keyword.codec) AgainstLastCardExiledWith.filter
  quantity <- Fields.required "quantity" quantityCodec AgainstLastCardExiledWith.quantity
  pure AgainstLastCardExiledWith.MkAgainstLastCardExiledWith {AgainstLastCardExiledWith.filter = criterion, AgainstLastCardExiledWith.quantity = quantity}
