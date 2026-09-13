{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Vote where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Vote as Vote

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Vote arm.
codec :: Codec.Codec Vote.Vote
codec = Fields.object $ do
  starter <- Fields.required "starter" PlayerRef.codec Vote.starter
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) Vote.filter
  slot <- Fields.required "slot" SlotName.codec Vote.slot
  pure
    Vote.MkVote
      { Vote.starter = starter,
        Vote.filter = filter_,
        Vote.slot = slot
      }
