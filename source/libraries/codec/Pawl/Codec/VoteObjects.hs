{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.VoteObjects where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.VoteObjects as VoteObjects

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.VoteChoices' Objects arm.
codec :: Codec.Codec VoteObjects.VoteObjects
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) VoteObjects.filter
  slot <- Fields.required "slot" SlotName.codec VoteObjects.slot
  pure
    VoteObjects.MkVoteObjects
      { VoteObjects.filter = filter_,
        VoteObjects.slot = slot
      }
