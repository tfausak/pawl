{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Vote where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.VoteChoices as VoteChoices
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Vote as Vote

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Vote arm.
codec :: Codec.Codec Vote.Vote
codec = Fields.object $ do
  starter <- Fields.required "starter" PlayerRef.codec Vote.starter
  choices <- Fields.required "choices" VoteChoices.codec Vote.choices
  pure
    Vote.MkVote
      { Vote.starter = starter,
        Vote.choices = choices
      }
