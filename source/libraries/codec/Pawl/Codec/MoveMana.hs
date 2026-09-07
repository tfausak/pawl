{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MoveMana where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MoveMana as MoveMana

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's MoveMana arm.
--
-- Both sides are required and neither defaults: CR 106.13 has a card name the
-- loser and the gainer alike, and defaulting either to CR 109.5's "you" would
-- make the empty object mean a transfer onto itself.
codec :: Codec.Codec MoveMana.MoveMana
codec = Fields.object $ do
  from <- Fields.required "from" PlayerRef.codec MoveMana.from
  to <- Fields.required "to" PlayerRef.codec MoveMana.to
  pure
    MoveMana.MkMoveMana
      { MoveMana.from = from,
        MoveMana.to = to
      }
