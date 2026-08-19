{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EachCardInGraveyard where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec EachCardInGraveyard.EachCardInGraveyard
codec = Fields.object $ do
  players <- Fields.required "players" PlayerScope.codec EachCardInGraveyard.players
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) EachCardInGraveyard.filter
  pure
    EachCardInGraveyard.MkEachCardInGraveyard
      { EachCardInGraveyard.players = players,
        EachCardInGraveyard.filter = filter_
      }
