{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenCardInHand where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenCardInHand as ChosenCardInHand

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
codec :: Codec.Codec ChosenCardInHand.ChosenCardInHand
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec ChosenCardInHand.player
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenCardInHand.filter
  pure
    ChosenCardInHand.MkChosenCardInHand
      { ChosenCardInHand.player = player,
        ChosenCardInHand.filter = filter_
      }
