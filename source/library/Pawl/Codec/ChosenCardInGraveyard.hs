{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenCardInGraveyard where

import qualified Pawl.Codec.Chooser as Chooser
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerScope as PlayerScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenCardInGraveyard as ChosenCardInGraveyard

-- | A bare object keyed by the record's field names, replacing the three-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ChosenCardInGraveyard.ChosenCardInGraveyard
codec = Fields.object $ do
  chooser <- Fields.required "chooser" Chooser.codec ChosenCardInGraveyard.chooser
  players <- Fields.required "players" PlayerScope.codec ChosenCardInGraveyard.players
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenCardInGraveyard.filter
  pure
    ChosenCardInGraveyard.MkChosenCardInGraveyard
      { ChosenCardInGraveyard.chooser = chooser,
        ChosenCardInGraveyard.players = players,
        ChosenCardInGraveyard.filter = filter_
      }
