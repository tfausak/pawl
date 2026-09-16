{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FromOutsideTheGame where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.OutsideDestination as OutsideDestination
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame

-- | A bare object keyed by the record's field names.

-- Every key is required, where Pawl.Codec.Search defaults its two flags. The
-- reason is which way an absent key would read: each of these three is a clause
-- of the printed sentence that every producer STATES, so a default would have
-- to pick the common spelling -- True for the reveal, the hand for the
-- destination -- and a card file that simply omitted the key would then print a
-- sentence its card does not (Death Wish reveals nothing; The Raven's Warning
-- names the library). Writing the key out in every card file that wishes is a
-- cheap price for that not being possible.
codec :: Codec.Codec FromOutsideTheGame.FromOutsideTheGame
codec = Fields.object $ do
  destination <- Fields.required "destination" OutsideDestination.codec FromOutsideTheGame.destination
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) FromOutsideTheGame.filter
  reveal <- Fields.required "reveal" Common.boolean FromOutsideTheGame.reveal
  pure
    FromOutsideTheGame.MkFromOutsideTheGame
      { FromOutsideTheGame.destination = destination,
        FromOutsideTheGame.filter = filter_,
        FromOutsideTheGame.reveal = reveal
      }
