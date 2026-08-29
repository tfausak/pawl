{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.FromOutsideTheGame where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.FromOutsideTheGame as FromOutsideTheGame

-- | A bare object keyed by the record's field names.

-- Both keys are required, where Pawl.Codec.Search defaults its two flags. The
-- reason is which way an absent key would read: the corpus's existing producers
-- all reveal, so a default would have to be True, and a card file that simply
-- omitted the key would then reveal a card its printed text does not
-- (Death Wish). Four card files is a cheap price for that not being possible.
codec :: Codec.Codec FromOutsideTheGame.FromOutsideTheGame
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) FromOutsideTheGame.filter
  reveal <- Fields.required "reveal" Common.boolean FromOutsideTheGame.reveal
  pure
    FromOutsideTheGame.MkFromOutsideTheGame
      { FromOutsideTheGame.filter = filter_,
        FromOutsideTheGame.reveal = reveal
      }
