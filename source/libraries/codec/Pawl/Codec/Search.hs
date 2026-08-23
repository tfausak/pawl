{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Search where

import qualified Data.Set as Set
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Search as Search
import qualified Pawl.Types.Zone as Zone.Type

-- | A bare object keyed by the record's field names. Naming them is the point:
-- 'Search.searcher' and 'Search.owner' are both a PlayerRef, so a positional
-- payload let a card file swap who looks with whose zones are looked at.
codec :: Codec.Codec Search.Search
codec = Fields.object $ do
  searcher <- Fields.required "searcher" PlayerRef.codec Search.searcher
  owner <- Fields.required "owner" PlayerRef.codec Search.owner
  -- Defaulted rather than required, for upTo's reason: an absent key is a card
  -- that names no zone, which every card file already written means as a
  -- library.
  zones <- Fields.defaulted "zones" (Set.singleton Zone.Type.Library) (Common.set Zone.codec) Search.zones
  -- Required but nullable, rather than defaulted-absent: a null is a card
  -- printing "any number of", and an absent key is a card file that forgot the
  -- count. Defaulting would read the second as the first.
  quantity <- Fields.required "quantity" (Common.maybe Quantity.codec) Search.quantity
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) Search.filter
  -- Defaulted rather than required: an absent key is a card that does not print
  -- "up to", which is the ordinary case, and the reading every card file already
  -- written gets.
  upTo <- Fields.defaulted "upTo" False Common.boolean Search.upTo
  destination <- Fields.required "destination" SearchDestination.codec Search.destination
  pure
    Search.MkSearch
      { Search.searcher = searcher,
        Search.owner = owner,
        Search.zones = zones,
        Search.quantity = quantity,
        Search.filter = filter_,
        Search.upTo = upTo,
        Search.destination = destination
      }
