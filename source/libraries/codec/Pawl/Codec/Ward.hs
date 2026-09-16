{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Ward where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Ward as Ward

-- | A bare object keyed by the record's field names, Pawl.Codec.Cycling's
-- shape. @perEach@ is defaulted rather than required, so a ward whose cost is a
-- printed number still writes nothing but its cost. The keyword codec is a
-- PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Ward.Ward keyword)
codec keywordCodec = Fields.object $ do
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Ward.cost
  perEach <- Fields.defaulted "perEach" Nothing (Common.maybe PlayerCounterTally.codec) Ward.perEach
  pure Ward.MkWard {Ward.cost = cost, Ward.perEach = perEach}
