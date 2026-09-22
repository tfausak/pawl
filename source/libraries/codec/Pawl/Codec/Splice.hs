{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Splice where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Cost as Cost
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Splice as Splice

-- | A bare object keyed by the record's field names, Pawl.Codec.Craft's shape.
-- The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Splice.Splice keyword)
codec keywordCodec = Fields.object $ do
  onto <- Fields.required "onto" (Filter.codec keywordCodec) Splice.onto
  cost <- Fields.required "cost" (Cost.codec keywordCodec) Splice.cost
  pure Splice.MkSplice {Splice.onto = onto, Splice.cost = cost}
