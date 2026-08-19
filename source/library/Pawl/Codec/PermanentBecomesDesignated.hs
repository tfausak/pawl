{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentBecomesDesignated where

import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentBecomesDesignated as PermanentBecomesDesignated

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be.
codec :: Codec.Codec PermanentBecomesDesignated.PermanentBecomesDesignated
codec = Fields.object $ do
  designation <- Fields.required "designation" Designation.codec PermanentBecomesDesignated.designation
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentBecomesDesignated.filter
  pure
    PermanentBecomesDesignated.MkPermanentBecomesDesignated
      { PermanentBecomesDesignated.designation = designation,
        PermanentBecomesDesignated.filter = filter_
      }
