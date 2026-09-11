{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AsCopy where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AsCopy as AsCopy

-- | A bare object keyed by the record's field names, Pawl.Codec.WithCounters'
-- shape. @exceptions@ is defaulted rather than required: CR 707.9's "except ..."
-- clause is absent from most printings, so a plain Clone writes the eligible
-- filter alone. @tapped@ is defaulted for the same reason: only a land that
-- enters tapped as a copy (Vesuva) writes it.
codec :: (Typeable.Typeable ability, Eq ability) => Codec.Codec ability -> Codec.Codec (AsCopy.AsCopy ability)
codec abilityCodec = Fields.object $ do
  eligible <- Fields.required "eligible" (Filter.codec Keyword.codec) AsCopy.eligible
  exceptions <- Fields.defaulted "exceptions" [] (Common.list (CopyException.codec abilityCodec)) AsCopy.exceptions
  tapped <- Fields.defaulted "tapped" False Common.boolean AsCopy.tapped
  pure AsCopy.MkAsCopy {AsCopy.eligible = eligible, AsCopy.exceptions = exceptions, AsCopy.tapped = tapped}
