{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AsCopy where

import qualified Data.Sequence as Seq
import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CopyException as CopyException
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.WithCounters as WithCounters
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AsCopy as AsCopy
import qualified Pawl.Types.Zone as Zone.Type

-- | A bare object keyed by the record's field names, Pawl.Codec.WithCounters'
-- shape. @exceptions@ is defaulted rather than required: CR 707.9's "except ..."
-- clause is absent from most printings, so a plain Clone writes the eligible
-- filter alone. @tapped@ is defaulted for the same reason: only a land that
-- enters tapped as a copy (Vesuva) writes it. So is @counters@, CR 707.9e's
-- additional-effect exception (Altered Ego), which a copy effect stating no
-- such clause leaves out, and so is @whenYouDo@, CR 707.9g's linked trigger
-- (Wall of Stolen Identity). @zone@ is defaulted to the battlefield, which is
-- where every printing but Superior Spider-Man's looks.
codec :: (Typeable.Typeable ability, Eq ability, Typeable.Typeable effect, Eq effect) => Codec.Codec ability -> Codec.Codec effect -> Codec.Codec (AsCopy.AsCopy ability effect)
codec abilityCodec effectCodec = Fields.object $ do
  eligible <- Fields.required "eligible" (Filter.codec Keyword.codec) AsCopy.eligible
  exceptions <- Fields.defaulted "exceptions" [] (Common.list (CopyException.codec abilityCodec)) AsCopy.exceptions
  tapped <- Fields.defaulted "tapped" False Common.boolean AsCopy.tapped
  counters <- Fields.defaulted "counters" Nothing (Common.maybe WithCounters.codec) AsCopy.counters
  whenYouDo <- Fields.defaulted "whenYouDo" Seq.empty (Common.seq effectCodec) AsCopy.whenYouDo
  zone <- Fields.defaulted "zone" Zone.Type.Battlefield Zone.codec AsCopy.zone
  pure AsCopy.MkAsCopy {AsCopy.eligible = eligible, AsCopy.exceptions = exceptions, AsCopy.tapped = tapped, AsCopy.counters = counters, AsCopy.whenYouDo = whenYouDo, AsCopy.zone = zone}
