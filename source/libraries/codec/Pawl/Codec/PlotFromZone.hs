{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlotFromZone where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.InZone as InZone
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlotFromZone as PlotFromZone

-- | A bare object keyed by the record's field names, Pawl.Codec.CastFromZone's
-- shape; the zone reference is Pawl.Codec.InZone's, so its CR 400.1 invariant is
-- enforced here too.
codec :: Codec.Codec PlotFromZone.PlotFromZone
codec = Fields.object $ do
  from <- Fields.required "from" InZone.codec PlotFromZone.from
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) PlotFromZone.matching
  pure PlotFromZone.MkPlotFromZone {PlotFromZone.from = from, PlotFromZone.matching = matching}
