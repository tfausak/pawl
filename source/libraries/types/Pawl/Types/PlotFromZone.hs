module Pawl.Types.PlotFromZone where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's PlotFrom arm: whose copy of which
-- zone CR 702.170f lets plot function in, and which cards in it are covered.
--
-- A library reference means its TOP CARD, Pawl.Engine.Cast.pileCandidates'
-- narrowing for Pawl.Types.CastFromZone one type over.
data PlotFromZone = MkPlotFromZone
  { from :: InZone.InZone,
    matching :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
