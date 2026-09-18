module Pawl.Types.ChosenPermanent where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 608.2d's singular battlefield choice: what may be picked, and WHO picks.

-- The Filter is read from the ABILITY's perspective whoever chooses -- CR 109.5's
-- "you" is the ability's controller, and Wormfang Crab's "a permanent you control"
-- keeps meaning the Crab's controller when an opponent is the one naming it. So
-- the two fields answer different questions and are named rather than positional,
-- Pawl.Types.ChosenCardInGraveyard's reason.
--
-- The CHOOSER is a PlayerRef naming ONE seat, Pawl.Types.ChosenCardFromAmong's
-- reading and not Pawl.Types.Chooser's: that type folds over a PlayerScope of
-- GRAVEYARDS, one per chooser, where the battlefield is one shared zone (CR
-- 400.1) however many seats are in play. CR 608.2c's default is the resolving
-- controller, which is what @Relative You@ spells; Wormfang Crab's "an opponent
-- chooses a permanent you control" is the other seat, named through the slot an
-- Pawl.Types.Effect ChoosePlayer filled earlier in the same resolution. A slot a
-- TRIGGER filled is the other road, and the one that outlives the resolution:
-- Pawl.Engine.Binding.triggerPlayer holds a seat named a priority window ago, so
-- CR 800.4g's departed chooser is reachable from here
-- (Pawl.Engine.Resolve.Effect.askedChooser).
--
-- A ref naming NO seat -- an unfilled slot, or one holding something that is not
-- a player -- names no permanent either, and that share of the instruction is
-- ignored (CR 101.3), Pawl.Engine.Resolve.Effect.chooseCardFromAmong's reading.
data ChosenPermanent = MkChosenPermanent
  { filter :: Filter.Filter Keyword.Keyword,
    chooser :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
